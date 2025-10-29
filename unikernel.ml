open Lwt.Infix

module K = struct
  open Cmdliner

  let host =
    let doc = Arg.info ~doc:"The host to trace." ["host"] in
    Mirage_runtime.register_arg
      Arg.(value & (opt Mirage_runtime_network.Arg.ipv4_address
                      (Ipaddr.V4.of_string_exn "141.1.1.1") doc))

  let timeout =
    let doc = Arg.info ~doc:"Timeout (in millisecond)." ["timeout"] in
    Mirage_runtime.register_arg Arg.(value & (opt int 1000 doc))

  let ipv4 =
    let doc = Arg.info ~doc:"IPv4 address. If not provided DHCP will be performed instead." ["ipv4"] in
    Mirage_runtime.register_arg
      Arg.(value & (opt (some Mirage_runtime_network.Arg.ipv4) None doc))

  let ipv4_gateway =
    let doc = Arg.info ~doc:"IPv4 gateway. Only used when --ipv4 is provided." ["ipv4-gateway"] in
    Mirage_runtime.register_arg
      Arg.(value & (opt (some Mirage_runtime_network.Arg.ipv4_address) None doc))

  let hostname =
    let parser str = Domain_name.of_string str in
    let pp = Domain_name.pp in
    let domain_name = Arg.conv (parser, pp) in
    let doc = Arg.info ~doc:"Hostname of the unikernel." ["hostname"] in
    Mirage_runtime.register_arg
      Arg.(value & (opt domain_name (Domain_name.of_string_exn "traceroute.robur.coop") doc))
end

(* takes a time-to-live (int) and timestamp (int64, nanoseconda), encodes them
   into 16 bit source port and 16 bit destination port:
   - the timestamp precision is 100ns (thus, it is divided by 100)
   - use the bits 27-11 of the timestamp as source port
   - use the bits 11-0 as destination port, and 5 bits of the ttl
*)
let ports_of_ttl_ts ttl ts =
  let ts = Int64.div ts 100L in
  let src_port = 0xffff land (Int64.(to_int (shift_right ts 11)))
  and dst_port = 0xffe0 land (Int64.(to_int (shift_left ts 5))) lor (0x001f land ttl)
  in
  src_port, dst_port

(* inverse operation of ports_of_ttl_ts for the range (src_port and dst_port
   are 16 bit values) *)
let ttl_ts_of_ports src_port dst_port =
  let ttl = 0x001f land dst_port in
  let ts =
    let low = Int64.of_int (dst_port lsr 5)
    and high = Int64.(shift_left (of_int src_port) 11)
    in
    Int64.add low high
  in
  let ts = Int64.mul ts 100L in
  ttl, ts

(* write a log line of a hop: the number, IP address, and round trip time *)
let log_one now ttl sent ip =
  let now = Int64.(mul (logand (div now 100L) 0x7FFFFFFL) 100L) in
  let duration = Mtime.Span.of_uint64_ns (Int64.sub now sent) in
  Logs.info (fun m -> m "%2d  %a  %a" ttl Ipaddr.V4.pp ip Mtime.Span.pp duration)

module Icmp = struct
  type t = {
    send : int -> unit Lwt.t ;
    log : int -> int64 -> Ipaddr.V4.t -> unit ;
    task_done : unit Lwt.u ;
  }

  type error = unit

  type ipaddr = Ipaddr.V4.t

  let pp_error ppf () = assert false

  let connect send log task_done =
    let t = { send ; log ; task_done } in
    Lwt.return t

  let disconnect t = assert false

  let write t ?src ~dst ?ttl _ = assert false

  (* This is called for each received ICMP packet. *)
  let input t ~src ~dst buf =
    let host = K.host () in
    let open Icmpv4_packet in
    (* Decode the received buffer (the IP header has been cut off already). *)
    match Unmarshal.of_cstruct buf with
    | Error s ->
      Lwt.fail_with (Fmt.str "ICMP: error parsing message from %a: %s" Ipaddr.V4.pp src s)
    | Ok (message, payload) ->
      let open Icmpv4_wire in
      (* There are two interesting cases: Time exceeded (-> send next packet),
         and Destination (port) unreachable (-> we reached the final host and can exit) *)
      match message.ty with
      | Time_exceeded ->
        (* Decode the payload, which should be an IPv4 header and a protocol header *)
        begin match Ipv4_packet.Unmarshal.header_of_cstruct payload with
          | Ok (pkt, off) when
              (* Ensure this packet matches our sent packet: the protocol is UDP
                 and the destination address is the host we're tracing *)
              pkt.Ipv4_packet.proto = Ipv4_packet.Marshal.protocol_to_int `UDP &&
              Ipaddr.V4.compare pkt.Ipv4_packet.dst host = 0 ->
            let src_port = Cstruct.BE.get_uint16 payload off
            and dst_port = Cstruct.BE.get_uint16 payload (off + 2)
            in
            (* Retrieve ttl and sent timestamp, encoded in the source port and
               destination port of the UDP packet we sent, and received back as
               ICMP payload. *)
            let ttl, sent = ttl_ts_of_ports src_port dst_port in
            (* Log this hop. *)
            t.log ttl sent src;
            (* Sent out the next UDP packet with an increased ttl. *)
            let ttl' = succ ttl in
            t.send ttl'
          | Ok (pkt, _) ->
            (* Some stray ICMP packet. *)
            Logs.debug (fun m -> m "unsolicited time exceeded from %a to %a (proto %X dst %a)"
                           Ipaddr.V4.pp src Ipaddr.V4.pp dst pkt.Ipv4_packet.proto Ipaddr.V4.pp pkt.Ipv4_packet.dst);
            Lwt.return_unit
          | Error e ->
            (* Decoding error. *)
            Logs.warn (fun m -> m "couldn't parse ICMP time exceeded payload (IPv4) (%a -> %a) %s"
                          Ipaddr.V4.pp src Ipaddr.V4.pp dst e);
            Lwt.return_unit
        end
      | Destination_unreachable when Ipaddr.V4.compare src host = 0 ->
        (* We reached the final host, and the destination port was not listened to *)
        begin match Ipv4_packet.Unmarshal.header_of_cstruct payload with
          | Ok (_, off) ->
            let src_port = Cstruct.BE.get_uint16 payload off
            and dst_port = Cstruct.BE.get_uint16 payload (off + 2)
            in
            (* Retrieve ttl and sent timestamp. *)
            let ttl, sent = ttl_ts_of_ports src_port dst_port in
            (* Log the final hop. *)
            t.log ttl sent src;
            (* Wakeup the waiter task to exit the unikernel. *)
            Lwt.wakeup t.task_done ();
            Lwt.return_unit
          | Error e ->
            (* Decoding error. *)
            Logs.warn (fun m -> m "couldn't parse ICMP unreachable payload (IPv4) (%a -> %a) %s"
                          Ipaddr.V4.pp src Ipaddr.V4.pp dst e);
            Lwt.return_unit
        end
      | ty ->
        Logs.debug (fun m -> m "ICMP unknown ty %s from %a to %a: %a"
                       (ty_to_string ty) Ipaddr.V4.pp src Ipaddr.V4.pp dst
                       Cstruct.hexdump_pp payload);
        Lwt.return_unit
end

module MakeV4
    (Netif    : Mirage_net.S)
    (Eth      : Ethernet.S)
    (Arpv4    : Arp.S)
    (Ip       : Tcpip.Ip.S with type ipaddr = Ipaddr.V4.t)
    (Icmpv4   : Icmpv4.S)
    (Udp      : Tcpip.Udp.S with type ipaddr = Ipaddr.V4.t)
    (Tcp      : Tcpip.Tcp.S with type ipaddr = Ipaddr.V4.t) = struct

  module UDP = Udp
  module TCP = Tcp
  module IP = Ip

  type t = {
    netif : Netif.t;
    ethif : Eth.t;
    arpv4 : Arpv4.t;
    icmpv4 : Icmpv4.t;
    ip : IP.t;
    udp : Udp.t;
    tcp : Tcp.t;
    mutable task : unit Lwt.t option;
  }

  let pp fmt t =
    Format.fprintf fmt "mac=%a,ip=%a" Macaddr.pp (Eth.mac t.ethif)
      Fmt.(list ~sep:(any ", ") IP.pp_prefix) (IP.configured_ips t.ip)

  let tcp { tcp; _ } = tcp
  let udp { udp; _ } = udp
  let ip { ip; _ } = ip

  let listen t =
    Lwt.catch (fun () ->
        Logs.debug (fun f -> f "Establishing or updating listener for stack %a" pp t);
        let tcp = Tcp.input t.tcp
        and udp = Udp.input t.udp
        and default ~proto ~src ~dst buf =
          match proto, src, dst with
          | 1, src, dst -> Icmpv4.input t.icmpv4 ~src ~dst buf
          | _ -> Lwt.return_unit
        in
        let ethif_listener = Eth.input
            ~arpv4:(Arpv4.input t.arpv4)
            ~ipv4:(IP.input ~tcp ~udp ~default t.ip)
            ~ipv6:(fun _ -> Lwt.return_unit)
            t.ethif
        in
        Netif.listen t.netif ~header_size:Ethernet.Packet.sizeof_ethernet ethif_listener
        >>= function
        | Error e ->
          Logs.warn (fun p -> p "%a" Netif.pp_error e) ;
          (* XXX: error should be passed to the caller *)
          Lwt.return_unit
        | Ok _res ->
          let nstat = Netif.get_stats_counters t.netif in
          let open Mirage_net in
          Logs.info (fun f ->
              f "listening loop of interface %s terminated regularly:@ %Lu bytes \
                 (%lu packets) received, %Lu bytes (%lu packets) sent@ "
                (Macaddr.to_string (Netif.mac t.netif))
                nstat.rx_bytes nstat.rx_pkts
                nstat.tx_bytes nstat.tx_pkts) ;
          Lwt.return_unit)
      (function
        | Lwt.Canceled ->
          Logs.info (fun f -> f "listen of %a cancelled" pp t);
          Lwt.return_unit
        | e -> Lwt.fail e)

  let connect netif ethif arpv4 ip icmpv4 udp tcp =
    let t = { netif; ethif; arpv4; ip; icmpv4; tcp; udp; task = None } in
    Logs.info (fun f -> f "MONO TCP/IP stack assembled: %a" pp t);
    Lwt.async (fun () -> let task = listen t in t.task <- Some task; task);
    Lwt.return t

  let disconnect t =
    Logs.info (fun f -> f "MONO TCP/IP stack disconnected: %a" pp t);
    (match t.task with None -> () | Some task -> Lwt.cancel task);
    Lwt.return_unit
end
module Main (N : Mirage_net.S) = struct
  module DHCP = Dhcp_ipv4.Make(N)
  module UDP = Udp.Make(DHCP.IPv4)
  module TCP = Tcp.Flow.Make(DHCP.IPv4)
  module Stack = MakeV4(DHCP.Net)(DHCP.Ethernet)(DHCP.Arp)(DHCP.IPv4)(Icmp)(UDP)(TCP)

  (* Global mutable state: the timeout task for a sent packet. *)
  let to_cancel = ref None

  (* Send a single packet with the given time to live. *)
  let rec send_udp timeout host udp ttl =
    (* This is called by the ICMP handler which successfully received a
       time exceeded, thus we cancel the timeout task. *)
    (match !to_cancel with
     | None -> ()
     | Some t -> Lwt.cancel t ; to_cancel := None);
    (* Our hop limit is 31 - 5 bit - should be sufficient for most networks. *)
    if ttl > 31 then
      Lwt.return_unit
    else
      (* Create a timeout task which:
         - sleeps for --timeout interval
         - logs an unknown hop
         - sends another packet with increased ttl
      *)
      let cancel =
        Lwt.catch (fun () ->
            Mirage_sleep.ns (Duration.of_ms timeout) >>= fun () ->
            Logs.info (fun m -> m "%2d  *" ttl);
            send_udp timeout host udp (succ ttl))
          (function Lwt.Canceled -> Lwt.return_unit | exc -> Lwt.fail exc)
      in
      (* Assign this timeout task. *)
      to_cancel := Some cancel;
      (* Figure out which source and destination port to use, based on ttl
         and current timestamp. *)
      let src_port, dst_port = ports_of_ttl_ts ttl (Mirage_mtime.elapsed_ns ()) in
      (* Send packet via UDP. *)
      UDP.write ~ttl ~src_port ~dst:host ~dst_port udp Cstruct.empty >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Fmt.str "while sending udp frame %a" UDP.pp_error e)

  let handle_lease lease =
    lease >|= function
    | None ->
      Logs.info (fun m -> m "We are not using DHCP, not configuring any log servers")
    | Some lease ->
      Logs.info (fun m -> m "Got log servers %a (but not doing anything with them yet!)"
                    Fmt.(list ~sep:(any ",") Ipaddr.V4.pp)
                    (Dhcp_wire.collect_log_servers lease))

  (* The main unikernel entry point. *)
  let start net =
    let log_one = fun port ip -> log_one (Mirage_mtime.elapsed_ns ()) port ip
    (* Create a task to wait on and a waiter to wakeup. *)
    and t, w = Lwt.task ()
    in
    (* Setup network stack: ethernet, ARP, IPv4, UDP, and ICMP. *)
    let options = [
      Dhcp_wire.Hostname (Mirage_runtime.name ());
      Dhcp_wire.Client_fqdn ([ `Server_A ], K.hostname ())
    ] in
    let requests = Dhcp_wire.[ SUBNET_MASK; ROUTERS; LOG_SERVERS; ] in
    let lease, wakeup_lease = Lwt.wait () in
    DHCP.connect ~registry:wakeup_lease ?cidr:(K.ipv4 ()) ?gateway:(K.ipv4_gateway ()) ~options ~requests net >>= fun (net, eth, arp, ip) ->
    (* Syslog.connect ~log_servers_from_dhcp:(lease >|= Option.map Dhcp_wire.collect_log_servers) ... *)
    Lwt.async (fun () -> handle_lease lease);
    UDP.connect ip >>= fun udp ->
    let send = send_udp (K.timeout ()) (K.host ()) udp in
    Icmp.connect send log_one w >>= fun icmp ->
    TCP.connect ip >>= fun tcp ->
    Stack.connect net eth arp ip icmp udp tcp >>= fun stack ->
    (* Send the initial UDP packet with a ttl of 1. This entails the domino
       effect to receive ICMP packets, send out another UDP packet with ttl
       increased by one, etc. - until a destination unreachable is received,
       or the hop limit is reached. *)
    send 1 >>= fun () ->
    t
end
