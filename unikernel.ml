open Lwt.Infix

module K = struct
  open Cmdliner

  let host =
    let doc = Arg.info ~doc:"The host to trace." ["host"] in
    Arg.(value & (opt Mirage_runtime_network.Arg.ipv4_address
                    (Ipaddr.V4.of_string_exn "141.1.1.1") doc))

  let timeout =
    let doc = Arg.info ~doc:"Timeout (in millisecond)." ["timeout"] in
    Arg.(value & (opt int 1000 doc))

  let ipv4 =
    let doc = Arg.info ~doc:"IPv4 address. If not provided DHCP will be performed instead." ["ipv4"] in
    Arg.(value & (opt (some Mirage_runtime_network.Arg.ipv4) None doc))

  let ipv4_gateway =
    let doc = Arg.info ~doc:"IPv4 gateway. Only used when --ipv4 is provided." ["ipv4-gateway"] in
    Arg.(value & (opt (some Mirage_runtime_network.Arg.ipv4_address) None doc))

  let hostname =
    let parser str = Domain_name.of_string str in
    let pp = Domain_name.pp in
    let domain_name = Arg.conv (parser, pp) in
    let doc = Arg.info ~doc:"Hostname of the unikernel." ["hostname"] in
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
let log_one ttl sent ip =
  let now = Mirage_mtime.elapsed_ns () in
  let now = Int64.(mul (logand (div now 100L) 0x7FFFFFFL) 100L) in
  let duration = Mtime.Span.of_uint64_ns (Int64.sub now sent) in
  Logs.info (fun m -> m "%2d  %a  %a" ttl Ipaddr.V4.pp ip Mtime.Span.pp duration)

module Icmp = struct
  type t = {
    mutable send : int -> unit Lwt.t ;
    task_done : unit Lwt.u ;
    task : unit Lwt.t ;
    host : Ipaddr.V4.t;
  }

  let connect host =
    let task, task_done = Lwt.wait () in
    let send (_ : int) = Lwt.return_unit in
    let t = { send ; task_done ; task; host } in
    Lwt.return t

  let set_send t send = t.send <- send

  let task { task; _ } = task

  (* This is called for each received ICMP packet. *)
  let input t ~src ~dst buf =
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
              Ipaddr.V4.compare pkt.Ipv4_packet.dst t.host = 0 ->
            let src_port = Cstruct.BE.get_uint16 payload off
            and dst_port = Cstruct.BE.get_uint16 payload (off + 2)
            in
            (* Retrieve ttl and sent timestamp, encoded in the source port and
               destination port of the UDP packet we sent, and received back as
               ICMP payload. *)
            let ttl, sent = ttl_ts_of_ports src_port dst_port in
            (* Log this hop. *)
            log_one ttl sent src;
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
      | Destination_unreachable when Ipaddr.V4.compare src t.host = 0 ->
        (* We reached the final host, and the destination port was not listened to *)
        begin match Ipv4_packet.Unmarshal.header_of_cstruct payload with
          | Ok (_, off) ->
            let src_port = Cstruct.BE.get_uint16 payload off
            and dst_port = Cstruct.BE.get_uint16 payload (off + 2)
            in
            (* Retrieve ttl and sent timestamp. *)
            let ttl, sent = ttl_ts_of_ports src_port dst_port in
            (* Log the final hop. *)
            log_one ttl sent src;
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

  (* Dummy definitions to satisfy Icmpv4.S *)
  type ipaddr = Ipaddr.V4.t
  type error = |
  let pp_error _ppf : error -> _ = function
    | _ -> .

  let write _t ?src:_ ~dst:_ ?ttl:_ _buf =
    Logs.warn (fun m -> m "Attempt at writing ICMP packet ignored");
    Lwt.return (Ok ())

  let disconnect _t = assert false
end

module Main (Icmp : module type of Icmp) (DHCP : Tcpip.Stack.V4V6) (_ : sig end) = struct

  (* Global mutable state: the timeout task for a sent packet. *)
  let to_cancel = ref None

  (* Send a single packet with the given time to live. *)
  let rec send_udp udp timeout host ttl =
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
            send_udp udp timeout host (succ ttl))
          (function Lwt.Canceled -> Lwt.return_unit | exc -> Lwt.fail exc)
      in
      (* Assign this timeout task. *)
      to_cancel := Some cancel;
      (* Figure out which source and destination port to use, based on ttl
         and current timestamp. *)
      let src_port, dst_port = ports_of_ttl_ts ttl (Mirage_mtime.elapsed_ns ()) in
      (* Send packet via UDP. *)
      DHCP.UDP.write ~ttl ~src_port ~dst:host ~dst_port udp Cstruct.empty >>= function
      | Ok () -> Lwt.return_unit
      | Error e -> Lwt.fail_with (Fmt.str "while sending udp frame %a" DHCP.UDP.pp_error e)

  (* The main unikernel entry point. *)
  let start icmp stack _ (message : string option) host timeout =
    Lwt.pause () >>= fun () ->
    Logs.app (fun m -> m "traceroute started!");
    Logs.app (fun m -> m "Hello, %s" (Option.value ~default:"no one." message));
    let host = Ipaddr.V4 host in
    let udp = DHCP.udp stack in
    let send = send_udp udp timeout host in
    Icmp.set_send icmp send;
    (* Send the initial UDP packet with a ttl of 1. This entails the domino
       effect to receive ICMP packets, send out another UDP packet with ttl
       increased by one, etc. - until a destination unreachable is received,
       or the hop limit is reached. *)
    send 1 >>= fun () ->
    Icmp.task icmp >|= fun () ->
    Logs.app (fun m -> m "Goodbye!")
end
