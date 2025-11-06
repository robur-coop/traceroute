open Lwt.Syntax

let src = Logs.Src.create "stack" ~doc:"DHCPv4 and UDPv4 stack"
module Log = (val Logs.src_log src : Logs.LOG)

let sizeof_ethernet = Ethernet.Packet.sizeof_ethernet

module Make_udpv4(Net : Mirage_net.S) = struct
  module DHCP = Dhcp_ipv4.Make(Net)
  module UDP = Udp.Make(DHCP.IPv4)
  include DHCP

  type t = {
    net : DHCP.Net.t;
    eth : DHCP.Ethernet.t;
    arp : DHCP.Arp.t;
    icmp : Unikernel.Icmp.t;
    ip : DHCP.IPv4.t;
    udp : UDP.t;
    lease : Dhcp_wire.dhcp_option list option Lwt.t;
    task : unit Lwt.t;
  }

  let net { net; _ } = net
  let eth { eth; _ } = eth
  let arp { arp; _ } = arp
  let icmp { icmp; _ } = icmp
  let ip { ip; _ } = ip
  let udp { udp; _ } = udp
  let lease { lease; _ } = lease

  let listen net eth arp ip icmp udp =
    Lwt.catch (fun () ->
        let default ~proto ~src ~dst buf =
          if proto = 1 then
            Unikernel.Icmp.input icmp ~src ~dst buf
          else
            Lwt.return_unit
        in
        let listener =
          Ethernet.input
            ~arpv4:(Arp.input arp)
            ~ipv4:(IPv4.input ip
                     ~default
                     ~tcp:(fun ~src:_ ~dst:_ _ -> Lwt.return_unit)
                     ~udp:(UDP.input udp))
            ~ipv6:(fun _ -> Lwt.return_unit)
            eth
        in
        let* r = Net.listen net listener ~header_size:sizeof_ethernet in
        match r with
        | Error e ->
          Log.warn (fun m -> m "%a" Net.pp_error e);
          Lwt.return_unit
        | Ok () ->
          Lwt.return_unit)
      (function
        | Lwt.Canceled ->
          Log.info (fun m -> m "listen of mac=%a cancelled" Macaddr.pp (Ethernet.mac eth));
          Lwt.return_unit
        | e -> Lwt.reraise e)

  let connect net ~hostname ~fqdn ~cidr ~gateway ~remote_host =
    let open Lwt.Syntax in
    let lease, wakeup_lease = Lwt.wait () in
    let options = [
      Dhcp_wire.Hostname hostname;
      Dhcp_wire.Client_fqdn ([ `Server_A ], fqdn);
    ] in
    let requests = Dhcp_wire.[ SUBNET_MASK; ROUTERS; LOG_SERVERS ] in
    let* (net, eth, arp, ip) = DHCP.connect net ~registry:wakeup_lease ?cidr ?gateway ~options ~requests in
    let* udp = UDP.connect ip in
    let* icmp = Unikernel.Icmp.connect remote_host in
    let task = listen net eth arp ip icmp udp in
    Lwt.return { net; eth; arp; ip; icmp; udp; lease; task}

  let disconnect { task; _ } =
    Lwt.cancel task;
    Lwt.return_unit
end
