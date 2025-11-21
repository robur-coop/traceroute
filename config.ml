(* mirage >= 4.10.0 & < 4.11.0 *)

open Mirage

type dhcpstackv4 = STACKV4
let dhcpstackv4 = typ STACKV4

(* Following definitions from mirage/lib/devices/icmp.ml *)
type 'a icmp = ICMP
type icmpv4 = v4 icmp
let icmp = typ ICMP
let icmpv4 : icmpv4 typ = icmp

type lease = LEASE
let lease = typ LEASE

let main =
  let packages = [
    package ~min:"9.0.0" ~max:"10.0.0" ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
    package "mtime";
    package ~min:"4.5.0" ~sublibs:["network"] "mirage-runtime";
  ]
  in
  let runtime_args = [
    runtime_arg ~pos:__POS__
      "Unikernel.K.host";
    runtime_arg ~pos:__POS__
      "Unikernel.K.timeout";
  ] in
  main
    ~packages ~runtime_args
    "Unikernel.Main"
    (icmpv4 @-> stackv4v6 @-> syslog @-> job)

let pin = "git+https://github.com/reynir/charrua.git#registry"

let dhcpstack =
  let runtime_args = [
    runtime_arg ~pos:__POS__
      "Unikernel.K.hostname";
    runtime_arg ~pos:__POS__
      "Unikernel.K.ipv4";
    runtime_arg ~pos:__POS__
      "Unikernel.K.ipv4_gateway";
  ] in
  let packages = [
    package ~pin ~min:"2.1.0" ~sublibs:[ "mirage" ] "charrua-client";
    package ~pin ~min:"2.1.0" "charrua";
    package ~min:"9.0.0" ~max:"10.0.0" ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
  ] in
  let connect _i modname = function
    | [ net; fqdn; cidr; gateway ] ->
      code ~pos:__POS__
        {|let options = [
          Dhcp_wire.Client_fqdn ([ `Server_A ], %s);
          Dhcp_wire.Hostname (Mirage_runtime.name ());
        ] in
        let requests = Dhcp_wire.[ SUBNET_MASK; ROUTERS; LOG_SERVERS ] in
        %s.connect %s ?cidr:%s ?gateway:%s ~options ~requests|}
        fqdn modname net cidr gateway
    | _ -> assert false
  in
  impl ~packages ~connect ~runtime_args
    "Dhcp_ipv4.Make"
    (network @-> dhcpstackv4)

let proj_connect _i modname = function
  | [ dhcpstack ] -> code ~pos:__POS__ "%s.connect %s" modname dhcpstack
  | _ -> assert false

let proj_packages = [ package ~pin ~min:"2.1.0" ~sublibs:["mirage"] "charrua-client" ]

let proj_net =
  impl ~packages:proj_packages ~connect:proj_connect
    "Dhcp_ipv4.Proj_net" (dhcpstackv4 @-> network)

let proj_ethernet =
  impl ~packages:proj_packages ~connect:proj_connect
    "Dhcp_ipv4.Proj_ethernet" (dhcpstackv4 @-> ethernet)

let proj_arpv4 =
  impl ~packages:proj_packages ~connect:proj_connect
    "Dhcp_ipv4.Proj_arp" (dhcpstackv4 @-> arpv4)

let proj_ipv4 =
  impl ~packages:proj_packages ~connect:proj_connect
    "Dhcp_ipv4.Proj_ipv4" (dhcpstackv4 @-> ipv4)

let proj_lease =
  impl ~packages:proj_packages ~connect:proj_connect
    "Dhcp_ipv4.Proj_lease" (dhcpstackv4 @-> lease)

(* Not copied from mirage *)
let icmp =
  let runtime_args = [
    runtime_arg ~pos:__POS__
      "Unikernel.K.host";
  ] in
  let connect _i modname = function
    | [ remote_host ] ->
      code ~pos:__POS__ "%s.connect %s" modname remote_host
    | _ -> assert false
  in
  impl ~connect ~runtime_args "Unikernel.Icmp" icmpv4

(* The following two definitions are altered versions from mirage/lib/devices/stack.ml *)
let stackv4v6_direct_conf () =
  let packages = [ package ~min:"9.0.0" ~max:"10.0.0" ~sublibs:[ "stack-direct" ] "tcpip" ] in
  let connect _i modname = function
    | [ interface; ethif; arp; ipv4v6; icmpv4; udp; tcp ] ->
        code ~pos:__POS__ "%s.connect %s %s %s %s %s %s %s" modname interface
          ethif arp ipv4v6 icmpv4 udp tcp
    | _ -> assert false
  in
  impl ~packages ~connect "Tcpip_stack_direct.MakeV4V6"
    (network
    @-> ethernet
    @-> arpv4
    @-> ipv4v6
    @-> icmpv4
    @-> udp
    @-> tcp
    @-> stackv4v6)

let direct_stackv4v6 ?group ?tcp network eth arp ipv4 ipv6 icmp =
  let ip = create_ipv4v6 ?group ipv4 ipv6 in
  stackv4v6_direct_conf ()
  $ network
  $ eth
  $ arp
  $ ip
  $ icmp (* XXX: here we inject our own ICMP implementation *)
  $ direct_udp ip
  $ match tcp with None -> direct_tcp ip | Some tcp -> tcp

(* Following modified definition from dns-secondary/config.ml *)
let syslog dhcp_lease =
  let packages = [
    package ~sublibs:["mirage"] ~min:"0.4.0" "logs-syslog"
      ~pin:"git+https://github.com/reynir/logs-syslog.git#reporting";
    package ~pin ~min:"2.1.0" "charrua"; (* for Dhcp_wire *)
  ] in
  let connect _ modname = function
    | [ stack ; lease ] ->
      code ~pos:__POS__
        "Lwt.return (match Option.map Dhcp_wire.collect_log_servers %s with
         | Some (ip :: rem) ->
           let ip = Ipaddr.V4 ip in
           let reporter = %s.create %s ip ~hostname:(Mirage_runtime.name ()) () in
           let old_reporter = Logs.reporter () in
           (* We first use the old reporter and then the newer reporter *)
           let report = fun src level ~over k msgf ->
             let v = old_reporter.Logs.report src level ~over:(fun () -> ()) k msgf in
             reporter.Logs.report src level ~over (fun () -> v) msgf
           in
           Logs.set_reporter { Logs.report };
           Logs.info (fun m -> m \"Using log server %%a.\" Ipaddr.pp ip);
           Logs.info (fun m ->
             if rem <> [] then
               m \"Ignoring additional log servers %%a\"
                 Fmt.(list ~sep:(any \" \") Ipaddr.V4.pp) rem)
         | None | Some [] -> Logs.warn (fun m -> m \"no syslog specified, dumping on stdout\"))"
        lease modname stack
    | _ -> assert false
  in
  impl ~packages ~connect "Logs_syslog_mirage.Udp"
    ~extra_deps:[ dep dhcp_lease ]
    (stackv4v6 @-> syslog)

let () =
  let dhcpstackv4 = dhcpstack $ default_network in
  let net = proj_net $ dhcpstackv4 in
  let ethernet = proj_ethernet $ dhcpstackv4 in
  let stack =
    direct_stackv4v6 net ethernet (proj_arpv4 $ dhcpstackv4) (proj_ipv4 $ dhcpstackv4) (create_ipv6 net ethernet) icmp
  in

  register "traceroute"
    [ main $ icmp $ stack $ (syslog (proj_lease $ dhcpstackv4) $ stack) ]
