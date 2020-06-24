open Mirage

let host =
  let doc = Key.Arg.info ~doc:"The host to trace." ["host"] in
  Key.(create "host" Arg.(opt ipv4_address (Ipaddr.V4.of_string_exn "141.1.1.1") doc))

let timeout =
  let doc = Key.Arg.info ~doc:"Timeout (in millisecond)" ["timeout"] in
  Key.(create "timeout" Arg.(opt int 1000 doc))

let ipv4 =
  let doc = Key.Arg.info ~doc:"IPv4 address" ["ipv4"] in
  Key.(create "ipv4" Arg.(required ipv4 doc))

let ipv4_gateway =
  let doc = Key.Arg.info ~doc:"IPv4 gateway" ["ipv4-gateway"] in
  Key.(create "ipv4-gateway" Arg.(required ipv4_address doc))

let main =
  let packages = [
    package ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
    package "ethernet";
    package "arp-mirage";
    package "mirage-protocols";
    package "mtime";
  ] in
  foreign
    ~keys:[Key.abstract ipv4 ; Key.abstract ipv4_gateway ; Key.abstract host ; Key.abstract timeout]
    ~packages
    "Unikernel.Main"
    (random @-> mclock @-> time @-> network @-> job)

let () =
  register "traceroute"
    [ main $ default_random $ default_monotonic_clock $ default_time $ default_network ]
