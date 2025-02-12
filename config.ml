(* mirage >= 4.8.0 & < 4.9.0 *)

open Mirage

let main =
  let packages = [
    package ~min:"7.0.0" ~max:"9.0.0" ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
    package ~min:"3.0.0" "ethernet";
    package ~sublibs:["mirage"] ~min:"3.0.0" "arp";
    package "mtime";
    package ~min:"4.5.0" ~sublibs:["network"] "mirage-runtime";
  ]
  in
  main
    ~packages
    "Unikernel.Main"
    (random @-> mclock @-> time @-> network @-> job)

let () =
  register "traceroute"
    [ main $ default_random $ default_monotonic_clock $ default_time $ default_network ]
