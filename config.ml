(* mirage >= 4.7.0 & < 4.8.0 *)

open Mirage

let main =
  let packages = [
    package ~min:"7.0.0" ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
    package ~min:"3.0.0" "ethernet";
    package ~sublibs:["mirage"] ~min:"3.0.0" "arp";
    package "mtime";
    package ~min:"4.5.0" ~sublibs:["network"] "mirage-runtime";
  ]
  and runtime_args = [
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv4";
    runtime_arg ~pos:__POS__ "Unikernel.K.ipv4_gateway";
    runtime_arg ~pos:__POS__ "Unikernel.K.host";
    runtime_arg ~pos:__POS__ "Unikernel.K.timeout";
  ] in
  main
    ~runtime_args
    ~packages
    "Unikernel.Main"
    (random @-> mclock @-> time @-> network @-> job)

let () =
  register "traceroute"
    [ main $ default_random $ default_monotonic_clock $ default_time $ default_network ]
