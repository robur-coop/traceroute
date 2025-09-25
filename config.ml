(* mirage >= 4.10.0 & < 4.11.0 *)

open Mirage

let main =
  let packages = [
    package ~min:"9.0.0" ~max:"10.0.0" ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
    package ~min:"3.0.0" "ethernet";
    package ~sublibs:["mirage"] ~min:"3.0.0" "arp";
    package "mtime";
    package ~min:"4.5.0" ~sublibs:["network"] "mirage-runtime";
    package ~sublibs:[ "mirage" ] "charrua-client";
  ]
  in
  main
    ~packages
    "Unikernel.Main"
    (network @-> job)

let () =
  register "traceroute"
    [ main $ default_network ]
