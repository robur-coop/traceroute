(* mirage >= 4.10.0 & < 4.11.0 *)

open Mirage

type dhcpstackv4 = STACKV4
let dhcpstackv4 = typ STACKV4

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
    (dhcpstackv4 @-> job)

let stack =
  let runtime_args = [
    runtime_arg ~pos:__POS__
      "Unikernel.K.hostname";
    runtime_arg ~pos:__POS__
      "Unikernel.K.ipv4";
    runtime_arg ~pos:__POS__
      "Unikernel.K.ipv4_gateway";
    runtime_arg ~pos:__POS__
      "Unikernel.K.host";
  ] in
  let packages = [
    package ~pin:"git+https://github.com/reynir/charrua.git#registry" ~min:"2.1.0" ~sublibs:[ "mirage" ] "charrua-client";
    package ~pin:"git+https://github.com/reynir/charrua.git#registry" ~min:"2.1.0" "charrua";
    package ~min:"9.0.0" ~max:"10.0.0" ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
  ] in
  let connect _i modname = function
    | [ net; fqdn; cidr; gateway; remote_host ] ->
      code ~pos:__POS__
        {|%s.connect %s ~hostname:(Mirage_runtime.name ()) ~fqdn:%s ~cidr:%s ~gateway:%s ~remote_host:%s|}
        modname net fqdn cidr gateway remote_host
    | _ -> assert false
  in
  impl ~packages ~connect ~runtime_args
    "Stack.Make_udpv4"
    (network @-> dhcpstackv4)

let syslog_udpv4 =
  let packages = [
    package ~min:"0.4.0" "logs-syslog";
    package "charrua";
  ] in
  let connect _i modname = function
    | [ stack ] ->
      code ~pos:__POS__
        {|%s.connect %s ~hostname:(Mirage_runtime.name ()) ()|}
        modname stack
    | _ -> assert false
  in
  impl ~packages ~connect
    "Unikernel.Syslog_udpv4"
    (dhcpstackv4 @-> job)

let () =
  register "traceroute"
    [ syslog_udpv4 $ (stack $ default_network) ;
      main $ (stack $ default_network) ]
