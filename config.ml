(* mirage >= 4.10.0 & < 4.11.0 *)

open Mirage

module Registry : sig
  type registry
  val registry : registry typ
end = struct
  type registry = REGISTRY
  let registry = typ REGISTRY
end

let main =
  let packages = [
    package ~min:"9.0.0" ~max:"10.0.0" ~sublibs:["ipv4"; "udp"; "icmpv4"] "tcpip";
    package "mtime";
    package ~min:"4.5.0" ~sublibs:["network"] "mirage-runtime";
    package ~min:"0.4.0" "logs-syslog";
  ]
  in
  main
    ~packages
    "Unikernel.Main"
    (network @-> Registry.registry @-> job)

let registry (* () *) =
  let connect _i modname = function
    | [] ->
      code ~pos:__POS__ {|%s.create ()|} modname
    | _ -> assert false
  in
  impl
    "Registry"
    Registry.registry
    ~connect

let dhcp_ipv4 =
  let packages = [
    package ~pin:"git+https://github.com/reynir/charrua.git#resolve" ~min:"2.1.0" ~sublibs:[ "mirage" ] "charrua-client";
    package ~pin:"git+https://github.com/reynir/charrua.git#resolve" ~min:"2.1.0" "charrua";
  ] in
  let connect _i modname = function
    | [ net; reg ] ->
      code ~pos:__POS__
        {|let options = [
          Dhcp_wire.Hostname (Mirage_runtime.name ());
          Dhcp_wire.Client_fqdn ([ `Server_A ], Unikernel.K.hostname ());
        ] in
        let requests = Dhcp_wire.[ SUBNET_MASK; ROUTERS; LOG_SERVERS ] in
        %s.connect ~registry:%s
          ?cidr:(Unikernel.K.ipv4 ()) ?gateway:(Unikernel.K.ipv4_gateway ())
          ~options ~requests %s|} modname reg net
    | _ -> assert false
  in
  impl
    ~packages
    ~connect
    "Dhcp_ipv4.Make"
    (* it's a network and more, but let's just pretend it's a network for now *)
    (network @-> Registry.registry @-> network)

let () =
  register "traceroute"
    [ main $ (dhcp_ipv4 $ default_network $ registry) $ registry ]
