let run_cmd cmd =
    match Sys.command cmd with
    | 0 -> ()
    | _ -> failwith ("Command failed: " ^ cmd)

let ensure_bridge_exists () =
    let check_cmd = "ip link show pint0 > /dev/null 2>&1" in

    match Sys.command check_cmd with
    | 0 -> 
        Printf.printf "[Network] Bridge pint0 already exists. Attaching...\n%!"
    | _ ->
        Printf.printf "[Network] Creating main bridge pint0...\n%!";
        run_cmd "ip link add pint0 type bridge";
        run_cmd "ip addr add 10.0.0.1/24 dev pint0";
        run_cmd "ip link set pint0 up";
        
        run_cmd "iptables -t nat -A POSTROUTING -s 10.0.0.0/24 -j MASQUERADE";
        run_cmd "sysctl -w net.ipv4.ip_forward=1";
        run_cmd "iptables -A FORWARD -i pint0 -j ACCEPT";
        run_cmd "iptables -A FORWARD -o pint0 -j ACCEPT"

let setup_net_ns container_pid container_ip = 
    ensure_bridge_exists ();

    let host_iface = Printf.sprintf "veth%d" container_pid in
    let temp_peer = Printf.sprintf "vpeer%d" container_pid in

    ignore (host_iface |> Printf.sprintf "ip link delete %s 2>/dev/null" |> Sys.command);
    ignore (temp_peer |> Printf.sprintf "ip link delete %s 2>/dev/null" |> Sys.command);

    Printf.sprintf "ip link add %s type veth peer name %s" host_iface temp_peer |> run_cmd;

    host_iface |> Printf.sprintf "ip link set %s master pint0" |> run_cmd;
    host_iface |> Printf.sprintf "ip link set %s up" |> run_cmd;

    Printf.sprintf "ip link set %s netns %d" temp_peer container_pid |> run_cmd;

    Printf.sprintf "nsenter -t %d -n ip link set dev %s name eth0" container_pid temp_peer |> run_cmd;

    Printf.sprintf "nsenter -t %d -n ip addr add %s/24 dev eth0" container_pid container_ip |> run_cmd;
    container_pid |> Printf.sprintf "nsenter -t %d -n ip link set eth0 up"  |> run_cmd;
    container_pid |> Printf.sprintf "nsenter -t %d -n ip link set lo up" |> run_cmd;
    container_pid |> Printf.sprintf "nsenter -t %d -n ip route add default via 10.0.0.1" |> run_cmd;

