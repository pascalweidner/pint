
let read_ips file_path =
    try
        let ic = open_in file_path in
        let rec aux acc =
            try
                let line = input_line ic in
                aux (int_of_string line :: acc)
            with End_of_file ->
                close_in ic;
                acc
        in
        aux [] |> List.sort Int.compare
    with 
        | Sys_error _ -> []


(*TODO: Factor this atomic write out to use it for all writes*)
let write_ips_ordered file_path ips =
    let updated_ips = List.sort Int.compare ips in
    (*Creates a temporary file to which to write before swapping
      the temp file with the real one*)
    let temp_path = file_path ^ ".tmp" in

    try
        Out_channel.with_open_text temp_path (fun oc -> 
            List.iter (fun ip -> Printf.fprintf oc "%d\n" ip) updated_ips
        );

        (*Overwrites the original file with the temporary one*)
        Sys.rename temp_path file_path
    with exn ->
        if Sys.file_exists temp_path then Sys.remove temp_path;
        failwith ("[IPAM] Fatal: IPs could not be written: " ^Printexc.to_string exn)



let get_ip () =
    let lock_file = "/run/pint/network/ipam.lock" in
    let file_path = "/run/pint/network/allocated.txt" in

    let lock_fd = Unix.openfile lock_file Unix.[O_CREAT; O_WRONLY] 0o644 in
    Unix.lockf lock_fd Unix.F_LOCK 0;

    try 
        let ips = read_ips file_path in

        let rec find_smallest expected = function
            | [] ->
                if expected < 255 then expected
                else failwith "[IPAM] Fatal: Subnet is completely full!"
            | h :: t ->
                if expected < h then
                    expected
                else if expected = h then
                    find_smallest (expected + 1) t
                else
                    find_smallest expected t
        in

        let next_ip = find_smallest 2 ips in

        write_ips_ordered file_path (next_ip :: ips);

        Printf.printf "[IPAM] Ip written to allocated.txt\n%!";

        Unix.lockf lock_fd Unix.F_ULOCK 0;
        Unix.close lock_fd;

        Printf.sprintf "10.0.0.%d" next_ip
    with e ->
        Unix.lockf lock_fd Unix.F_ULOCK 0;
        Unix.close lock_fd;
        raise e

let release_ip ip =
    let lock_file = "/run/pint/network/ipam.lock" in
    let file_path = "/run/pint/network/allocated.txt" in

    let ip_to_release =
        try
            Scanf.sscanf ip "10.0.0.%d" (fun x -> x)
        with _ ->
            failwith (Printf.sprintf "[IPAM] Fatal: Malformed IP address passed to release: %s" ip)
    in

    let lock_fd = Unix.openfile lock_file Unix.[O_CREAT; O_WRONLY] 0o644 in
    Unix.lockf lock_fd Unix.F_LOCK 0;

    try
        let ips = read_ips file_path in
        
        let updated_ips = List.filter (fun curr_ip -> curr_ip <> ip_to_release) ips in

        write_ips_ordered file_path updated_ips;

        Printf.printf "[IPAM] Successfully released IP: 10.0.0.%d\n%!" ip_to_release;

        Unix.lockf lock_fd Unix.F_ULOCK 0;
        Unix.close lock_fd
    with e ->
        Unix.lockf lock_fd Unix.F_ULOCK 0;
        Unix.close lock_fd;
        raise e


        

