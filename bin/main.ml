open Pint

let generate_id () =
    let ic = open_in_bin "/dev/urandom" in
    let raw_bytes = Bytes.create 16 in

    really_input ic raw_bytes 0 16;
    close_in ic;

    let hex_bytes = Bytes.create 32 in
    let hex_chars = "0123456789abcdef" in

    for i = 0 to 15 do
        let b = Char.code (Bytes.get raw_bytes i) in
        Bytes.set hex_bytes (i * 2) hex_chars.[b lsr 4];
        Bytes.set hex_bytes (i * 2 + 1) hex_chars.[b land 0x0f];
    done;

    Bytes.to_string hex_bytes

let () =
    Init.init_infrastructure ();

    let args = Sys.argv in
    match Array.length args with
    | 2 -> 
        let config = Parse.load_config args.(1) in
        let container_id = generate_id () in
        let free_ip = Ipam.get_ip () in
        let folder = Container.setup container_id args.(1) in
        Container.start config folder free_ip container_id
    | _ -> failwith ("No configuration specified!")