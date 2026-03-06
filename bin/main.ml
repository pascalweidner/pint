open Pint

let usage () =
    print_endline "Usage: pint <command> [args]";
    print_endline "Commands:";
    print_endline "  run <config.json>      Start a new container";
    print_endline "  stop <container_name>  Stop a running container";
    print_endline "  rm <container_name>    Delete a stopped container";
    print_endline "  start <container_name> Start a stopped container"


let internal_shim args =
    if (Array.length args) <> 4 then failwith "Internal error: Invalid shim";

    let id = args.(2) in
    let folder = args.(3) in

    let config_path = Printf.sprintf "%s/config.json" folder in
    let config = Parse.load_config config_path in

    Shim.setup_and_start config folder id

let run args =
    if (Array.length args) <> 3 then usage ();

    let config_path = args.(2) in

    Cli.setup_and_start_container config_path

let stop args =
    if (Array.length args) <> 3 then usage ();

    let container_id = args.(2) in

    Cli.stop_container container_id


let () =
    Init.init_infrastructure ();

    let args = Sys.argv in

    if Array.length args < 2 then (
        usage ();
        exit 1;
    ) else

        match args.(1) with
        | "run" -> run args
        | "stop" -> stop args
        | "internal_shim" -> internal_shim args
        | _ -> 
            Printf.printf "Unknown command: %s\n" args.(1);
            usage ();
            exit 1
