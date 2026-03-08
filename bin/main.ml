open Pint

let usage () =
    print_endline "Usage: pint <command> [args]";
    print_endline "Commands:";
    print_endline "  run <config.json>      Start a new container";
    print_endline "  stop <container_name>  Stop a running container";
    print_endline "  rm <container_name>    Delete a stopped container";
    print_endline "  start <container_name> Start a stopped container";
    print_endline "  ps                     List all running containers"


let internal_shim args =
    (*if (Array.length args) <> 4 then failwith "Internal error: Invalid shim";*)

    let id = args.(2) in
    let folder = args.(3) in
    let interactive =
        if Array.length args = 5 && args.(4) = "-i" then 
            true
        else
            false
    in

    let config_path = Printf.sprintf "%s/config.json" folder in
    let config = Parse.load_config config_path in

    Shim.setup_and_start config folder id interactive


let attach args =
    if Array.length args <> 3 then usage ();

    let id = args.(2) in

    Cli.attach_container id


let run args =
    let args_list =
        let all_args = Array.to_list args in
        match all_args with
        | _ :: _ :: rest -> rest
        | _ -> []
    in

    let rec parse detach is_interactive config_path = function
        | [] -> (detach, is_interactive, config_path)
        | ("-id" | "-di") :: rest ->
            parse true true config_path rest
        | ("-d" | "--detach") :: rest ->
            parse true is_interactive config_path rest
        | ("-i" | "--interactive") :: rest ->
            parse detach true config_path rest
        | value :: rest ->
            if config_path = None then
                parse detach is_interactive (Some value) rest
            else failwith (Printf.sprintf "Unexpected argument: %s" value)
    in

    match parse false false None args_list with
        | (detach, is_interactive, Some config_path) ->
            Cli.setup_and_start_container config_path detach is_interactive
        | (_, _, None) ->
            usage ()


let stop args =
    if (Array.length args) <> 3 then usage (); 

    let container_id = args.(2) in

    Cli.stop_container container_id


let ps args =
    let all =
        if Array.length args >= 3 && (args.(2) = "-a" || args.(2) = "--all") then true 
        else false
    in
    
    Cli.list_containers all


let rm args =
    let args_list =
        let all_args = Array.to_list args in
        match all_args with
        | _ :: _ :: rest -> rest
        | _ -> []
    in

    let rec parse force id = function
        | [] -> (force, id)
        | ("-f" | "--foce") :: rest ->
            parse true id rest
        | value :: rest ->
            if id = None then
                parse force (Some value) rest
            else failwith (Printf.sprintf "Unepected argument: %s" value)
    in

    match parse false None args_list with
        | (force, Some container_id) ->
            Cli.remove_container container_id force
        | (_, None) ->
            usage()


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
        | "ps" -> ps args
        | "rm" -> rm args
        | "internal_shim" -> internal_shim args
        | "attach" -> attach args
        | _ -> 
            Printf.printf "Unknown command: %s\n" args.(1);
            usage ();
            exit 1
