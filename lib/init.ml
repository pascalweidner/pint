
let safe_mkdir dir = 
    try
        Unix.mkdir dir 0o755
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) ->
        ()
    | Unix.Unix_error (err, _, _) ->
        let msg = Unix.error_message err in
        failwith (Printf.sprintf "[Fatal] Failed to create %s: %s\n(Did you forget 'sudo'?)" dir msg)

let init_infrastructure () =
    let var_base = "/var/lib/pint" in
    let var_dirs = [
        var_base ^ "/cache";
        var_base ^ "/network";
        var_base ^ "/containers";
    ] in

    let run_base = "/run/pint" in
    let run_dirs = [
        run_base ^ "/network";
        run_base ^ "/containers";
    ] in

    safe_mkdir var_base;
    safe_mkdir run_base;

    List.iter safe_mkdir var_dirs;
    List.iter safe_mkdir run_dirs
         
