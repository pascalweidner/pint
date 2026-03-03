let find_cgroup2_mount () =
    let filename = "/proc/mounts" in
    let ic = open_in filename in
    let rec loop () =
        try
            let line = input_line ic in
            match String.split_on_char ' ' line with
            | _device :: path :: "cgroup2" :: _rest ->
                close_in ic;
                Some path
            | _ -> loop ()
        with End_of_file ->
            close_in ic;
            None
    in 
    loop ()

let setup_cgroup container_pid =
    match find_cgroup2_mount () with
    | Some path ->
        let cgroup_dir = Printf.sprintf "%s/container_%d" path container_pid in
        Printf.printf "Setting up cgroup at: %s\n" cgroup_dir;

        begin
            try 
                Unix.mkdir cgroup_dir 0o755 
            with 
            |Unix.Unix_error (Unix.EEXIST, _, _) -> 
                Printf.printf "[CGroup] Leftover cgroup found. Cleaning up...\n%!";
                Unix.rmdir cgroup_dir;
                Unix.mkdir cgroup_dir 0o755
        end;
        
        let procs_path = cgroup_dir ^ "/cgroup.procs" in
        let oc = open_out procs_path in
        Printf.fprintf oc "%d\n" container_pid;
        close_out oc;

        Printf.printf "[CGroup] Trapped PID %d in cgroup successfully.\n%!" container_pid
    | None ->
        failwith "[CGroup] Cgroup mount was not found\n"

let write_limit name limit_path raw_value convert =
    let str_val = convert raw_value in
    Printf.printf "[CGroup] Setting %s limit to %s...\n%!" name str_val;
    try
        let oc = open_out limit_path in
        Printf.fprintf oc "%s\n" str_val;
        close_out oc;
        Printf.printf "[CGroup] Successfully set %s.\n%!" name
    with Sys_error err ->
        Printf.printf "[CGroup] Error setting %s: %s\n%!" name err

let add_limits (limits_opt: Parse.resource_config option) container_pid =
    match find_cgroup2_mount () with
    | None -> failwith "[CGroup] Fatal: cgroup2 mount not found!"
    | Some path ->
        match limits_opt with
        | None -> 
            let cgroup_dir = Printf.sprintf "%s/container_%d/pids.max" path container_pid in
            write_limit "PIDs" cgroup_dir 512 string_of_int
        | Some limits ->
            let cgroup_dir = Printf.sprintf "%s/container_%d" path container_pid in

            let pids_val = Option.value limits.pids_limit ~default:512 in
            write_limit "PIDs" (cgroup_dir ^ "/pids.max") pids_val string_of_int;

            Option.iter (fun mb ->
                write_limit "Memory" (cgroup_dir ^ "/memory.max") mb (fun x ->
                    string_of_int (x * 1024 * 1024))
            ) limits.memory_mb;

            Option.iter (fun cores ->
                write_limit "CPU" (cgroup_dir ^ "/cpu.max") cores (fun c ->
                    let period = 100_000 in
                    let quota = int_of_float (c *. float_of_int period) in
                    Printf.sprintf "%d %d" quota period
                )
            ) limits.cpus

let destroy_cgroup container_pid =
    match find_cgroup2_mount () with
    | Some path ->
        let cgroup_dir = Printf.sprintf "%s/container_%d" path container_pid in
        Printf.printf "Removing cgroup at: %s\n" cgroup_dir;
        begin try
            Unix.rmdir cgroup_dir
        with Unix.Unix_error (err, _, _) ->
            Printf.printf "[Warning] Failed to remove cgroup: %s\n%!" (Unix.error_message err)
        end
    | None -> ()
