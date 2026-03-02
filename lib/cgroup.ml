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
