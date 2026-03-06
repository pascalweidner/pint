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

let safe_mkdir dir = 
    try
        Unix.mkdir dir 0o755
    with
    | Unix.Unix_error (err, _, _) ->
        let msg = Unix.error_message err in
        failwith (Printf.sprintf "[Fatal] Failed to create %s: %s\n" dir msg)

let copy_file source dest =
    let ic = open_in_bin source in
    let oc = open_out_bin dest in
    let buffer_size = 8192 in
    let buffer = Bytes.create buffer_size in
    let rec loop () =
        let len = input ic buffer 0 buffer_size in
        if len > 0 then (
            output oc buffer 0 len;
            loop ()
        )
    in
    try 
        loop ();
        close_in ic;
        close_out oc
    with e ->
        close_in_noerr ic;
        close_out_noerr oc;
        raise e 

let download_file url dest_path =
    let open Lwt.Infix in
    Printf.printf "[Image] Downloading %s to %s...\n" url dest_path;

    Lwt_unix.openfile dest_path Unix.[O_CREAT; O_WRONLY; O_TRUNC] 0o644 >>= fun fd ->
    let out_channel = Lwt_io.of_fd ~mode:Lwt_io.Output fd in

    Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (resp, body) ->
    
    let status = Cohttp.Response.status resp in
    if status <> `OK then
        Lwt_io.close out_channel >>= fun () ->
        Lwt.fail_with (Printf.sprintf "Http Error: %s" (Cohttp.Code.string_of_status status))
    else
        Cohttp_lwt.Body.to_stream body
        |> Lwt_stream.iter_s (fun chunk -> Lwt_io.write out_channel chunk)
        >>= fun () ->
        
        Lwt_io.close out_channel >>= fun () ->
        Printf.printf "[Image] Download complete.\n%!";
        Lwt.return_unit

let setup_rootfs container_id =
    let cache_dir = "/var/lib/pint/cache" in
    let tarball = cache_dir ^ "/alpine.tar.gz" in
    let rootfs_dir = Printf.sprintf "/var/lib/pint/containers/%s/rootfs" container_id in

    if not (Sys.file_exists tarball) then begin
        let url = "https://dl-cdn.alpinelinux.org/alpine/v3.19/releases/x86_64/alpine-minirootfs-3.19.1-x86_64.tar.gz" in
        Lwt_main.run (download_file url tarball)
    end;

    Printf.printf "[Image] Extracting root filesystem safely...\n%!";

    let pid = Unix.create_process "tar"
        [|"tar"; "-xzf"; tarball; "-C"; rootfs_dir|]
        Unix.stdin Unix.stdout Unix.stderr in

    let (_, status) = Unix.waitpid [] pid in
    match status with
    | Unix.WEXITED 0 ->
        Printf.printf "[Image] Rootfs ready at %s\n%!" rootfs_dir
    | _ ->
        failwith "Fatal: GNU Tar failed to extract the rootfs!"

let setup_run_folder container_id =
    let path = Printf.sprintf "/run/pint/containers/%s" container_id in
    safe_mkdir path

let setup_container config_path =
    let id = generate_id () in

    let path = Printf.sprintf "/var/lib/pint/containers/%s" id in
    safe_mkdir path;
    let dirs = [
        path ^ "/rootfs";
    ] in
    List.iter safe_mkdir dirs;
    copy_file config_path (path ^ "/config.json");
    setup_rootfs id;
    setup_run_folder id;
    path, id

let format_log json_str =
    let open Yojson.Basic.Util in
    try
        let json = Yojson.Basic.from_string json_str in

        json |> member "log" |> to_string
    with _ ->
        json_str

let send_command sock_path cmd =
    let open Lwt.Infix in
    
    Lwt_unix.file_exists sock_path >>= fun exists ->
    if not exists then begin
        Printf.printf "Cannot send command: Socket %s not found.\n%!" sock_path;
        Lwt.return_unit
    end else begin
        let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        
        Lwt.catch (fun () ->
            Lwt_unix.connect socket (Unix.ADDR_UNIX sock_path) >>= fun () ->
            
            let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
            
            Lwt_io.write_line oc cmd >>= fun () ->
            Lwt_io.close oc
        ) (fun exn ->
            Printf.printf "Failed to send command to Shim: %s\n%!" (Printexc.to_string exn);
            Lwt_unix.close socket
        )
    end

let stream_logs log_sock_path cmd_sock_path =
    let open Lwt.Infix in

    let _signal_id = Lwt_unix.on_signal Sys.sigint (fun _ ->
        Lwt.async (fun () ->
            send_command cmd_sock_path "stop" >>= fun () ->
            exit 0
        )
    ) in
    
    let rec connect_with_retry retries =
        if retries = 0 then
            Lwt.fail_with "Could not connect to log socket (Timeout)"
        else if Sys.file_exists log_sock_path then
            let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
            Lwt.catch (fun () ->
                Lwt_unix.connect sock (Unix.ADDR_UNIX log_sock_path) >>= fun () ->
                Lwt.return sock
            ) (fun _exn ->
                Lwt_unix.sleep 0.05 >>= fun () -> connect_with_retry (retries - 1)
            )
        else
            Lwt_unix.sleep 0.05 >>= fun () -> connect_with_retry (retries -1)
    in
    
    connect_with_retry 20 >>= fun sock ->
    
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
    let rec read_loop () = 
        Lwt_io.read_line_opt ic >>= function
        | Some raw_json ->
            let readable_msg = format_log raw_json in
            Printf.printf "%s\n%!" readable_msg;
            read_loop ()
        | None ->
            Printf.printf "\nContainer exited or log stream closed.\n%!";
            Lwt.return_unit
    in
    read_loop ()
    

let setup_and_start_container config_path detach =
    let (folder, id) = setup_container config_path in

    match Unix.fork () with
    | -1 -> failwith "Container could not be created";
    | 0 -> 
        ignore (Unix.execv "/proc/self/exe" [|"pint"; "internal_shim"; id; folder|])
    | _ ->
        if detach then begin
            Printf.printf "Container %s started in the background!\n%!" id;
            exit 0
        end else begin
            Printf.printf "Attaching to container %s... (Press Ctrl+C to stop)\n%!" id;

            let log_sock = Printf.sprintf "/run/pint/containers/%s/logs.sock" id in
            let cmd_sock = Printf.sprintf "/run/pint/containers/%s/shim.sock" id in
            
            Lwt_main.run (stream_logs log_sock cmd_sock)
        end

let stop_container container_id = 
    let folder = Printf.sprintf "/run/pint/containers/%s" container_id in
    if not (Sys.file_exists folder) then begin
        Printf.printf "The container with id %s does not exist!\n" container_id;
        exit 1
    end; 

    let path = Printf.sprintf "%s/shim.sock" folder in
    if not (Sys.file_exists path) then begin
        Printf.printf "The container with id %s is currently not running!\n" container_id;
        exit 1;
    end;

    let open Lwt.Infix in

    let success = Lwt_main.run (
        let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in

        Lwt.catch (fun () ->
            Lwt_unix.connect sock (Unix.ADDR_UNIX path) >>= fun () ->
            
            let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in

            Lwt_io.write_line oc "stop" >>= fun () ->
        
            Lwt_io.close oc >>= fun () ->
            Printf.printf "Container with id %s is stopping\n" container_id;
            Lwt.return_true
        )
        (fun _exn ->
            Lwt_unix.close sock >>= fun () ->
            Printf.printf "Stopping container with id %s failed with error %s\n" container_id 
                    (Printexc.to_string _exn);
            Lwt.return_false
        )
    ) in

    let wait_for_cleanup socket_path timeout_seconds =
        let start_time = Unix.gettimeofday () in
        
        let rec poll () =
            if not (Sys.file_exists socket_path) then
                true
            else if Unix.gettimeofday () -. start_time > timeout_seconds then
                false
            else begin
                Unix.sleepf 0.1;
                poll ()
            end
        in
        poll ()
    in
    if success then begin
        Printf.printf "Waiting for container to cleanly shut down...\n%!";

        if wait_for_cleanup path 5.0 then
            Printf.printf "Container %s stopped successfully!\n%!" container_id 
        else
            Printf.printf "Warning: Container %s did not shut down within 5 seconds.\n%!" container_id
    end else
        Printf.printf "Container %s is already stopped or the Shim crashed\n%!" container_id


let ping_container id =
    let open Lwt.Infix in
    let sock_path = Printf.sprintf "/run/pint/containers/%s/shim.sock" id in

    Lwt_unix.file_exists sock_path >>= fun exists ->
    if not exists then
        Lwt.return (id, "Stopped")
    else 
        let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        
        Lwt.catch (fun () ->
            Lwt_unix.connect sock (Unix.ADDR_UNIX sock_path) >>= fun () ->
            Lwt_unix.close sock >>= fun () ->
            Lwt.return (id, "Up")
        )
        (fun _exn ->
            Lwt_unix.close sock >>= fun () ->
            Lwt.return (id, "Exited (Dirty)")
        )


let get_container_ids path =
    let open Lwt.Infix in
    Lwt.catch (fun () ->
        Lwt_unix.opendir path >>= fun dir ->
        let rec read_all acc =
            Lwt.catch (fun () ->
                Lwt_unix.readdir dir >>= fun name ->
                if name = "." || name = ".." then
                    read_all acc
                else
                    read_all (name :: acc)
            )
            (function
                | End_of_file ->
                    Lwt_unix.closedir dir >>= fun () -> Lwt.return acc
                | exn ->
                    Lwt_unix.closedir dir >>= fun () -> Lwt.fail exn
            )
        in
        read_all []
    )
    (fun _exn ->
        Lwt.return []
    )


let list_containers all =
    let open Lwt.Infix in
    let path = 
        if all then "/var/lib/pint/containers"
        else"/run/pint/containers"
    in

    Lwt_main.run (
        get_container_ids path >>= fun ids ->

        if ids = [] then begin
            if all then 
                Printf.printf "No containers found on disk.\n%!"
            else
                Printf.printf "No running containers found.\n%!";
            Lwt.return_unit
        end else begin
            Lwt_list.map_p ping_container ids >>= fun results ->
            
            Printf.printf "%-35s %s\n" "CONTAINER ID" "STATUS";
            Printf.printf "-----------------------------------------------------\n";
            
            List.iter (fun (id, status) ->
                Printf.printf "%-35s %s\n" id status
            ) results;

            Lwt.return_unit
        end
    )


let rec rm_rf path =
    try
        let stats = Unix.lstat path in
        
        match stats.st_kind with
        | Unix.S_DIR ->
            let dir = Unix.opendir path in
            let rec process_entries () =
                match Unix.readdir dir with
                | "." | ".." ->
                    process_entries ()
                | entry ->
                    let child_path = Filename.concat path entry in
                    rm_rf child_path;
                    process_entries ()
                | exception End_of_file ->
                    ()
            in

            process_entries ();
            Unix.closedir dir;
            
            Unix.rmdir path
        | _ ->
            Unix.unlink path
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
        ()


let remove_container id force =
    let path = Printf.sprintf "/var/lib/pint/containers/%s" id in

    if not (Sys.file_exists path) then 
        Printf.printf "Container with id %s does not exist!\n%!" id
    else begin
        let (_, status) = Lwt_main.run (ping_container id) in

        if status = "Up" then begin
            if force then begin
                stop_container id;

                rm_rf path;
                Printf.printf "Successfully removed container %s\n%!" id
            end else 
                Printf.printf "Error: Container %s is still running. Stop it before removing it.\n%!" id
        end else begin
            
            rm_rf path;
            Printf.printf "Successfully removed container %s\n%!" id
        end
    end
        



    
        

    
