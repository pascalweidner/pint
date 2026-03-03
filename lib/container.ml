let safe_mkdir dir = 
    try
        Unix.mkdir dir 0o755
    with
    | Unix.Unix_error (err, _, _) ->
        let msg = Unix.error_message err in
        failwith (Printf.sprintf "[Fatal] Failed to create %s: %s\n" dir msg)


let notify_pipe fd =
    let written = Unix.write_substring fd "x" 0 1 in
    if written <> 1 then failwith "Failed to write pipe"

let wait_pipe fd =
    let buf = Bytes.create 1 in
    let bytes_read = Unix.read fd buf 0 1 in
    if bytes_read = 0 then
        failwith "Pipe returned 0 bytes (EOF). The other process crashed!"

let setup_logger id = 
    let path = Printf.sprintf "/var/lib/pint/containers/%s/container.log" id in
    let oc = open_out_gen [Open_creat; Open_wronly; Open_append] 0o644 path in

    let start_msg = Printf.sprintf "{\"log\": \"Engine attached\", \"stream\": \"system\", \"time\": \"%f\"}" (Unix.gettimeofday ()) in
    output_string oc start_msg;
    flush oc;
    oc
    

let rec read_log_stream stream_name ic log_oc =
    let open Lwt.Infix in
    Lwt_io.read_line_opt ic >>= function
    | Some line ->
        let json_log = Printf.sprintf
        "{\"log\": \"%s\", \"stream\": \"%s\", \"time\": \"%f\"}" 
            (String.escaped line) stream_name (Unix.gettimeofday ())
        in 
        output_string log_oc json_log;
        flush log_oc;
        
        read_log_stream stream_name ic log_oc
    | None -> 
        Printf.printf "[Logger] %s stream closed.\n%!" stream_name;
        Lwt.return_unit


let log id stdout stderr =
    let open Lwt.Infix in

    Unix.set_nonblock stdout;
    Unix.set_nonblock stderr;
    
    let stdout_fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false stdout in
    let stderr_fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false stderr in

    let stdout_ic = Lwt_io.of_fd ~mode:Lwt_io.Input stdout_fd in
    let stderr_ic = Lwt_io.of_fd ~mode:Lwt_io.Input stderr_fd in

    let log_oc = setup_logger id in   

    Printf.printf "[Logger] Log multiplexer attached and listening...\n%!";

    Lwt.join [
        read_log_stream "stdout" stdout_ic log_oc;
        read_log_stream "stderr" stderr_ic log_oc;
    ] >>= fun () ->

    Printf.printf "[Logger] Container logging finished. Closing file.\n%!";
    close_out log_oc;
    Lwt.return_unit
    

let start (config: Parse.container_config) folder ip id =
    let open Lwt.Infix in

    let (syn_pipe1_r, syn_pipe1_w) = Unix.pipe () in
    let (syn_pipe2_r, syn_pipe2_w) = Unix.pipe () in

    Lwt_main.run (

        let pid = Lwt_unix.fork () in

        if pid = 0 then begin
            Unix.close syn_pipe1_w;
            Unix.close syn_pipe2_r;

            let (stdout_r, stdout_w) = Unix.pipe() in
            let (stderr_r, stderr_w) = Unix.pipe() in

            try
                Isolate.(namespaces [NEWPID; NEWNET]);
                
                match Unix.fork() with
                | 0 ->
                    Unix.close syn_pipe2_w;

                    Isolate.(namespaces [NEWNS; NEWIPC; NEWUTS]);

                    Uts.setup_uts_ns config.container_name config.container_name;

                    wait_pipe syn_pipe1_r;
                    Unix.close syn_pipe1_r;

                    Isolate.(namespaces [NEWCGROUP]);

                    Mount.setup_mount_ns folder;

                    Printf.printf "\n[Container] pivot_root successful!\n";

                    let dev_null = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o666 in
                    Unix.dup2 dev_null Unix.stdin;
                    Unix.close dev_null;

                    Unix.dup2 stdout_w Unix.stdout;
                    Unix.dup2 stderr_w Unix.stderr;

                    Unix.close stdout_w;
                    Unix.close stderr_w;
                    Unix.close stdout_r;
                    Unix.close stderr_r;

                    Printf.printf "\n[Container] Start shell...\n%!";

                    let cmd_array =
                        if Array.length config.command = 0 then [|"/bin/sh"|]
                        else config.command
                    in
                    Unix.execvp cmd_array.(0) cmd_array
                | gc_pid ->
                    Unix.close syn_pipe1_r;
                    Unix.close stdout_w;
                    Unix.close stderr_w;

                    let dev_null = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0o666 in
                    Unix.dup2 dev_null Unix.stdout;
                    Unix.dup2 dev_null Unix.stderr;
                    Unix.close dev_null;

                    Cgroup.setup_cgroup gc_pid;
                    Cgroup.add_limits config.resources gc_pid;

                    notify_pipe syn_pipe2_w;
                    Unix.close syn_pipe2_w;

                    log id stdout_r stderr_r >>= fun () ->

                    ignore (Unix.waitpid [] gc_pid);

                    Cgroup.destroy_cgroup gc_pid;
                    Lwt.return_unit
            with e ->
                Printf.printf "\n[CHILD CRASHED] %s\n%!" (Printexc.to_string e);
                exit 1
        end else begin
            Unix.close syn_pipe1_r;
            Unix.close syn_pipe2_w;

            Printf.printf "[Main] Container started with PID: %d\n%!" pid;

            wait_pipe syn_pipe2_r;
            Unix.close syn_pipe2_r;

            Net.setup_net_ns pid ip;
            (*TODO: Tear down the veth cables when the container exits)*)
            
            notify_pipe syn_pipe1_w;
            Unix.close syn_pipe1_w;

            Printf.printf "[Main] Network ready. CLI exiting, container running in backgroun!\n%!";
            Lwt.return_unit
        end
    )
    

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
        Printf.printf "[Image] Rootfs ready at %s\n%!" rootfs_dir;
        rootfs_dir
    | _ ->
        failwith "Fatal: GNU Tar failed to extract the rootfs!"


let setup container_id config_path = 
    let path = Printf.sprintf "/var/lib/pint/containers/%s" container_id in
    safe_mkdir path;
    let dirs = [
        path ^ "/rootfs";
    ] in
    List.iter safe_mkdir dirs;
    copy_file config_path (path ^ "/config.json");
    setup_rootfs container_id
