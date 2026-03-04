let notify_pipe fd =
    let written = Unix.write_substring fd "x" 0 1 in
    if written <> 1 then failwith "Failed to write pipe"

let wait_pipe fd =
    let buf = Bytes.create 1 in
    let bytes_read = Unix.read fd buf 0 1 in
    if bytes_read = 0 then
        failwith "Pipe returned 0 bytes (EOF). The other process crashed!"
        
let setup_logger folder =
    let open Lwt.Infix in
    let path = Printf.sprintf "%s/container.log" folder in
    Lwt_io.open_file ~flags:[Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] ~perm:0o644 ~mode:Lwt_io.Output path >>= fun oc -> 

    let start_msg = Printf.sprintf "{\"log\": \"Engine attached\", \"stream\": \"system\", \"time\": \"%f\"}" (Unix.gettimeofday ()) in
    Lwt_io.write oc start_msg >>= fun () ->
    Lwt.return oc

let rec read_log_stream stream_name ic log_oc =
    let open Lwt.Infix in
    Lwt_io.read_line_opt ic >>= function
    | Some line ->
        let json_log = Printf.sprintf
        "{\"log\": \"%s\", \"stream\": \"%s\", \"time\": \"%f\"}" 
            (String.escaped line) stream_name (Unix.gettimeofday ())
        in
        Lwt_io.write log_oc json_log >>= fun () ->
        Lwt_io.flush log_oc >>= fun () ->
        read_log_stream stream_name ic log_oc
    | None ->
        Printf.printf "[Logger] %s stream closed.\n%!" stream_name;
        Lwt.return_unit


let log folder stdout stderr =
    let open Lwt.Infix in

    let stdout_fd = Lwt_unix.of_unix_file_descr stdout in
    let stderr_fd = Lwt_unix.of_unix_file_descr stderr in

    let stdout = Lwt_io.of_fd ~mode:Lwt_io.Input stdout_fd in
    let stderr = Lwt_io.of_fd ~mode:Lwt_io.Input stderr_fd in

    setup_logger folder >>= fun oc ->

    Printf.printf "[Logger] Log multiplexer attached and listening...\n%!";
    
    Lwt.join [
        read_log_stream "stdout" stdout oc;
        read_log_stream "stderr" stderr oc;
    ] >>= fun () ->

    Printf.printf "[Logger] Container logging finished. Closing file.\n%!";
    Lwt_io.close oc >>= fun () ->
    Lwt.return_unit

let setup_server path =
    let open Lwt.Infix in
    if Sys.file_exists path then Sys.remove path;

    let raw_sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Lwt_unix.bind raw_sock (Unix.ADDR_UNIX path) >>= fun () ->
    Lwt_unix.listen raw_sock 10;

    Lwt.return raw_sock

let server path gc_pid =
    let open Lwt.Infix in

    setup_server path >>= fun sock ->

    let rec accept_loop () =
        Lwt_unix.accept sock >>= fun (client_fd, _addr) ->
        
        let ic = Lwt_io.of_fd ~mode:Lwt_io.Input client_fd in

        Lwt_io.read_line_opt ic >>= function
        | Some "stop" -> 
            Printf.printf "\n[Shim] Received STOP command! Terminating container.\n%!";

            Unix.kill gc_pid Sys.sigterm;

            Lwt_unix.close client_fd >>= fun () ->
            accept_loop ()
        | Some unknown_cmd ->
            Printf.printf "[Shim] Unknown command received: %s\n%!" unknown_cmd;
            Lwt_unix.close client_fd >>= fun () ->
            accept_loop ()
        | None ->
            Lwt_unix.close client_fd >>= fun () ->
            accept_loop () 
    in
    accept_loop ()



let start_container (config: Parse.container_config) syn1_r folder stdout_w stderr_w syn2_w =
    try
            Isolate.(namespaces [NEWPID; NEWNET]);

            notify_pipe syn2_w;
            Unix.close syn2_w;

            wait_pipe syn1_r;
            Unix.close syn1_r;

            match Unix.fork() with
            | -1 -> failwith "[Shim] Fork failed"
            | 0 -> begin 
                Isolate.(namespaces [NEWNS; NEWIPC; NEWUTS; NEWCGROUP]);

                Uts.setup_uts_ns config.container_name config.container_name;

                Mount.setup_mount_ns (folder ^ "/rootfs");

                Printf.printf "\n[Container] pivot_root successful!\n";

                let dev_null = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o666 in
                Unix.dup2 dev_null Unix.stdin;
                Unix.close dev_null;

                Unix.dup2 stdout_w Unix.stdout;
                Unix.dup2 stderr_w Unix.stderr;

                Unix.close stdout_w;
                Unix.close stderr_w;

                Printf.printf "\n[Container] Start shell...\n%!";

                let cmd_array = 
                    if Array.length config.command = 0 then [|"/bin/sh"|]
                    else config.command
                in
                Unix.execvp cmd_array.(0) cmd_array

            end
            | pid -> 
                Unix.close stdout_w;
                Unix.close stderr_w;

                ignore (Unix.waitpid [] pid)


        with e ->
            Printf.printf "\n[Container] Crashed %s\n%!" (Printexc.to_string e);
            exit 1

let start_shim pid (config: Parse.container_config) ip syn1_w syn2_r stdout_r stderr_r folder =
    wait_pipe syn2_r;

    Cgroup.setup_cgroup pid;
    Cgroup.add_limits config.resources pid;

    Net.setup_net_ns pid ip;

    notify_pipe syn1_w;

    Unix.close syn1_w;
    Unix.close syn2_r;

    let sock_path = Printf.sprintf "%s/shim.sock" folder in

    Lwt.Infix.(Lwt_main.run(
        Lwt.pick [
            log folder stdout_r stderr_r;
            server sock_path pid;
        ] >>= fun _ ->
        Lwt.return_unit
    ));

    ignore (Unix.waitpid [] pid);

    Cgroup.destroy_cgroup pid;

    Ipam.release_ip ip


let start_daemon ip config folder =
    let (syn1_r, syn1_w) = Unix.pipe () in
    let (syn2_r, syn2_w) = Unix.pipe () in

    let (stdout_r, stdout_w) = Unix.pipe () in
    let (stderr_r, stderr_w) = Unix.pipe () in

    match Unix.fork() with
    | -1 -> failwith "[Shim] Fork failed"
    | 0 -> 
        Unix.close syn1_w;
        Unix.close stdout_r;
        Unix.close stderr_r;

        start_container config syn1_r folder stdout_w stderr_w syn2_w;
    | pid -> 
        Unix.close syn1_r;
        Unix.close syn2_w;
        Unix.close stdout_w;
        Unix.close stderr_w;

        start_shim pid config ip syn1_w syn2_r stdout_r stderr_r folder


let setup_and_start config folder =
    let ip = Ipam.get_ip () in
    start_daemon ip config folder