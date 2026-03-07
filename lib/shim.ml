let notify_pipe fd =
    let written = Unix.write_substring fd "x" 0 1 in
    if written <> 1 then failwith "Failed to write pipe"

let wait_pipe fd =
    let buf = Bytes.create 1 in
    let bytes_read = Unix.read fd buf 0 1 in
    if bytes_read = 0 then
        failwith "Pipe returned 0 bytes (EOF). The other process crashed!"
        
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



let start_container (config: Parse.container_config) syn1_r folder stdout_w stderr_w syn2_w stdin_r=
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

                Unix.dup2 stdout_w Unix.stdout;
                Unix.dup2 stderr_w Unix.stderr;

                Unix.close stdout_w;
                Unix.close stderr_w;

                Printf.printf "\n[Container] pivot_root successful!\n";

                match stdin_r with
                | Some r -> begin
                    Unix.dup2 r Unix.stdin;
                    Unix.close r
                end
                | None -> begin
                    let dev_null = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0o666 in
                    Unix.dup2 dev_null Unix.stdin;
                    Unix.close dev_null
                end;
                

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
                (match stdin_r with
                | Some r -> Unix.close r
                | None -> ());

                Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ ->
                    try Unix.kill pid Sys.sigkill with _ -> ()
                ));

                let rec wait_loop () =
                    try
                        ignore (Unix.waitpid [] pid)
                    with Unix.Unix_error (Unix.EINTR, _, _) ->
                        wait_loop ();
                in
                wait_loop ()


        with e ->
            Printf.printf "\n[Container] Crashed %s\n%!" (Printexc.to_string e);
            exit 1

let start_shim pid (config: Parse.container_config) ip syn1_w syn2_r stdout_r stderr_r folder id stdin_w =
    wait_pipe syn2_r;

    Cgroup.setup_cgroup pid;
    Cgroup.add_limits config.resources pid;

    Net.setup_net_ns pid ip;

    notify_pipe syn1_w;

    Unix.close syn1_w;
    Unix.close syn2_r;

    let sock_path = Printf.sprintf "/run/pint/containers/%s/shim.sock" id in
    let log_sock_path = Printf.sprintf "/run/pint/containers/%s/logs.sock" id in
    let stdin_sock_path = Printf.sprintf "/run/pint/containers/%s/stdin.sock" id in

    let log_buffer = Ringbuffer.create 1000 in

    let base_servers = [
        Multiplexer.start folder stdout_r stderr_r log_buffer;
        server sock_path pid;
        Logserver.start log_sock_path log_buffer;
    ] in

    let all_servers = match stdin_w with
        | Some w ->
            (Stdinserver.start stdin_sock_path w) :: base_servers
        | None ->
            base_servers
    in

    Lwt.Infix.(Lwt_main.run(
        Lwt.pick all_servers >>= fun _ ->
        Lwt.return_unit
    ));

    ignore (Unix.waitpid [] pid);

    Cgroup.destroy_cgroup pid;

    Ipam.release_ip ip;

    Sys.remove sock_path;
    Sys.remove log_sock_path;
    (match stdin_w with Some _ -> Sys.remove stdin_sock_path | None -> ());
    Unix.rmdir (Printf.sprintf "/run/pint/containers/%s" id)


let start_daemon ip config folder id is_interactive =
    let (syn1_r, syn1_w) = Unix.pipe () in
    let (syn2_r, syn2_w) = Unix.pipe () in

    let (stdout_r, stdout_w) = Unix.pipe () in
    let (stderr_r, stderr_w) = Unix.pipe () in

    let stdin_pipe = if is_interactive then
        Some (Unix.pipe ()) else None
    in


    match Unix.fork() with
    | -1 -> failwith "[Shim] Fork failed"
    | 0 -> 
        Unix.close syn1_w;
        Unix.close stdout_r;
        Unix.close stderr_r;

        let stdin_r =
            match stdin_pipe with
            | Some (r, w) -> Unix.close w; Some r
            | None -> None
        in

        start_container config syn1_r folder stdout_w stderr_w syn2_w stdin_r;
    | pid -> 
        Unix.close syn1_r;
        Unix.close syn2_w;
        Unix.close stdout_w;
        Unix.close stderr_w;

        let stdin_w = 
            match stdin_pipe with
            | Some (r, w) -> Unix.close r; Some w
            | None -> None
        in

        start_shim pid config ip syn1_w syn2_r stdout_r stderr_r folder id stdin_w


let setup_and_start config folder id is_interactive =
    ignore (Unix.setsid ());

    let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o666 in
    (*Unix.dup2 dev_null Unix.stdin;
    Unix.dup2 dev_null Unix.stdout;*)
    Unix.dup2 dev_null Unix.stderr;
    Unix.close dev_null;

    let ip = Ipam.get_ip () in


    start_daemon ip config folder id is_interactive
