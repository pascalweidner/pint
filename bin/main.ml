open Pint

let notify_pipe fd =
    let written = Unix.write_substring fd "x" 0 1 in
    if written <> 1 then failwith "Failed to write pipe"

let wait_pipe fd =
    let buf = Bytes.create 1 in
    let bytes_read = Unix.read fd buf 0 1 in
    if bytes_read = 0 then
        failwith "Pipe returned 0 bytes (EOF). The other process crashed!"


let () =
    let (pipe_r, pipe_w) = Unix.pipe () in
    let (pipe2_r, pipe2_w) = Unix.pipe () in
    let pid = Unix.fork() in
    if pid = 0 then begin
        Unix.close pipe_w;
        Unix.close pipe2_r;

        try 
            Isolate.(namespaces [NEWPID; NEWIPC; NEWNET; NEWNS; NEWUTS]);

            match Unix.fork() with
            | 0 -> 
                Uts.setup_uts_ns "pint1" "pint2";

                notify_pipe pipe2_w;
                Unix.close pipe2_w;

                wait_pipe pipe_r;
                Unix.close pipe_r;

                Mount.setup_mount_ns "/mnt/d/development/pint/container";

                Printf.printf "\n[Container] pivot_root successful!\n";
                let files = Sys.readdir "/" in
                Array.iter (fun f -> Printf.printf "  - %s\n" f) files;

                Printf.printf "\n[Container] Start shell...\n%!";
                Unix.execv "/bin/sh" [|"/bin/sh"|]
            | gc_pid -> 
                Unix.close pipe2_w;
                Unix.close pipe_r;

                ignore (Unix.waitpid [] gc_pid);
                exit 0

        with e ->
            Printf.printf "\n[CHILD CRASHED] %s\n%!" (Printexc.to_string e);
            exit 1
    end else begin
        Unix.close pipe_r;
        Unix.close pipe2_w;

        Printf.printf "[Main] Container started with PID: %d\n%!" pid;

        wait_pipe pipe2_r;
        Unix.close pipe2_r;

        Net.setup_net_ns pid "10.0.0.2";

        notify_pipe pipe_w;
        Unix.close pipe_w;

        Printf.printf "[Main] Network ready. Waiting on the end of the container...\n%!";
        ignore (Unix.waitpid [] pid);
        Printf.printf "[Main] Container exited.\n%!"
    end