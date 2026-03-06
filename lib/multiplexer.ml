let setup_logger folder =
    let open Lwt.Infix in
    let path = Printf.sprintf "%s/container.logs" folder in
    Lwt_io.open_file ~flags:[Unix.O_CREAT; Unix.O_WRONLY; Unix.O_APPEND] ~perm:0o644 ~mode:Lwt_io.Output path >>= fun oc ->

    let start_msg = Printf.sprintf "{\"log\": \"Engine attached\", \"stream\": \"system\", \"time\": \"%f\"}" (Unix.gettimeofday ()) in
    Lwt_io.write_line oc start_msg >>= fun () ->
    Lwt.return oc


let rec read_log_stream stream_name ic log_oc ring_buffer =
    let open Lwt.Infix in
    Lwt_io.read_line_opt ic >>= function
        | Some line ->
            let json_obj = `Assoc [
                ("log", `String line);
                ("stream", `String stream_name);
                ("time", `Float (Unix.gettimeofday ()))
            ] in

            let json_str = Yojson.Basic.to_string json_obj in

            Lwt_io.write_line log_oc json_str >>= fun () ->
            Lwt_io.flush log_oc >>= fun () ->
            
            Ringbuffer.push ring_buffer json_str;
            
            read_log_stream stream_name ic log_oc ring_buffer

        | None ->
            Printf.printf "[Multiplexer] %s stream closed.\n%!" stream_name;
            Lwt.return_unit

let start folder stdout stderr ring_buffer =
    let open Lwt.Infix in

    let stdout_fd = Lwt_unix.of_unix_file_descr stdout in
    let stderr_fd = Lwt_unix.of_unix_file_descr stderr in

    let stdout_ic = Lwt_io.of_fd ~mode:Lwt_io.Input stdout_fd in
    let stderr_ic = Lwt_io.of_fd ~mode:Lwt_io.Input stderr_fd in

    setup_logger folder >>= fun oc ->

    Printf.printf "[Multiplexer] Attached and listening...\n%!";

    Lwt.join [
        read_log_stream "stdout" stdout_ic oc ring_buffer;
        read_log_stream "stderr" stderr_ic oc ring_buffer;
    ] >>= fun () ->

    Printf.printf "[Multiplexer] Container logging finished. Closing file.\n%!";    
    Lwt_io.close oc >>= fun () ->
    Lwt.return_unit
