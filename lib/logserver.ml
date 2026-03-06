let setup_socket path =
    let open Lwt.Infix in
    if Sys.file_exists path then Sys.remove path;

    let raw_sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Lwt_unix.bind raw_sock (Unix.ADDR_UNIX path) >>= fun () ->
    Lwt_unix.listen raw_sock 10;
    Lwt.return raw_sock

let rec stream_to_client oc ring_buffer client_abs_idx =
    let open Lwt.Infix in
    Lwt.catch (fun () ->
        if client_abs_idx = ring_buffer.Ringbuffer.total_written then begin
            Lwt_condition.wait ring_buffer.Ringbuffer.new_data >>= fun () ->
            stream_to_client oc ring_buffer client_abs_idx
        end else begin
            let actual_idx =
                if ring_buffer.Ringbuffer.total_written - client_abs_idx > ring_buffer.Ringbuffer.capacity then
                    ring_buffer.Ringbuffer.total_written - ring_buffer.Ringbuffer.capacity
                else 
                    client_abs_idx
            in
            let array_idx = actual_idx mod ring_buffer.Ringbuffer.capacity in
            let log_line = ring_buffer.Ringbuffer.buffer.(array_idx) in
            
            Lwt_io.write_line oc log_line >>= fun () ->
            Lwt_io.flush oc >>= fun () ->
            stream_to_client oc ring_buffer (actual_idx + 1)
        end
    )
    (fun _exn ->
        Lwt.return_unit
    )

let start path ring_buffer =
    let open Lwt.Infix in

    setup_socket path >>= fun sock ->

    let rec accept_loop () =
        Lwt_unix.accept sock >>= fun (client_fd, _addr) ->
        
        let oc = Lwt_io.of_fd ~mode:Lwt_io.Output client_fd in

        let start_idx = max 0 (ring_buffer.Ringbuffer.total_written - ring_buffer.Ringbuffer.capacity) in

        Lwt.async (fun () ->
            stream_to_client oc ring_buffer start_idx >>= fun () ->
            Lwt_io.close oc
        );

        accept_loop ()
    in
    accept_loop ()
        



