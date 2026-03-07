let setup_socket path =
    let open Lwt.Infix in
    if Sys.file_exists path then Sys.remove path;

    let raw_sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Lwt_unix.bind raw_sock (Unix.ADDR_UNIX path) >>= fun () ->
    Lwt_unix.listen raw_sock 10;
    Lwt.return raw_sock


let rec stream_from_client ic stdin_oc = 
    let open Lwt.Infix in
    Lwt.catch (fun () ->
        Lwt_io.read_char_opt ic >>= function
        | Some char ->
            Lwt_io.write_char stdin_oc char >>= fun () ->
            Lwt_io.flush stdin_oc >>= fun () ->
            stream_from_client ic stdin_oc
        | None ->
            Lwt.return_unit
    ) (fun _exn ->
        Lwt.return_unit
    )

let start path stdin =
    let open Lwt.Infix in

    let stdin_fd = Lwt_unix.of_unix_file_descr ~set_flags:false stdin in
    let stdin_oc = Lwt_io.of_fd ~mode:Lwt_io.Output stdin_fd in

    setup_socket path >>= fun sock ->
    
    let rec accept_loop () =
        Lwt_unix.accept sock >>= fun (client_fd, _addr) ->

        let ic = Lwt_io.of_fd ~mode:Lwt_io.Input client_fd in
        
        Lwt.async (fun () -> 
            stream_from_client ic stdin_oc >>= fun () ->
            Lwt_io.close ic
        );

        accept_loop ()
    in
    accept_loop ()

