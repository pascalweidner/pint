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
    path, id

let setup_and_start_container config_path =
    let (folder, id) = setup_container config_path in

    match Unix.fork () with
    | -1 -> failwith "Container could not be created";
    | 0 -> 
        ignore (Unix.execv "/proc/self/exe" [|"pint"; "internal_shim"; id; folder|])
    | pid ->
        Printf.printf "Container %s started in background!\n" id;
        ignore (Unix.waitpid [] pid);
        exit 0
