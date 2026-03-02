open Ctypes
open Foreign

module MountRaw = struct
    let msbind = 4096
    let msrec = 16384
    let msprivate = 1 lsl 18
    let msnosuid = 2
    let msnoexec = 8
    let msrdonly = 1

    let mount = foreign "mount" ~check_errno:true 
        (string @-> string @-> string @-> ulong @-> ptr void 
        @-> returning int)

    let mntdetach = 2

    let umount2 = foreign "umount2" ~check_errno:true
        (string @-> int @-> returning int)
end

let mount_private target = let open MountRaw in
    let flags = Unsigned.ULong.of_int (msprivate lor msrec) in
    mount "none" target "" flags null

let mount_bind target = let open MountRaw in
    let flags = Unsigned.ULong.of_int (msbind lor msrec) in
    mount target target "" flags null

let mount_proc () = 
    let open MountRaw in
    let flags = Unsigned.ULong.zero in

    match mount "proc" "/proc" "proc" flags null with
    | 0 -> ()
    | _ -> prerr_endline "[Warning] Failed to mount /proc"

let mount_sys new_root = 
    let open MountRaw in
    let flags = Unsigned.ULong.zero in
    (*TODO: create the sys folder if it doesn't exist*)

    match mount "sysfs" (new_root ^ "/sys") "sysfs" flags null with
    | 0 -> ()
    | _ -> prerr_endline "[Warning] Failed to mount /sys"

let mount_cgroup new_root =
    let open MountRaw in
    let flags = Unsigned.ULong.of_int msrdonly in

    begin
        try 
            Unix.mkdir (new_root ^ "/sys/fs/cgroup") 0o755    
        with Unix.Unix_error (Unix.EEXIST, _, _) ->
            Printf.eprintf "[Info] /sys/fs/cgroup already exists. Will not be created.\n%!"
    end;

    match mount "cgroup2" (new_root ^ "/sys/fs/cgroup") "cgroup2" flags null with
    | 0 -> ()
    | _ -> prerr_endline "[Warning] Failed to mount /sys/fs/cgroup"

let mount_dev new_root =
    let open MountRaw in

    let flags = Unsigned.ULong.of_int (msnosuid lor msnoexec) in
    let flags2 = Unsigned.ULong.of_int (msbind lor msrec) in

    (match mount "tmpfs" (new_root ^ "/dev") "tmpfs" flags null with
    | 0 -> ()
    | _ -> prerr_endline "[Warning] Failed to mount /dev");

    let dev_null = new_root ^ "/dev/null" in
    let dev_zero = new_root ^ "/dev/zero" in
    let dev_random = new_root ^ "/dev/random" in
    let dev_urandom = new_root ^ "/dev/urandom" in

    let touch path =
        let fd = Unix.openfile path Unix.[O_CREAT; O_EXCL; O_WRONLY] 0o666 in
        Unix.close fd
    in
    touch dev_null;
    touch dev_zero;
    touch dev_random;
    touch dev_urandom;

    ignore (mount "/dev/null" dev_null "" flags2 null); 
    ignore (mount "/dev/zero" dev_zero "" flags2 null); 
    ignore (mount "/dev/random" dev_random "" flags2 null); 
    ignore (mount "/dev/urandom" dev_urandom "" flags2 null); 

    Unix.symlink "/proc/self/fd" (new_root ^ "/dev/fd");
    Unix.symlink "/proc/self/fd/0" (new_root ^ "/dev/stdin");
    Unix.symlink "/proc/self/fd/1" (new_root ^ "/dev/stdout");
    Unix.symlink "/proc/self/fd/2" (new_root ^ "/dev/stderr")



module PivotRaw = struct
    let syspivotroot = 155

    let syscall = foreign "syscall" ~check_errno:true 
        (int @-> string @-> string @-> returning int)
end

let pivot_root = let open PivotRaw in
    syscall syspivotroot



let setup_mount_ns new_root =
    try begin
        ignore (mount_private "/");
        ignore (mount_bind new_root);
        ignore (mount_dev new_root);
        mount_sys new_root;
        mount_cgroup new_root;

        let resolv_conf_path = new_root ^ "/etc/resolv.conf" in
        let out_ch = open_out resolv_conf_path in
        output_string out_ch "nameserver 8.8.8.8\n";
        close_out out_ch;

        Unix.chdir new_root;

        let put_old_dir = "put_old" in
        Unix.mkdir put_old_dir 0o777;

        ignore( pivot_root "." put_old_dir);

        Unix.chdir "/";

        let absolute_put_old = "/put_old" in
        let open MountRaw in
        ignore (umount2 absolute_put_old mntdetach);
        Unix.rmdir absolute_put_old;

        mount_proc ();
    end
    with
        | Unix.Unix_error (err, syscall_name, _) ->
            let msg = Unix.error_message err in
            prerr_endline ("Mount namespace setup failed at " 
                ^ syscall_name ^ ": " ^ msg);
            raise (Unix.Unix_error (err, syscall_name, ""))