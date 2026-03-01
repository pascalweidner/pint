open Ctypes
open Foreign

module Raw = struct
    let newns = 0x00020000
    let newuts = 0x04000000
    let newipc = 0x08000000
    let newuser = 0x10000000
    let newpid = 0x20000000
    let newnet = 0x40000000
    let newcgroup = 0x02000000

    let unshare = foreign "unshare" ~check_errno:true (int @-> returning int) 
end

type clone_flag = NEWNS | NEWUTS | NEWIPC | NEWUSER | NEWPID | NEWNET | NEWCGROUP

let flag_to_int flag = 
    let open Raw in match flag with 
    | NEWNS -> newns
    | NEWUTS -> newuts
    | NEWIPC -> newipc
    | NEWUSER -> newuser
    | NEWPID -> newpid
    | NEWNET -> newnet
    | NEWCGROUP -> newcgroup
    

let namespaces flags =
    let bitmask = List.fold_left (fun acc flag -> acc lor (flag_to_int flag)) 0 flags in
    try
        ignore (Raw.unshare bitmask)
    with
        | Unix.Unix_error (err, _, _) ->
            let msg = Unix.error_message err in
            prerr_endline ("Container isolation failed: " ^ msg);
            raise (Unix.Unix_error (err, "unshare", ""))


