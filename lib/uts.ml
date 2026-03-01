open Ctypes
open Foreign

module UtsRaw = struct
    let sethostname = foreign "sethostname" ~check_errno:true (string @-> size_t @-> returning int)

    let setdomainname = foreign "setdomainname" ~check_errno:true (string @-> size_t @-> returning int)
end

let setup_uts_ns hostname domainname = 
    let open UtsRaw in
    let len = Unsigned.Size_t.of_int (String.length hostname) in
    let len2 = Unsigned.Size_t.of_int (String.length domainname) in
    ignore (sethostname hostname len);
    ignore (setdomainname domainname len2)



