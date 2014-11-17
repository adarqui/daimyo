let try_finalize f x finally y =
    let res = try f x with exn -> finally y; raise exn in finally y;
    res

let open_something arg =
    let buf = Bytes.create 129 in
    let fd = Unix.openfile arg [Unix.O_RDONLY] 0 in
    let bs = Unix.read fd buf 0 128 in
    print_bytes buf

let cleanup arg =
    print_string "CLEANUP!"

let example arg =
    try_finalize open_something arg cleanup ()
