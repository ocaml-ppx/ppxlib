type t = int [@@deriving sexp]

let f x = x + 1

let%test _ = f 1 = 2
let%test _ = f 2 = 3

let () = ()
let () = print_s [%sexp { x = 1 }]; ()
