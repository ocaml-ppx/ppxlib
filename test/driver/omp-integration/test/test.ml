open Ppx_sexp_conv_lib.Conv

type t = int [@@deriving_inline sexp]
let _ = fun (_ : t)  -> ()
let t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = int_of_sexp
let _ = t_of_sexp
let sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = sexp_of_int
let _ = sexp_of_t
[@@@end]

let%expect_test _ =
  Printf.printf "%d\n" [%omp_test];
  [%expect {| 42 |}]
