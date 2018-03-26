open Base

type t = int [@@deriving_inline sexp, compare]
let t_of_sexp = (int_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t)
let sexp_of_t = (sexp_of_int : t -> Ppx_sexp_conv_lib.Sexp.t)
let compare = (compare_int : t -> t -> int)
[@@@end]

type u = int [@@deriving_inline bin_shape { basetype = "foo" }]
let bin_shape_u =
  (Bin_prot.Shape.basetype (Bin_prot.Shape.Uuid.of_string "foo")) []
[@@@end]
