open Base

type t = int [@@deriving_inline sexp, compare]
let t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = int_of_sexp
let sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = sexp_of_int
let compare : t -> t -> int = compare_int
[@@@end]

type u = int [@@deriving_inline bin_shape { basetype = "foo" }]
let bin_shape_u =
  (Bin_prot.Shape.basetype (Bin_prot.Shape.Uuid.of_string "foo")) []
[@@@end]
