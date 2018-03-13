open Base

type t = int [@@deriving_inline sexp, compare]
val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t
val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t
val compare : t -> t -> int
[@@@end]

type u = int [@@deriving_inline bin_shape]
val bin_shape_u : Bin_prot.Shape.t
[@@@end]
