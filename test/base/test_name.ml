open Ppxlib_base
open Ppx_core

module N = Ppx_core_private.Name

let%expect_test _ =
  let dot_suffixes name =
    printf "%s" (Sexp.to_string_hum
                   (List.sexp_of_t String.sexp_of_t (N.dot_suffixes name)))
  in
  dot_suffixes "foo.bar.baz";
  [%expect {| (baz bar.baz foo.bar.baz) |}];
  dot_suffixes "foo.@bar.baz";
  [%expect {| (bar.baz foo.bar.baz) |}]

let%expect_test _ =
  let split_path name =
    let a, b = N.split_path name in
    printf "%s" (Sexp.to_string_hum
                   (List [sexp_of_string a; Option.sexp_of_t sexp_of_string b]))
  in
  split_path "a.b.c";
  [%expect {| (a.b.c ()) |}];
  split_path "a.b.c.D";
  [%expect {| (a.b.c (D)) |}];
  split_path ".D";
  [%expect {| ("" (D)) |}];
;;
