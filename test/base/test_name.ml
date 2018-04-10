open Base
open Stdio
open Ppxlib

module N = Ppxlib_private.Name

let () =
  print_endline "dot_suffixes:";
  let dot_suffixes name =
    printf "%s\n" (Sexp.to_string_hum
                     (List.sexp_of_t String.sexp_of_t (N.dot_suffixes name)))
  in
  dot_suffixes "foo.bar.baz";
  dot_suffixes "foo.@bar.baz"

let () =
  print_endline "split_path:";
  let split_path name =
    let a, b = N.split_path name in
    printf "%s\n" (Sexp.to_string_hum
                     (List [sexp_of_string a; Option.sexp_of_t sexp_of_string b]))
  in
  split_path "a.b.c";
  split_path "a.b.c.D";
  split_path ".D"
;;
