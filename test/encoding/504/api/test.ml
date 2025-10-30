open Ppxlib_ast

module To_ocaml = Convert (Js) (Compiler_version)
module From_ocaml = Convert (Compiler_version) (Js)

open Ppxlib

#install_printer Pprintast.core_type;;

module Builder = Ast_builder.Make(struct let loc = Location.none end)

let ptyp_int = Builder.(ptyp_constr (Located.mk (Longident.parse "int")) [])
let ptyp_string =
  Builder.(ptyp_constr (Located.mk (Longident.parse "string")) []);;
[%%ignore]

(* Generate an encoded labeled tuple type *)
let encoded_labeled_tuple_type =
  Builder.ptyp_labeled_tuple
    [ Some "a", ptyp_int
    ; Some "b", ptyp_int
    ; None, ptyp_string
    ]

(* Migrate it to the current compiler (>= 5.4, as per dune rules) *)
let labeled_tuple_type = To_ocaml.copy_core_type encoded_labeled_tuple_type;;
[%%ignore]

let as_source =
  Format.asprintf "%a" Astlib.Compiler_pprintast.core_type labeled_tuple_type;;
[%%expect{|
val as_source : string = "(a:int * b:int * string)"
|}]

(* Migrate back to ppxlib's AST *)
let encoded_by_migration = From_ocaml.copy_core_type labeled_tuple_type

let pattern = Ast_pattern.(ptyp_labeled_tuple __);;
[%%ignore]


(* Destruct both the migration and Ast_builder generated encodings with
   the Ast_pattern function. *)
let destruct_from_migration =
  Ast_pattern.parse_res pattern Location.none encoded_by_migration (fun x -> x);;
[%%expect{|
val destruct_from_migration :
  ((string option * core_type) list, Location.Error.t Stdppx.NonEmptyList.t)
  result = Ok [(Some "a", int); (Some "b", int); (None, string)]
|}]

let destruct =
  Ast_pattern.parse_res pattern Location.none
    encoded_labeled_tuple_type (fun x -> x);;
[%%expect{|
val destruct :
  ((string option * core_type) list, Location.Error.t Stdppx.NonEmptyList.t)
  result = Ok [(Some "a", int); (Some "b", int); (None, string)]
|}]
