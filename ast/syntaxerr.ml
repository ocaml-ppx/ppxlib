(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliary type for reporting syntax errors *)

open Import

type error =
    Unclosed of Location.t * string * Location.t * string
  | Expecting of Location.t * string
  | Not_expecting of Location.t * string
  | Applicative_path of Location.t
  | Variable_in_scope of Location.t * string
  | Other of Location.t
  | Ill_formed_ast of Location.t * string
  | Invalid_package_type of Location.t * string

exception Error of error
exception Escape_error

let make_error ~loc ?(sub = []) msg =
  Selected_ast.Ast.Ast_mapper.make_error_of_message ~loc msg ~sub

let prepare_error = function
  | Unclosed(opening_loc, opening, closing_loc, closing) ->
    make_error
      ~loc:closing_loc
      ~sub:[
        opening_loc, (Printf.sprintf
          "This '%s' might be unmatched" opening)
      ]
      (Printf.sprintf "Syntax error: '%s' expected" closing)
  | Expecting (loc, nonterm) ->
      make_error ~loc (Printf.sprintf "Syntax error: %s expected." nonterm)
  | Not_expecting (loc, nonterm) ->
      make_error ~loc (Printf.sprintf "Syntax error: %s not expected." nonterm)
  | Applicative_path loc ->
      make_error ~loc
        "Syntax error: applicative paths of the form F(X).t \
         are not supported when the option -no-app-func is set."
  | Variable_in_scope (loc, var) ->
      make_error ~loc
        (Printf.sprintf "In this scoped type, variable '%s \
         is reserved for the local type %s."
         var var)
  | Other loc ->
      make_error ~loc "Syntax error"
  | Ill_formed_ast (loc, s) ->
      make_error ~loc (Printf.sprintf "broken invariant in parsetree: %s" s)
  | Invalid_package_type (loc, s) ->
      make_error ~loc (Printf.sprintf "invalid package type: %s" s)

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (prepare_error err)
      | _ -> None
    )


let report_error ppf err =
  Selected_ast.Ast.Ast_mapper.print_error ppf (prepare_error err)

let location_of_error = function
  | Unclosed(l,_,_,_)
  | Applicative_path l
  | Variable_in_scope(l,_)
  | Other l
  | Not_expecting (l, _)
  | Ill_formed_ast (l, _)
  | Invalid_package_type (l, _)
  | Expecting (l, _) -> l


let ill_formed_ast loc s =
  raise (Error (Ill_formed_ast (loc, s)))
