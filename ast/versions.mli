(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                             Frédéric Bour                              *)
(*                   Jérémie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(*$ #use "src/cinaps_helpers" $*)

(** {1 Abstracting an OCaml frontend} *)

(** Abstract view of a version of an OCaml Ast *)
module type Ast = sig
  (*$ foreach_module (fun m types ->
      printf "module %s : sig\n" m;
      List.iter types ~f:(printf "type %s\n");
      printf "end\n"
    )
  *)
  module Parsetree : sig
    type structure
    type signature
    type toplevel_phrase
    type core_type
    type expression
    type pattern
    type case
    type type_declaration
    type type_extension
    type extension_constructor
  end
  (*$*)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end

(* Shortcuts for talking about ast types outside of the module language *)

type 'a _types = 'a constraint 'a
  = <
    (*$ foreach_type (fun _ s -> printf "%-21s : _;\n" s) *)
    structure             : _;
    signature             : _;
    toplevel_phrase       : _;
    core_type             : _;
    expression            : _;
    pattern               : _;
    case                  : _;
    type_declaration      : _;
    type_extension        : _;
    extension_constructor : _;
    (*$*)
  >
;;

(*$ foreach_type (fun _ s ->
    printf "type 'a get_%s = 'x constraint 'a _types = < %s : 'x; .. >\n" s s
  );
  printf ";;\n" *)
type 'a get_structure = 'x constraint 'a _types = < structure : 'x; .. >
type 'a get_signature = 'x constraint 'a _types = < signature : 'x; .. >
type 'a get_toplevel_phrase = 'x constraint 'a _types = < toplevel_phrase : 'x; .. >
type 'a get_core_type = 'x constraint 'a _types = < core_type : 'x; .. >
type 'a get_expression = 'x constraint 'a _types = < expression : 'x; .. >
type 'a get_pattern = 'x constraint 'a _types = < pattern : 'x; .. >
type 'a get_case = 'x constraint 'a _types = < case : 'x; .. >
type 'a get_type_declaration = 'x constraint 'a _types = < type_declaration : 'x; .. >
type 'a get_type_extension = 'x constraint 'a _types = < type_extension : 'x; .. >
type 'a get_extension_constructor = 'x constraint 'a _types = < extension_constructor : 'x; .. >
;;
(*$*)

(** A version of the OCaml frontend packs the ast with type witnesses
    so that equalities can be recovered dynamically. *)
type _ witnesses (*IF_AT_LEAST 406 = private ..*)

(** [migration_info] is an opaque type that is used to generate migration
    functions. *)
type _ migration_info

(** An OCaml frontend versions an Ast, version number and some witnesses for
    conversion. *)
module type OCaml_version = sig

  (** Ast definition for this version *)
  module Ast : Ast

  (* Version number as an integer, 402, 403, 404, ... *)
  val version : int

  (* Version number as a user-friendly string *)
  val string_version : string (* 4.02, 4.03, 4.04, ... *)

  (** Shortcut for talking about Ast types *)
  type types = <
    (*$ foreach_type (fun m s -> printf "%-21s : Ast.%s.%s;\n" s m s) *)
    structure             : Ast.Parsetree.structure;
    signature             : Ast.Parsetree.signature;
    toplevel_phrase       : Ast.Parsetree.toplevel_phrase;
    core_type             : Ast.Parsetree.core_type;
    expression            : Ast.Parsetree.expression;
    pattern               : Ast.Parsetree.pattern;
    case                  : Ast.Parsetree.case;
    type_declaration      : Ast.Parsetree.type_declaration;
    type_extension        : Ast.Parsetree.type_extension;
    extension_constructor : Ast.Parsetree.extension_constructor;
    (*$*)
  > _types

  (** A construtor for recovering type equalities between two arbitrary
      versions. *)
  type _ witnesses += Version : types witnesses

  (** Information used to derive migration functions, see below *)
  val migration_info : types migration_info
end

(** Representing an ocaml version in type language *)
type 'types ocaml_version =
  (module OCaml_version
    (*$ let sep = with_then_and () in
      foreach_type (fun m s ->
          printf "%t type Ast.%s.%s = 'types get_%s\n" sep m s s) *)
    with type Ast.Parsetree.structure = 'types get_structure
     and type Ast.Parsetree.signature = 'types get_signature
     and type Ast.Parsetree.toplevel_phrase = 'types get_toplevel_phrase
     and type Ast.Parsetree.core_type = 'types get_core_type
     and type Ast.Parsetree.expression = 'types get_expression
     and type Ast.Parsetree.pattern = 'types get_pattern
     and type Ast.Parsetree.case = 'types get_case
     and type Ast.Parsetree.type_declaration = 'types get_type_declaration
     and type Ast.Parsetree.type_extension = 'types get_type_extension
     and type Ast.Parsetree.extension_constructor = 'types get_extension_constructor
     (*$*)
  )

(** {1 Concrete frontend instances} *)

(*$foreach_version (fun suffix _ ->
    printf "module OCaml_%s : OCaml_version with module Ast = Migrate_parsetree.Ast_%s\n"
      suffix suffix;
    printf "val ocaml_%s : OCaml_%s.types ocaml_version\n" suffix suffix;
  )*)
module OCaml_402 : OCaml_version with module Ast = Migrate_parsetree.Ast_402
val ocaml_402 : OCaml_402.types ocaml_version
module OCaml_403 : OCaml_version with module Ast = Migrate_parsetree.Ast_403
val ocaml_403 : OCaml_403.types ocaml_version
module OCaml_404 : OCaml_version with module Ast = Migrate_parsetree.Ast_404
val ocaml_404 : OCaml_404.types ocaml_version
module OCaml_405 : OCaml_version with module Ast = Migrate_parsetree.Ast_405
val ocaml_405 : OCaml_405.types ocaml_version
module OCaml_406 : OCaml_version with module Ast = Migrate_parsetree.Ast_406
val ocaml_406 : OCaml_406.types ocaml_version
module OCaml_407 : OCaml_version with module Ast = Migrate_parsetree.Ast_407
val ocaml_407 : OCaml_407.types ocaml_version
module OCaml_408 : OCaml_version with module Ast = Migrate_parsetree.Ast_408
val ocaml_408 : OCaml_408.types ocaml_version
module OCaml_409 : OCaml_version with module Ast = Migrate_parsetree.Ast_409
val ocaml_409 : OCaml_409.types ocaml_version
module OCaml_410 : OCaml_version with module Ast = Migrate_parsetree.Ast_410
val ocaml_410 : OCaml_410.types ocaml_version
module OCaml_411 : OCaml_version with module Ast = Migrate_parsetree.Ast_411
val ocaml_411 : OCaml_411.types ocaml_version
(*$*)

(* An alias to the current compiler version *)
module OCaml_current = OCaml_OCAML_VERSION

(** {1 Convenience definitions} *)

(** Module level migration *)
module Convert (A : OCaml_version) (B : OCaml_version) : sig
  (*$ foreach_type (fun m s ->
      let fq = sprintf "%s.%s" m s in
      printf "  val copy_%-21s : A.Ast.%-31s -> B.Ast.%s\n" s fq fq) *)
  val copy_structure             : A.Ast.Parsetree.structure             -> B.Ast.Parsetree.structure
  val copy_signature             : A.Ast.Parsetree.signature             -> B.Ast.Parsetree.signature
  val copy_toplevel_phrase       : A.Ast.Parsetree.toplevel_phrase       -> B.Ast.Parsetree.toplevel_phrase
  val copy_core_type             : A.Ast.Parsetree.core_type             -> B.Ast.Parsetree.core_type
  val copy_expression            : A.Ast.Parsetree.expression            -> B.Ast.Parsetree.expression
  val copy_pattern               : A.Ast.Parsetree.pattern               -> B.Ast.Parsetree.pattern
  val copy_case                  : A.Ast.Parsetree.case                  -> B.Ast.Parsetree.case
  val copy_type_declaration      : A.Ast.Parsetree.type_declaration      -> B.Ast.Parsetree.type_declaration
  val copy_type_extension        : A.Ast.Parsetree.type_extension        -> B.Ast.Parsetree.type_extension
  val copy_extension_constructor : A.Ast.Parsetree.extension_constructor -> B.Ast.Parsetree.extension_constructor
  (*$*)
end
