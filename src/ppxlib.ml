(** Standard library for ppx rewriters *)

(** Make sure code using Ppxlib doesn't refer to compiler-libs without being explicit
    about it *)
include struct
  [@@@warning "-3"]
  open Ocaml_shadow

  include (Ocaml_shadow : module type of struct include Ocaml_shadow end
           with module Ast_helper   := Ast_helper
           with module Asttypes     := Asttypes
           with module Docstrings   := Docstrings
           with module Identifiable := Identifiable
           with module Lexer        := Lexer
           with module Location     := Location
           with module Longident    := Longident
           with module Parse        := Parse
           with module Parser       := Parser
           with module Parsetree    := Parsetree
           with module Pprintast    := Pprintast
           with module Syntaxerr    := Syntaxerr
          )
end (** @inline *)

(** Expose all modules from Ppxlib_ast;
    in particular, overwrite some of the modules above *)
module type OCaml_version = Ppxlib_ast.OCaml_version

module Ast                = Ppxlib_ast.Ast
module Ast_helper         = Ppxlib_ast.Ast_helper
module Ast_magic          = Ppxlib_ast.Ast_magic
module Asttypes           = Ppxlib_ast.Asttypes
module Compiler_version   = Ppxlib_ast.Compiler_version
module Js                 = Ppxlib_ast.Js
module Find_version       = Ppxlib_ast.Find_version
module Convert            = Ppxlib_ast.Convert
module Extra_warnings     = Ppxlib_ast.Extra_warnings
module Location_error     = Ppxlib_ast.Location_error
module Parse              = Ppxlib_ast.Parse
module Parser             = Ppxlib_ast.Parser
module Parsetree          = Ppxlib_ast.Parsetree
module Pprintast          = Ppxlib_ast.Pprintast
module Select_ast         = Ppxlib_ast.Select_ast
module Selected_ast       = Ppxlib_ast.Selected_ast
module Syntaxerr          = Ppxlib_ast.Syntaxerr
module Import_for_core    = Ppxlib_ast.Import_for_core

(** Include all the Ast definitions since we need them in every single ppx *)
include Ast

module Ast_builder         = Ast_builder
module Ast_pattern         = Ast_pattern
module Ast_traverse        = Ast_traverse
module Attribute           = Attribute
module Code_path           = Code_path
module Caller_id           = Caller_id
module Context_free        = Context_free
module Deriving            = Deriving
module Driver              = Driver
module Expansion_context   = Expansion_context
module Extension           = Extension
module File_path           = File_path
module Keyword             = Keyword
module Loc                 = Loc
module Location            = Location
module Longident           = Longident
module Merlin_helpers      = Merlin_helpers
module Reserved_namespaces = Name.Reserved_namespaces
module Spellcheck          = Spellcheck
module Quoter              = Quoter

include Common

(**/**)

(* For tests and Ppx_core compatibility layer *)
module Ppxlib_private = struct
  module Common = Common
  module Name   = Name
end
