(**************************************************************************)
(*                                                                        *)
(*                         OCaml Migrate Parsetree                        *)
(*                                                                        *)
(*                         Frédéric Bour, Facebook                        *)
(*            Jérémie Dimino and Leo White, Jane Street Europe            *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                         Alain Frisch, LexiFi                           *)
(*       Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique (INRIA).                                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* TODO: remove this open *)
open Stdlib0
open Ocaml_common

module Location = Location
module Longident = Longident

include Migrate_parsetree.Ast_410

[@@@warning "-9"]
module Ast_helper : sig
  open Asttypes
  open Docstrings
  open Parsetree

  type 'a with_loc = 'a Location.loc
  type loc = Location.t

  type lid = Longident.t with_loc
  type str = string with_loc
  type str_opt = string option with_loc
  type attrs = attribute list

  (** {1 Default locations} *)

  val default_loc: loc ref
  (** Default value for all optional location arguments. *)

  val with_default_loc: loc -> (unit -> 'a) -> 'a
  (** Set the [default_loc] within the scope of the execution
      of the provided function. *)

  (** {1 Constants} *)

  module Const : sig
    val char : char -> constant
    val string : ?quotation_delimiter:string -> string -> constant
    val integer : ?suffix:char -> string -> constant
    val int : ?suffix:char -> int -> constant
    val int32 : ?suffix:char -> int32 -> constant
    val int64 : ?suffix:char -> int64 -> constant
    val nativeint : ?suffix:char -> nativeint -> constant
    val float : ?suffix:char -> string -> constant
  end

  (** {1 Attributes} *)
  module Attr : sig
    val mk: ?loc:loc -> str -> payload -> attribute
  end

  (** {1 Core language} *)

  (** Type expressions *)
  module Typ :
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> core_type_desc -> core_type
    val attr: core_type -> attribute -> core_type

    val any: ?loc:loc -> ?attrs:attrs -> unit -> core_type
    val var: ?loc:loc -> ?attrs:attrs -> string -> core_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type -> core_type
      -> core_type
    val tuple: ?loc:loc -> ?attrs:attrs -> core_type list -> core_type
    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val object_: ?loc:loc -> ?attrs:attrs -> object_field list
      -> closed_flag -> core_type
    val class_: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> core_type
    val alias: ?loc:loc -> ?attrs:attrs -> core_type -> string -> core_type
    val variant: ?loc:loc -> ?attrs:attrs -> row_field list -> closed_flag
      -> label list option -> core_type
    val poly: ?loc:loc -> ?attrs:attrs -> str list -> core_type -> core_type
    val package: ?loc:loc -> ?attrs:attrs -> lid -> (lid * core_type) list
      -> core_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> core_type

    val force_poly: core_type -> core_type

    val varify_constructors: str list -> core_type -> core_type
    (** [varify_constructors newtypes te] is type expression [te], of which
        any of nullary type constructor [tc] is replaced by type variable of
        the same name, if [tc]'s name appears in [newtypes].
        Raise [Syntaxerr.Variable_in_scope] if any type variable inside [te]
        appears in [newtypes].
        @since 4.05
    *)
  end

  (** Patterns *)
  module Pat:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> pattern_desc -> pattern
    val attr:pattern -> attribute -> pattern

    val any: ?loc:loc -> ?attrs:attrs -> unit -> pattern
    val var: ?loc:loc -> ?attrs:attrs -> str -> pattern
    val alias: ?loc:loc -> ?attrs:attrs -> pattern -> str -> pattern
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> pattern
    val interval: ?loc:loc -> ?attrs:attrs -> constant -> constant -> pattern
    val tuple: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> pattern option -> pattern
    val variant: ?loc:loc -> ?attrs:attrs -> label -> pattern option -> pattern
    val record: ?loc:loc -> ?attrs:attrs -> (lid * pattern) list -> closed_flag
      -> pattern
    val array: ?loc:loc -> ?attrs:attrs -> pattern list -> pattern
    val or_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern -> pattern
    val constraint_: ?loc:loc -> ?attrs:attrs -> pattern -> core_type -> pattern
    val type_: ?loc:loc -> ?attrs:attrs -> lid -> pattern
    val lazy_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val unpack: ?loc:loc -> ?attrs:attrs -> str_opt -> pattern
    val open_: ?loc:loc -> ?attrs:attrs  -> lid -> pattern -> pattern
    val exception_: ?loc:loc -> ?attrs:attrs -> pattern -> pattern
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> pattern
  end

  (** Expressions *)
  module Exp:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> expression_desc -> expression
    val attr: expression -> attribute -> expression

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val constant: ?loc:loc -> ?attrs:attrs -> constant -> expression
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list
      -> expression -> expression
    val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option
      -> pattern -> expression -> expression
    val function_: ?loc:loc -> ?attrs:attrs -> case list -> expression
    val apply: ?loc:loc -> ?attrs:attrs -> expression
      -> (arg_label * expression) list -> expression
    val match_: ?loc:loc -> ?attrs:attrs -> expression -> case list
      -> expression
    val try_: ?loc:loc -> ?attrs:attrs -> expression -> case list -> expression
    val tuple: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val construct: ?loc:loc -> ?attrs:attrs -> lid -> expression option
      -> expression
    val variant: ?loc:loc -> ?attrs:attrs -> label -> expression option
      -> expression
    val record: ?loc:loc -> ?attrs:attrs -> (lid * expression) list
      -> expression option -> expression
    val field: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
    val setfield: ?loc:loc -> ?attrs:attrs -> expression -> lid -> expression
      -> expression
    val array: ?loc:loc -> ?attrs:attrs -> expression list -> expression
    val ifthenelse: ?loc:loc -> ?attrs:attrs -> expression -> expression
      -> expression option -> expression
    val sequence: ?loc:loc -> ?attrs:attrs -> expression -> expression
      -> expression
    val while_: ?loc:loc -> ?attrs:attrs -> expression -> expression
      -> expression
    val for_: ?loc:loc -> ?attrs:attrs -> pattern -> expression -> expression
      -> direction_flag -> expression -> expression
    val coerce: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
      -> core_type -> expression
    val constraint_: ?loc:loc -> ?attrs:attrs -> expression -> core_type
      -> expression
    val send: ?loc:loc -> ?attrs:attrs -> expression -> str -> expression
    val new_: ?loc:loc -> ?attrs:attrs -> lid -> expression
    val setinstvar: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
    val override: ?loc:loc -> ?attrs:attrs -> (str * expression) list
      -> expression
    val letmodule: ?loc:loc -> ?attrs:attrs -> str_opt -> module_expr
      -> expression -> expression
    val letexception:
      ?loc:loc -> ?attrs:attrs -> extension_constructor -> expression
      -> expression
    val assert_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val lazy_: ?loc:loc -> ?attrs:attrs -> expression -> expression
    val poly: ?loc:loc -> ?attrs:attrs -> expression -> core_type option
      -> expression
    val object_: ?loc:loc -> ?attrs:attrs -> class_structure -> expression
    val newtype: ?loc:loc -> ?attrs:attrs -> str -> expression -> expression
    val pack: ?loc:loc -> ?attrs:attrs -> module_expr -> expression
    val open_: ?loc:loc -> ?attrs:attrs -> open_declaration -> expression
      -> expression
    val letop: ?loc:loc -> ?attrs:attrs -> binding_op
      -> binding_op list -> expression -> expression
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> expression
    val unreachable: ?loc:loc -> ?attrs:attrs -> unit -> expression

    val case: pattern -> ?guard:expression -> expression -> case
    val binding_op: str -> pattern -> expression -> loc -> binding_op
  end

  (** Value declarations *)
  module Val:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?prim:string list -> str -> core_type -> value_description
  end

  (** Type declarations *)
  module Type:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?params:(core_type * variance) list ->
      ?cstrs:(core_type * core_type * loc) list ->
      ?kind:type_kind -> ?priv:private_flag -> ?manifest:core_type -> str ->
      type_declaration

    val constructor: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?args:constructor_arguments -> ?res:core_type -> str ->
      constructor_declaration
    val field: ?loc:loc -> ?attrs:attrs -> ?info:info ->
      ?mut:mutable_flag -> str -> core_type -> label_declaration
  end

  (** Type extensions *)
  module Te:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      ?params:(core_type * variance) list -> ?priv:private_flag ->
      lid -> extension_constructor list -> type_extension

    val mk_exception: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      extension_constructor -> type_exception

    val constructor: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> extension_constructor_kind -> extension_constructor

    val decl: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      ?args:constructor_arguments -> ?res:core_type -> str ->
      extension_constructor
    val rebind: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?info:info ->
      str -> lid -> extension_constructor
  end

  (** {1 Module language} *)

  (** Module type expressions *)
  module Mty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_type_desc -> module_type
    val attr: module_type -> attribute -> module_type

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val alias: ?loc:loc -> ?attrs:attrs -> lid -> module_type
    val signature: ?loc:loc -> ?attrs:attrs -> signature -> module_type
    val functor_: ?loc:loc -> ?attrs:attrs ->
      functor_parameter -> module_type -> module_type
    val with_: ?loc:loc -> ?attrs:attrs -> module_type ->
      with_constraint list -> module_type
    val typeof_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_type
  end

  (** Module expressions *)
  module Mod:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> module_expr_desc -> module_expr
    val attr: module_expr -> attribute -> module_expr

    val ident: ?loc:loc -> ?attrs:attrs -> lid -> module_expr
    val structure: ?loc:loc -> ?attrs:attrs -> structure -> module_expr
    val functor_: ?loc:loc -> ?attrs:attrs ->
      functor_parameter -> module_expr -> module_expr
    val apply: ?loc:loc -> ?attrs:attrs -> module_expr -> module_expr ->
      module_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> module_expr -> module_type ->
      module_expr
    val unpack: ?loc:loc -> ?attrs:attrs -> expression -> module_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> module_expr
  end

  (** Signature items *)
  module Sig:
  sig
    val mk: ?loc:loc -> signature_item_desc -> signature_item

    val value: ?loc:loc -> value_description -> signature_item
    val type_: ?loc:loc -> rec_flag -> type_declaration list -> signature_item
    val type_subst: ?loc:loc -> type_declaration list -> signature_item
    val type_extension: ?loc:loc -> type_extension -> signature_item
    val exception_: ?loc:loc -> type_exception -> signature_item
    val module_: ?loc:loc -> module_declaration -> signature_item
    val mod_subst: ?loc:loc -> module_substitution -> signature_item
    val rec_module: ?loc:loc -> module_declaration list -> signature_item
    val modtype: ?loc:loc -> module_type_declaration -> signature_item
    val open_: ?loc:loc -> open_description -> signature_item
    val include_: ?loc:loc -> include_description -> signature_item
    val class_: ?loc:loc -> class_description list -> signature_item
    val class_type: ?loc:loc -> class_type_declaration list -> signature_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> signature_item
    val attribute: ?loc:loc -> attribute -> signature_item
    val text: text -> signature_item list
  end

  (** Structure items *)
  module Str:
  sig
    val mk: ?loc:loc -> structure_item_desc -> structure_item

    val eval: ?loc:loc -> ?attrs:attributes -> expression -> structure_item
    val value: ?loc:loc -> rec_flag -> value_binding list -> structure_item
    val primitive: ?loc:loc -> value_description -> structure_item
    val type_: ?loc:loc -> rec_flag -> type_declaration list -> structure_item
    val type_extension: ?loc:loc -> type_extension -> structure_item
    val exception_: ?loc:loc -> type_exception -> structure_item
    val module_: ?loc:loc -> module_binding -> structure_item
    val rec_module: ?loc:loc -> module_binding list -> structure_item
    val modtype: ?loc:loc -> module_type_declaration -> structure_item
    val open_: ?loc:loc -> open_declaration -> structure_item
    val class_: ?loc:loc -> class_declaration list -> structure_item
    val class_type: ?loc:loc -> class_type_declaration list -> structure_item
    val include_: ?loc:loc -> include_declaration -> structure_item
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> structure_item
    val attribute: ?loc:loc -> attribute -> structure_item
    val text: text -> structure_item list
  end

  (** Module declarations *)
  module Md:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_opt -> module_type -> module_declaration
  end

  (** Module substitutions *)
  module Ms:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str -> lid -> module_substitution
  end

  (** Module type declarations *)
  module Mtd:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?typ:module_type -> str -> module_type_declaration
  end

  (** Module bindings *)
  module Mb:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      str_opt -> module_expr -> module_binding
  end

  (** Opens *)
  module Opn:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs ->
      ?override:override_flag -> 'a -> 'a open_infos
  end

  (** Includes *)
  module Incl:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> 'a -> 'a include_infos
  end

  (** Value bindings *)
  module Vb:
  sig
    val mk: ?loc: loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      pattern -> expression -> value_binding
  end


  (** {1 Class language} *)

  (** Class type expressions *)
  module Cty:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_type_desc -> class_type
    val attr: class_type -> attribute -> class_type

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_type
    val signature: ?loc:loc -> ?attrs:attrs -> class_signature -> class_type
    val arrow: ?loc:loc -> ?attrs:attrs -> arg_label -> core_type ->
      class_type -> class_type
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_type
      -> class_type
  end

  (** Class type fields *)
  module Ctf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs ->
      class_type_field_desc -> class_type_field
    val attr: class_type_field -> attribute -> class_type_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> class_type -> class_type_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
      virtual_flag -> core_type -> class_type_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
      virtual_flag -> core_type -> class_type_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_type_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_type_field
    val attribute: ?loc:loc -> attribute -> class_type_field
    val text: text -> class_type_field list
  end

  (** Class expressions *)
  module Cl:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> class_expr_desc -> class_expr
    val attr: class_expr -> attribute -> class_expr

    val constr: ?loc:loc -> ?attrs:attrs -> lid -> core_type list -> class_expr
    val structure: ?loc:loc -> ?attrs:attrs -> class_structure -> class_expr
    val fun_: ?loc:loc -> ?attrs:attrs -> arg_label -> expression option ->
      pattern -> class_expr -> class_expr
    val apply: ?loc:loc -> ?attrs:attrs -> class_expr ->
      (arg_label * expression) list -> class_expr
    val let_: ?loc:loc -> ?attrs:attrs -> rec_flag -> value_binding list ->
      class_expr -> class_expr
    val constraint_: ?loc:loc -> ?attrs:attrs -> class_expr -> class_type ->
      class_expr
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_expr
    val open_: ?loc:loc -> ?attrs:attrs -> open_description -> class_expr
      -> class_expr
  end

  (** Class fields *)
  module Cf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> class_field_desc ->
      class_field
    val attr: class_field -> attribute -> class_field

    val inherit_: ?loc:loc -> ?attrs:attrs -> override_flag -> class_expr ->
      str option -> class_field
    val val_: ?loc:loc -> ?attrs:attrs -> str -> mutable_flag ->
      class_field_kind -> class_field
    val method_: ?loc:loc -> ?attrs:attrs -> str -> private_flag ->
      class_field_kind -> class_field
    val constraint_: ?loc:loc -> ?attrs:attrs -> core_type -> core_type ->
      class_field
    val initializer_: ?loc:loc -> ?attrs:attrs -> expression -> class_field
    val extension: ?loc:loc -> ?attrs:attrs -> extension -> class_field
    val attribute: ?loc:loc -> attribute -> class_field
    val text: text -> class_field list

    val virtual_: core_type -> class_field_kind
    val concrete: override_flag -> expression -> class_field_kind

  end

  (** Classes *)
  module Ci:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> ?docs:docs -> ?text:text ->
      ?virt:virtual_flag -> ?params:(core_type * variance) list ->
      str -> 'a -> 'a class_infos
  end

  (** Class signatures *)
  module Csig:
  sig
    val mk: core_type -> class_type_field list -> class_signature
  end

  (** Class structures *)
  module Cstr:
  sig
    val mk: pattern -> class_field list -> class_structure
  end

  (** Row fields *)
  module Rf:
  sig
    val mk: ?loc:loc -> ?attrs:attrs -> row_field_desc -> row_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label with_loc -> bool -> core_type list -> row_field
    val inherit_: ?loc:loc -> core_type -> row_field
  end

  (** Object fields *)
  module Of:
  sig
    val mk: ?loc:loc -> ?attrs:attrs ->
      object_field_desc -> object_field
    val tag: ?loc:loc -> ?attrs:attrs ->
      label with_loc -> core_type -> object_field
    val inherit_: ?loc:loc -> core_type -> object_field
  end

end = struct
  open Asttypes
  open Parsetree
  open Docstrings

  type 'a with_loc = 'a Location.loc
  type loc = Location.t

  type lid = Longident.t with_loc
  type str = string with_loc
  type str_opt = string option with_loc
  type attrs = attribute list

  let default_loc = ref Location.none

  let with_default_loc l f =
    Misc.protect_refs [Misc.R (default_loc, l)] f

  module Const = struct
    let integer ?suffix i = Pconst_integer (i, suffix)
    let int ?suffix i = integer ?suffix (Int.to_string i)
    let int32 ?(suffix='l') i = integer ~suffix (Int32.to_string i)
    let int64 ?(suffix='L') i = integer ~suffix (Int64.to_string i)
    let nativeint ?(suffix='n') i = integer ~suffix (Nativeint.to_string i)
    let float ?suffix f = Pconst_float (f, suffix)
    let char c = Pconst_char c
    let string ?quotation_delimiter s = Pconst_string (s, quotation_delimiter)
  end

  module Attr = struct
    let mk ?(loc= !default_loc) name payload =
      { attr_name = name;
        attr_payload = payload;
        attr_loc = loc }
  end

  module Typ = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ptyp_desc = d;
       ptyp_loc = loc;
       ptyp_loc_stack = [];
       ptyp_attributes = attrs}

    let attr d a = {d with ptyp_attributes = d.ptyp_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ptyp_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ptyp_var a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_arrow (a, b, c))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ptyp_tuple a)
    let constr ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_constr (a, b))
    let object_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_object (a, b))
    let class_ ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_class (a, b))
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_alias (a, b))
    let variant ?loc ?attrs a b c = mk ?loc ?attrs (Ptyp_variant (a, b, c))
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_poly (a, b))
    let package ?loc ?attrs a b = mk ?loc ?attrs (Ptyp_package (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ptyp_extension a)

    let force_poly t =
      match t.ptyp_desc with
      | Ptyp_poly _ -> t
      | _ -> poly ~loc:t.ptyp_loc [] t (* -> ghost? *)

    let varify_constructors var_names t =
      let check_variable vl loc v =
        if List.mem v vl then
          raise Syntaxerr.(Error(Variable_in_scope(loc,v))) in
      let var_names = List.map (fun v -> v.txt) var_names in
      let rec loop t =
        let desc =
          match t.ptyp_desc with
          | Ptyp_any -> Ptyp_any
          | Ptyp_var x ->
              check_variable var_names t.ptyp_loc x;
              Ptyp_var x
          | Ptyp_arrow (label,core_type,core_type') ->
              Ptyp_arrow(label, loop core_type, loop core_type')
          | Ptyp_tuple lst -> Ptyp_tuple (List.map loop lst)
          | Ptyp_constr( { txt = Longident.Lident s }, [])
            when List.mem s var_names ->
              Ptyp_var s
          | Ptyp_constr(longident, lst) ->
              Ptyp_constr(longident, List.map loop lst)
          | Ptyp_object (lst, o) ->
              Ptyp_object (List.map loop_object_field lst, o)
          | Ptyp_class (longident, lst) ->
              Ptyp_class (longident, List.map loop lst)
          | Ptyp_alias(core_type, string) ->
              check_variable var_names t.ptyp_loc string;
              Ptyp_alias(loop core_type, string)
          | Ptyp_variant(row_field_list, flag, lbl_lst_option) ->
              Ptyp_variant(List.map loop_row_field row_field_list,
                           flag, lbl_lst_option)
          | Ptyp_poly(string_lst, core_type) ->
              List.iter (fun v ->
                  check_variable var_names t.ptyp_loc v.txt) string_lst;
              Ptyp_poly(string_lst, loop core_type)
          | Ptyp_package(longident,lst) ->
              Ptyp_package(longident,List.map (fun (n,typ) -> (n,loop typ) ) lst)
          | Ptyp_extension (s, arg) ->
              Ptyp_extension (s, arg)
        in
        {t with ptyp_desc = desc}
      and loop_row_field field =
        let prf_desc = match field.prf_desc with
          | Rtag(label,flag,lst) ->
              Rtag(label,flag,List.map loop lst)
          | Rinherit t ->
              Rinherit (loop t)
        in
        { field with prf_desc; }
      and loop_object_field field =
        let pof_desc = match field.pof_desc with
          | Otag(label, t) ->
              Otag(label, loop t)
          | Oinherit t ->
              Oinherit (loop t)
        in
        { field with pof_desc; }
      in
      loop t

  end

  module Pat = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {ppat_desc = d;
       ppat_loc = loc;
       ppat_loc_stack = [];
       ppat_attributes = attrs}
    let attr d a = {d with ppat_attributes = d.ppat_attributes @ [a]}

    let any ?loc ?attrs () = mk ?loc ?attrs Ppat_any
    let var ?loc ?attrs a = mk ?loc ?attrs (Ppat_var a)
    let alias ?loc ?attrs a b = mk ?loc ?attrs (Ppat_alias (a, b))
    let constant ?loc ?attrs a = mk ?loc ?attrs (Ppat_constant a)
    let interval ?loc ?attrs a b = mk ?loc ?attrs (Ppat_interval (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Ppat_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Ppat_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Ppat_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Ppat_record (a, b))
    let array ?loc ?attrs a = mk ?loc ?attrs (Ppat_array a)
    let or_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_or (a, b))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_constraint (a, b))
    let type_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_type a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_lazy a)
    let unpack ?loc ?attrs a = mk ?loc ?attrs (Ppat_unpack a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Ppat_open (a, b))
    let exception_ ?loc ?attrs a = mk ?loc ?attrs (Ppat_exception a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Ppat_extension a)
  end

  module Exp = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pexp_desc = d;
       pexp_loc = loc;
       pexp_loc_stack = [];
       pexp_attributes = attrs}
    let attr d a = {d with pexp_attributes = d.pexp_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pexp_ident a)
    let constant ?loc ?attrs a = mk ?loc ?attrs (Pexp_constant a)
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pexp_fun (a, b, c, d))
    let function_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_function a)
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pexp_apply (a, b))
    let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
    let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))
    let tuple ?loc ?attrs a = mk ?loc ?attrs (Pexp_tuple a)
    let construct ?loc ?attrs a b = mk ?loc ?attrs (Pexp_construct (a, b))
    let variant ?loc ?attrs a b = mk ?loc ?attrs (Pexp_variant (a, b))
    let record ?loc ?attrs a b = mk ?loc ?attrs (Pexp_record (a, b))
    let field ?loc ?attrs a b = mk ?loc ?attrs (Pexp_field (a, b))
    let setfield ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_setfield (a, b, c))
    let array ?loc ?attrs a = mk ?loc ?attrs (Pexp_array a)
    let ifthenelse ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_ifthenelse (a, b, c))
    let sequence ?loc ?attrs a b = mk ?loc ?attrs (Pexp_sequence (a, b))
    let while_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_while (a, b))
    let for_ ?loc ?attrs a b c d e = mk ?loc ?attrs (Pexp_for (a, b, c, d, e))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_constraint (a, b))
    let coerce ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_coerce (a, b, c))
    let send ?loc ?attrs a b = mk ?loc ?attrs (Pexp_send (a, b))
    let new_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_new a)
    let setinstvar ?loc ?attrs a b = mk ?loc ?attrs (Pexp_setinstvar (a, b))
    let override ?loc ?attrs a = mk ?loc ?attrs (Pexp_override a)
    let letmodule ?loc ?attrs a b c= mk ?loc ?attrs (Pexp_letmodule (a, b, c))
    let letexception ?loc ?attrs a b = mk ?loc ?attrs (Pexp_letexception (a, b))
    let assert_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_assert a)
    let lazy_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_lazy a)
    let poly ?loc ?attrs a b = mk ?loc ?attrs (Pexp_poly (a, b))
    let object_ ?loc ?attrs a = mk ?loc ?attrs (Pexp_object a)
    let newtype ?loc ?attrs a b = mk ?loc ?attrs (Pexp_newtype (a, b))
    let pack ?loc ?attrs a = mk ?loc ?attrs (Pexp_pack a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_open (a, b))
    let letop ?loc ?attrs let_ ands body =
      mk ?loc ?attrs (Pexp_letop {let_; ands; body})
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pexp_extension a)
    let unreachable ?loc ?attrs () = mk ?loc ?attrs Pexp_unreachable

    let case lhs ?guard rhs =
      {
        pc_lhs = lhs;
        pc_guard = guard;
        pc_rhs = rhs;
      }

    let binding_op op pat exp loc =
      {
        pbop_op = op;
        pbop_pat = pat;
        pbop_exp = exp;
        pbop_loc = loc;
      }
  end

  module Mty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmty_desc = d; pmty_loc = loc; pmty_attributes = attrs}
    let attr d a = {d with pmty_attributes = d.pmty_attributes @ [a]}

    let ident ?loc ?attrs a = mk ?loc ?attrs (Pmty_ident a)
    let alias ?loc ?attrs a = mk ?loc ?attrs (Pmty_alias a)
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pmty_signature a)
    let functor_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_functor (a, b))
    let with_ ?loc ?attrs a b = mk ?loc ?attrs (Pmty_with (a, b))
    let typeof_ ?loc ?attrs a = mk ?loc ?attrs (Pmty_typeof a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmty_extension a)
  end

  module Mod = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {pmod_desc = d; pmod_loc = loc; pmod_attributes = attrs}
    let attr d a = {d with pmod_attributes = d.pmod_attributes @ [a]}

    let ident ?loc ?attrs x = mk ?loc ?attrs (Pmod_ident x)
    let structure ?loc ?attrs x = mk ?loc ?attrs (Pmod_structure x)
    let functor_ ?loc ?attrs arg body =
      mk ?loc ?attrs (Pmod_functor (arg, body))
    let apply ?loc ?attrs m1 m2 = mk ?loc ?attrs (Pmod_apply (m1, m2))
    let constraint_ ?loc ?attrs m mty = mk ?loc ?attrs (Pmod_constraint (m, mty))
    let unpack ?loc ?attrs e = mk ?loc ?attrs (Pmod_unpack e)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pmod_extension a)
  end

  module Sig = struct
    let mk ?(loc = !default_loc) d = {psig_desc = d; psig_loc = loc}

    let value ?loc a = mk ?loc (Psig_value a)
    let type_ ?loc rec_flag a = mk ?loc (Psig_type (rec_flag, a))
    let type_subst ?loc a = mk ?loc (Psig_typesubst a)
    let type_extension ?loc a = mk ?loc (Psig_typext a)
    let exception_ ?loc a = mk ?loc (Psig_exception a)
    let module_ ?loc a = mk ?loc (Psig_module a)
    let mod_subst ?loc a = mk ?loc (Psig_modsubst a)
    let rec_module ?loc a = mk ?loc (Psig_recmodule a)
    let modtype ?loc a = mk ?loc (Psig_modtype a)
    let open_ ?loc a = mk ?loc (Psig_open a)
    let include_ ?loc a = mk ?loc (Psig_include a)
    let class_ ?loc a = mk ?loc (Psig_class a)
    let class_type ?loc a = mk ?loc (Psig_class_type a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Psig_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Psig_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Str = struct
    let mk ?(loc = !default_loc) d = {pstr_desc = d; pstr_loc = loc}

    let eval ?loc ?(attrs = []) a = mk ?loc (Pstr_eval (a, attrs))
    let value ?loc a b = mk ?loc (Pstr_value (a, b))
    let primitive ?loc a = mk ?loc (Pstr_primitive a)
    let type_ ?loc rec_flag a = mk ?loc (Pstr_type (rec_flag, a))
    let type_extension ?loc a = mk ?loc (Pstr_typext a)
    let exception_ ?loc a = mk ?loc (Pstr_exception a)
    let module_ ?loc a = mk ?loc (Pstr_module a)
    let rec_module ?loc a = mk ?loc (Pstr_recmodule a)
    let modtype ?loc a = mk ?loc (Pstr_modtype a)
    let open_ ?loc a = mk ?loc (Pstr_open a)
    let class_ ?loc a = mk ?loc (Pstr_class a)
    let class_type ?loc a = mk ?loc (Pstr_class_type a)
    let include_ ?loc a = mk ?loc (Pstr_include a)
    let extension ?loc ?(attrs = []) a = mk ?loc (Pstr_extension (a, attrs))
    let attribute ?loc a = mk ?loc (Pstr_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt
  end

  module Cl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
        pcl_desc = d;
        pcl_loc = loc;
        pcl_attributes = attrs;
      }
    let attr d a = {d with pcl_attributes = d.pcl_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constr (a, b))
    let structure ?loc ?attrs a = mk ?loc ?attrs (Pcl_structure a)
    let fun_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pcl_fun (a, b, c, d))
    let apply ?loc ?attrs a b = mk ?loc ?attrs (Pcl_apply (a, b))
    let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcl_let (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcl_extension a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcl_open (a, b))
  end

  module Cty = struct
    let mk ?(loc = !default_loc) ?(attrs = []) d =
      {
        pcty_desc = d;
        pcty_loc = loc;
        pcty_attributes = attrs;
      }
    let attr d a = {d with pcty_attributes = d.pcty_attributes @ [a]}

    let constr ?loc ?attrs a b = mk ?loc ?attrs (Pcty_constr (a, b))
    let signature ?loc ?attrs a = mk ?loc ?attrs (Pcty_signature a)
    let arrow ?loc ?attrs a b c = mk ?loc ?attrs (Pcty_arrow (a, b, c))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcty_extension a)
    let open_ ?loc ?attrs a b = mk ?loc ?attrs (Pcty_open (a, b))
  end

  module Ctf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
      {
        pctf_desc = d;
        pctf_loc = loc;
        pctf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a = mk ?loc ?attrs (Pctf_inherit a)
    let val_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_val (a, b, c, d))
    let method_ ?loc ?attrs a b c d = mk ?loc ?attrs (Pctf_method (a, b, c, d))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pctf_constraint (a, b))
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pctf_extension a)
    let attribute ?loc a = mk ?loc (Pctf_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let attr d a = {d with pctf_attributes = d.pctf_attributes @ [a]}

  end

  module Cf = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) d =
      {
        pcf_desc = d;
        pcf_loc = loc;
        pcf_attributes = add_docs_attrs docs attrs;
      }

    let inherit_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_inherit (a, b, c))
    let val_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_val (a, b, c))
    let method_ ?loc ?attrs a b c = mk ?loc ?attrs (Pcf_method (a, b, c))
    let constraint_ ?loc ?attrs a b = mk ?loc ?attrs (Pcf_constraint (a, b))
    let initializer_ ?loc ?attrs a = mk ?loc ?attrs (Pcf_initializer a)
    let extension ?loc ?attrs a = mk ?loc ?attrs (Pcf_extension a)
    let attribute ?loc a = mk ?loc (Pcf_attribute a)
    let text txt =
      let f_txt = List.filter (fun ds -> docstring_body ds <> "") txt in
      List.map
        (fun ds -> attribute ~loc:(docstring_loc ds) (text_attr ds))
        f_txt

    let virtual_ ct = Cfk_virtual ct
    let concrete o e = Cfk_concrete (o, e)

    let attr d a = {d with pcf_attributes = d.pcf_attributes @ [a]}

  end

  module Val = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(prim = []) name typ =
      {
        pval_name = name;
        pval_type = typ;
        pval_attributes = add_docs_attrs docs attrs;
        pval_loc = loc;
        pval_prim = prim;
      }
  end

  module Md = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name typ =
      {
        pmd_name = name;
        pmd_type = typ;
        pmd_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pmd_loc = loc;
      }
  end

  module Ms = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name syn =
      {
        pms_name = name;
        pms_manifest = syn;
        pms_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pms_loc = loc;
      }
  end

  module Mtd = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) ?typ name =
      {
        pmtd_name = name;
        pmtd_type = typ;
        pmtd_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pmtd_loc = loc;
      }
  end

  module Mb = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = []) name expr =
      {
        pmb_name = name;
        pmb_expr = expr;
        pmb_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pmb_loc = loc;
      }
  end

  module Opn = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(override = Fresh) expr =
      {
        popen_expr = expr;
        popen_override = override;
        popen_loc = loc;
        popen_attributes = add_docs_attrs docs attrs;
      }
  end

  module Incl = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs) mexpr =
      {
        pincl_mod = mexpr;
        pincl_loc = loc;
        pincl_attributes = add_docs_attrs docs attrs;
      }

  end

  module Vb = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(text = []) pat expr =
      {
        pvb_pat = pat;
        pvb_expr = expr;
        pvb_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pvb_loc = loc;
      }
  end

  module Ci = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(virt = Concrete) ?(params = []) name expr =
      {
        pci_virt = virt;
        pci_params = params;
        pci_name = name;
        pci_expr = expr;
        pci_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        pci_loc = loc;
      }
  end

  module Type = struct
    let mk ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(text = [])
        ?(params = [])
        ?(cstrs = [])
        ?(kind = Ptype_abstract)
        ?(priv = Public)
        ?manifest
        name =
      {
        ptype_name = name;
        ptype_params = params;
        ptype_cstrs = cstrs;
        ptype_kind = kind;
        ptype_private = priv;
        ptype_manifest = manifest;
        ptype_attributes =
          add_text_attrs text (add_docs_attrs docs attrs);
        ptype_loc = loc;
      }

    let constructor ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(args = Pcstr_tuple []) ?res name =
      {
        pcd_name = name;
        pcd_args = args;
        pcd_res = res;
        pcd_loc = loc;
        pcd_attributes = add_info_attrs info attrs;
      }

    let field ?(loc = !default_loc) ?(attrs = []) ?(info = empty_info)
        ?(mut = Immutable) name typ =
      {
        pld_name = name;
        pld_mutable = mut;
        pld_type = typ;
        pld_loc = loc;
        pld_attributes = add_info_attrs info attrs;
      }

  end

  (** Type extensions *)
  module Te = struct
    let mk ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(params = []) ?(priv = Public) path constructors =
      {
        ptyext_path = path;
        ptyext_params = params;
        ptyext_constructors = constructors;
        ptyext_private = priv;
        ptyext_loc = loc;
        ptyext_attributes = add_docs_attrs docs attrs;
      }

    let mk_exception ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        constructor =
      {
        ptyexn_constructor = constructor;
        ptyexn_loc = loc;
        ptyexn_attributes = add_docs_attrs docs attrs;
      }

    let constructor ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name kind =
      {
        pext_name = name;
        pext_kind = kind;
        pext_loc = loc;
        pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let decl ?(loc = !default_loc) ?(attrs = []) ?(docs = empty_docs)
        ?(info = empty_info) ?(args = Pcstr_tuple []) ?res name =
      {
        pext_name = name;
        pext_kind = Pext_decl(args, res);
        pext_loc = loc;
        pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

    let rebind ?(loc = !default_loc) ?(attrs = [])
        ?(docs = empty_docs) ?(info = empty_info) name lid =
      {
        pext_name = name;
        pext_kind = Pext_rebind lid;
        pext_loc = loc;
        pext_attributes = add_docs_attrs docs (add_info_attrs info attrs);
      }

  end

  module Csig = struct
    let mk self fields =
      {
        pcsig_self = self;
        pcsig_fields = fields;
      }
  end

  module Cstr = struct
    let mk self fields =
      {
        pcstr_self = self;
        pcstr_fields = fields;
      }
  end

  (** Row fields *)
  module Rf = struct
    let mk ?(loc = !default_loc) ?(attrs = []) desc = {
      prf_desc = desc;
      prf_loc = loc;
      prf_attributes = attrs;
    }
    let tag ?loc ?attrs label const tys =
      mk ?loc ?attrs (Rtag (label, const, tys))
    let inherit_?loc ty =
      mk ?loc (Rinherit ty)
  end

  (** Object fields *)
  module Of = struct
    let mk ?(loc = !default_loc) ?(attrs=[]) desc = {
      pof_desc = desc;
      pof_loc = loc;
      pof_attributes = attrs;
    }
    let tag ?loc ?attrs label ty =
      mk ?loc ?attrs (Otag (label, ty))
    let inherit_ ?loc ty =
      mk ?loc (Oinherit ty)
  end
end

