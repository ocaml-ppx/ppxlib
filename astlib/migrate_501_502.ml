open Stdlib0
module From = Ast_501
module To = Ast_502

(** Look for a particular attribute and remove it from the list. Attributes are
    used to make certain migrations round-trip. Returns [None] if the specified
    attribute is not found. *)
let extract_attr name (attrs : Ast_501.Parsetree.attributes) =
  let rec loop acc = function
    | [] -> (None, List.rev acc)
    | { Ast_501.Parsetree.attr_name = { txt; _ }; attr_payload; _ } :: q
      when txt = name ->
        (Some attr_payload, List.rev_append acc q)
    | hd :: tl -> loop (hd :: acc) tl
  in
  loop [] attrs

let migrate_ppx_context_load_path expr =
  let open Ast_501.Parsetree in
  let payload, other_attrs =
    extract_attr "ppxlib.migration.load_path" expr.pexp_attributes
  in
  match payload with
  | None ->
      let pexp_desc =
        Pexp_construct ({ txt = Lident "[]"; loc = expr.pexp_loc }, None)
      in
      let hidden = { expr with pexp_desc; pexp_attributes = [] } in
      let visible = expr in
      {
        expr with
        pexp_attributes = other_attrs;
        pexp_desc = Pexp_tuple [ visible; hidden ];
      }
  | Some (PStr [ { pstr_desc = Pstr_eval (expr, []); _ } ]) -> expr
  | Some _ -> invalid_arg "Invalid ppxlib.migration.load_path payload"

let migrate_ppx_context_fields fields =
  List.map
    (fun (lident_loc, expr) ->
      match lident_loc.Ast_501.Asttypes.txt with
      | Longident.Lident "load_path" ->
          (lident_loc, migrate_ppx_context_load_path expr)
      | _ -> (lident_loc, expr))
    fields

let migrate_ppx_context_payload payload =
  let open Ast_501.Parsetree in
  match payload with
  | PStr
      [
        ({
           pstr_desc =
             Pstr_eval
               (({ pexp_desc = Pexp_record (fields, None) } as expr), attributes);
         } as stri);
      ] ->
      let new_fields = migrate_ppx_context_fields fields in
      let new_expr = { expr with pexp_desc = Pexp_record (new_fields, None) } in
      PStr [ { stri with pstr_desc = Pstr_eval (new_expr, attributes) } ]
  | _ -> payload

let rec copy_toplevel_phrase :
    Ast_501.Parsetree.toplevel_phrase -> Ast_502.Parsetree.toplevel_phrase =
  function
  | Ast_501.Parsetree.Ptop_def x0 ->
      Ast_502.Parsetree.Ptop_def (copy_structure x0)
  | Ast_501.Parsetree.Ptop_dir x0 ->
      Ast_502.Parsetree.Ptop_dir (copy_toplevel_directive x0)

and copy_toplevel_directive :
    Ast_501.Parsetree.toplevel_directive -> Ast_502.Parsetree.toplevel_directive
    =
 fun {
       Ast_501.Parsetree.pdir_name;
       Ast_501.Parsetree.pdir_arg;
       Ast_501.Parsetree.pdir_loc;
     } ->
  {
    Ast_502.Parsetree.pdir_name = copy_loc (fun x -> x) pdir_name;
    Ast_502.Parsetree.pdir_arg = Option.map copy_directive_argument pdir_arg;
    Ast_502.Parsetree.pdir_loc = copy_location pdir_loc;
  }

and copy_directive_argument :
    Ast_501.Parsetree.directive_argument -> Ast_502.Parsetree.directive_argument
    =
 fun { Ast_501.Parsetree.pdira_desc; Ast_501.Parsetree.pdira_loc } ->
  {
    Ast_502.Parsetree.pdira_desc = copy_directive_argument_desc pdira_desc;
    Ast_502.Parsetree.pdira_loc = copy_location pdira_loc;
  }

and copy_directive_argument_desc :
    Ast_501.Parsetree.directive_argument_desc ->
    Ast_502.Parsetree.directive_argument_desc = function
  | Ast_501.Parsetree.Pdir_string x0 -> Ast_502.Parsetree.Pdir_string x0
  | Ast_501.Parsetree.Pdir_int (x0, x1) ->
      Ast_502.Parsetree.Pdir_int (x0, Option.map (fun x -> x) x1)
  | Ast_501.Parsetree.Pdir_ident x0 ->
      Ast_502.Parsetree.Pdir_ident (copy_Longident_t x0)
  | Ast_501.Parsetree.Pdir_bool x0 -> Ast_502.Parsetree.Pdir_bool x0

and copy_expression :
    Ast_501.Parsetree.expression -> Ast_502.Parsetree.expression =
 fun {
       Ast_501.Parsetree.pexp_desc;
       Ast_501.Parsetree.pexp_loc;
       Ast_501.Parsetree.pexp_loc_stack;
       Ast_501.Parsetree.pexp_attributes;
     } ->
  let pexp_loc = copy_location pexp_loc in
  {
    Ast_502.Parsetree.pexp_desc = copy_expression_desc pexp_loc pexp_desc;
    Ast_502.Parsetree.pexp_loc;
    Ast_502.Parsetree.pexp_loc_stack = copy_location_stack pexp_loc_stack;
    Ast_502.Parsetree.pexp_attributes = copy_attributes pexp_attributes;
  }

and copy_expression_desc loc :
    Ast_501.Parsetree.expression_desc -> Ast_502.Parsetree.expression_desc =
  function
  | Ast_501.Parsetree.Pexp_ident x0 ->
      Ast_502.Parsetree.Pexp_ident (copy_loc copy_Longident_t x0)
  | Ast_501.Parsetree.Pexp_constant x0 ->
      Ast_502.Parsetree.Pexp_constant (copy_constant x0)
  | Ast_501.Parsetree.Pexp_let (x0, x1, x2) ->
      Ast_502.Parsetree.Pexp_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_expression x2)
  | Ast_501.Parsetree.Pexp_function x0 ->
      Ast_502.Parsetree.Pexp_function
        ( [],
          None,
          Ast_502.Parsetree.Pfunction_cases (List.map copy_case x0, loc, []) )
  | Ast_501.Parsetree.Pexp_fun (arg_label, opt_default, pat, expr) ->
      let take_body (e : Ast_501.Parsetree.expression) =
        match e.pexp_desc with
        | Ast_501.Parsetree.Pexp_function case_list ->
            Ast_502.Parsetree.Pfunction_cases
              ( List.map copy_case case_list,
                e.pexp_loc,
                copy_attributes e.pexp_attributes )
        | _ -> Ast_502.Parsetree.Pfunction_body (copy_expression e)
      in
      let rec take_arguments acc (e : Ast_501.Parsetree.expression) =
        if e.pexp_attributes <> [] then
          (* The attribute list is not empty, none of these nodes could have
             been created by the downward migration. Except for [Pexp_fun], for
             which we add a ghost attribute to help us roundtrip. *)
          let _, attrs =
            extract_attr "ppxlib.migration.stop_taking" e.pexp_attributes
          in
          ( acc,
            None,
            Ast_502.Parsetree.Pfunction_body
              (copy_expression { e with pexp_attributes = attrs }) )
        else
          (* These nodes are likely to have been synthetized during the
             downward migration. *)
          match e.pexp_desc with
          | Ast_501.Parsetree.Pexp_fun (arg_label, opt_default, pat, expr) ->
              take_arguments_fun acc arg_label opt_default pat expr
          | Ast_501.Parsetree.Pexp_newtype (t, expr) ->
              let acc =
                {
                  Ast_502.Parsetree.pparam_loc = t.loc;
                  pparam_desc = Pparam_newtype t;
                }
                :: acc
              in
              take_arguments acc expr
          | Ast_501.Parsetree.Pexp_constraint (exp, ct) ->
              (* These two expression are represented the same on 5.1 but
                 differently on 5.2:
                 {[
                   let _ = fun x : (_ -> int) -> fun y -> x+y
                   let _ = fun x -> ((fun y -> x+y) : _ -> int)
                 ]}
                 We normalize the second into the first when migrating to 5.2,
                 making the migration 5.2->5.1->5.2 not roundtrip but hopefully
                 without change in semantics. *)
              let ct =
                Some (Ast_502.Parsetree.Pconstraint (copy_core_type ct))
              in
              (acc, ct, take_body exp)
          | Ast_501.Parsetree.Pexp_coerce (exp, c1, c2) ->
              (* Same as above, might not roundtrip but hopefully OK. *)
              let c1 = Option.map copy_core_type c1
              and c2 = copy_core_type c2 in
              (acc, Some (Ast_502.Parsetree.Pcoerce (c1, c2)), take_body exp)
          | _ -> (acc, None, take_body e)
      and take_arguments_fun acc arg_label opt_default pat expr =
        let acc =
          let pparam_desc =
            Ast_502.Parsetree.Pparam_val
              ( copy_arg_label arg_label,
                Option.map copy_expression opt_default,
                copy_pattern pat )
          in
          (* Best-effort location. *)
          { Ast_502.Parsetree.pparam_loc = pat.ppat_loc; pparam_desc } :: acc
        in
        take_arguments acc expr
      in
      (* The argument list returned by [take_arguments] is reversed *)
      let arg_list, type_constraint, body =
        take_arguments_fun [] arg_label opt_default pat expr
      in
      Ast_502.Parsetree.Pexp_function (List.rev arg_list, type_constraint, body)
  | Ast_501.Parsetree.Pexp_apply (x0, x1) ->
      Ast_502.Parsetree.Pexp_apply
        ( copy_expression x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_501.Parsetree.Pexp_match (x0, x1) ->
      Ast_502.Parsetree.Pexp_match (copy_expression x0, List.map copy_case x1)
  | Ast_501.Parsetree.Pexp_try (x0, x1) ->
      Ast_502.Parsetree.Pexp_try (copy_expression x0, List.map copy_case x1)
  | Ast_501.Parsetree.Pexp_tuple x0 ->
      Ast_502.Parsetree.Pexp_tuple (List.map copy_expression x0)
  | Ast_501.Parsetree.Pexp_construct (x0, x1) ->
      Ast_502.Parsetree.Pexp_construct
        (copy_loc copy_Longident_t x0, Option.map copy_expression x1)
  | Ast_501.Parsetree.Pexp_variant (x0, x1) ->
      Ast_502.Parsetree.Pexp_variant
        (copy_label x0, Option.map copy_expression x1)
  | Ast_501.Parsetree.Pexp_record (x0, x1) ->
      Ast_502.Parsetree.Pexp_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_Longident_t x0, copy_expression x1))
            x0,
          Option.map copy_expression x1 )
  | Ast_501.Parsetree.Pexp_field (x0, x1) ->
      Ast_502.Parsetree.Pexp_field
        (copy_expression x0, copy_loc copy_Longident_t x1)
  | Ast_501.Parsetree.Pexp_setfield (x0, x1, x2) ->
      Ast_502.Parsetree.Pexp_setfield
        (copy_expression x0, copy_loc copy_Longident_t x1, copy_expression x2)
  | Ast_501.Parsetree.Pexp_array x0 ->
      Ast_502.Parsetree.Pexp_array (List.map copy_expression x0)
  | Ast_501.Parsetree.Pexp_ifthenelse (x0, x1, x2) ->
      Ast_502.Parsetree.Pexp_ifthenelse
        (copy_expression x0, copy_expression x1, Option.map copy_expression x2)
  | Ast_501.Parsetree.Pexp_sequence (x0, x1) ->
      Ast_502.Parsetree.Pexp_sequence (copy_expression x0, copy_expression x1)
  | Ast_501.Parsetree.Pexp_while (x0, x1) ->
      Ast_502.Parsetree.Pexp_while (copy_expression x0, copy_expression x1)
  | Ast_501.Parsetree.Pexp_for (x0, x1, x2, x3, x4) ->
      Ast_502.Parsetree.Pexp_for
        ( copy_pattern x0,
          copy_expression x1,
          copy_expression x2,
          copy_direction_flag x3,
          copy_expression x4 )
  | Ast_501.Parsetree.Pexp_constraint (x0, x1) ->
      Ast_502.Parsetree.Pexp_constraint (copy_expression x0, copy_core_type x1)
  | Ast_501.Parsetree.Pexp_coerce (x0, x1, x2) ->
      Ast_502.Parsetree.Pexp_coerce
        (copy_expression x0, Option.map copy_core_type x1, copy_core_type x2)
  | Ast_501.Parsetree.Pexp_send (x0, x1) ->
      Ast_502.Parsetree.Pexp_send (copy_expression x0, copy_loc copy_label x1)
  | Ast_501.Parsetree.Pexp_new x0 ->
      Ast_502.Parsetree.Pexp_new (copy_loc copy_Longident_t x0)
  | Ast_501.Parsetree.Pexp_setinstvar (x0, x1) ->
      Ast_502.Parsetree.Pexp_setinstvar
        (copy_loc copy_label x0, copy_expression x1)
  | Ast_501.Parsetree.Pexp_override x0 ->
      Ast_502.Parsetree.Pexp_override
        (List.map
           (fun x ->
             let x0, x1 = x in
             (copy_loc copy_label x0, copy_expression x1))
           x0)
  | Ast_501.Parsetree.Pexp_letmodule (x0, x1, x2) ->
      Ast_502.Parsetree.Pexp_letmodule
        ( copy_loc (fun x -> Option.map (fun x -> x) x) x0,
          copy_module_expr x1,
          copy_expression x2 )
  | Ast_501.Parsetree.Pexp_letexception (x0, x1) ->
      Ast_502.Parsetree.Pexp_letexception
        (copy_extension_constructor x0, copy_expression x1)
  | Ast_501.Parsetree.Pexp_assert x0 ->
      Ast_502.Parsetree.Pexp_assert (copy_expression x0)
  | Ast_501.Parsetree.Pexp_lazy x0 ->
      Ast_502.Parsetree.Pexp_lazy (copy_expression x0)
  | Ast_501.Parsetree.Pexp_poly (x0, x1) ->
      Ast_502.Parsetree.Pexp_poly
        (copy_expression x0, Option.map copy_core_type x1)
  | Ast_501.Parsetree.Pexp_object x0 ->
      Ast_502.Parsetree.Pexp_object (copy_class_structure x0)
  | Ast_501.Parsetree.Pexp_newtype (x0, x1) ->
      Ast_502.Parsetree.Pexp_newtype
        (copy_loc (fun x -> x) x0, copy_expression x1)
  | Ast_501.Parsetree.Pexp_pack x0 ->
      Ast_502.Parsetree.Pexp_pack (copy_module_expr x0)
  | Ast_501.Parsetree.Pexp_open (x0, x1) ->
      Ast_502.Parsetree.Pexp_open (copy_open_declaration x0, copy_expression x1)
  | Ast_501.Parsetree.Pexp_letop x0 ->
      Ast_502.Parsetree.Pexp_letop (copy_letop x0)
  | Ast_501.Parsetree.Pexp_extension x0 ->
      Ast_502.Parsetree.Pexp_extension (copy_extension x0)
  | Ast_501.Parsetree.Pexp_unreachable -> Ast_502.Parsetree.Pexp_unreachable

and copy_letop : Ast_501.Parsetree.letop -> Ast_502.Parsetree.letop =
 fun { Ast_501.Parsetree.let_; Ast_501.Parsetree.ands; Ast_501.Parsetree.body } ->
  {
    Ast_502.Parsetree.let_ = copy_binding_op let_;
    Ast_502.Parsetree.ands = List.map copy_binding_op ands;
    Ast_502.Parsetree.body = copy_expression body;
  }

and copy_binding_op :
    Ast_501.Parsetree.binding_op -> Ast_502.Parsetree.binding_op =
 fun {
       Ast_501.Parsetree.pbop_op;
       Ast_501.Parsetree.pbop_pat;
       Ast_501.Parsetree.pbop_exp;
       Ast_501.Parsetree.pbop_loc;
     } ->
  {
    Ast_502.Parsetree.pbop_op = copy_loc (fun x -> x) pbop_op;
    Ast_502.Parsetree.pbop_pat = copy_pattern pbop_pat;
    Ast_502.Parsetree.pbop_exp = copy_expression pbop_exp;
    Ast_502.Parsetree.pbop_loc = copy_location pbop_loc;
  }

and copy_direction_flag :
    Ast_501.Asttypes.direction_flag -> Ast_502.Asttypes.direction_flag =
  function
  | Ast_501.Asttypes.Upto -> Ast_502.Asttypes.Upto
  | Ast_501.Asttypes.Downto -> Ast_502.Asttypes.Downto

and copy_case : Ast_501.Parsetree.case -> Ast_502.Parsetree.case =
 fun {
       Ast_501.Parsetree.pc_lhs;
       Ast_501.Parsetree.pc_guard;
       Ast_501.Parsetree.pc_rhs;
     } ->
  {
    Ast_502.Parsetree.pc_lhs = copy_pattern pc_lhs;
    Ast_502.Parsetree.pc_guard = Option.map copy_expression pc_guard;
    Ast_502.Parsetree.pc_rhs = copy_expression pc_rhs;
  }

and copy_value_binding :
    Ast_501.Parsetree.value_binding -> Ast_502.Parsetree.value_binding =
 fun {
       Ast_501.Parsetree.pvb_pat;
       Ast_501.Parsetree.pvb_expr;
       Ast_501.Parsetree.pvb_constraint;
       Ast_501.Parsetree.pvb_attributes;
       Ast_501.Parsetree.pvb_loc;
     } ->
  {
    Ast_502.Parsetree.pvb_pat = copy_pattern pvb_pat;
    Ast_502.Parsetree.pvb_expr = copy_expression pvb_expr;
    Ast_502.Parsetree.pvb_constraint =
      Option.map copy_value_constraint pvb_constraint;
    Ast_502.Parsetree.pvb_attributes = copy_attributes pvb_attributes;
    Ast_502.Parsetree.pvb_loc = copy_location pvb_loc;
  }

and copy_pattern : Ast_501.Parsetree.pattern -> Ast_502.Parsetree.pattern =
 fun {
       Ast_501.Parsetree.ppat_desc;
       Ast_501.Parsetree.ppat_loc;
       Ast_501.Parsetree.ppat_loc_stack;
       Ast_501.Parsetree.ppat_attributes;
     } ->
  {
    Ast_502.Parsetree.ppat_desc = copy_pattern_desc ppat_desc;
    Ast_502.Parsetree.ppat_loc = copy_location ppat_loc;
    Ast_502.Parsetree.ppat_loc_stack = copy_location_stack ppat_loc_stack;
    Ast_502.Parsetree.ppat_attributes = copy_attributes ppat_attributes;
  }

and copy_pattern_desc :
    Ast_501.Parsetree.pattern_desc -> Ast_502.Parsetree.pattern_desc = function
  | Ast_501.Parsetree.Ppat_any -> Ast_502.Parsetree.Ppat_any
  | Ast_501.Parsetree.Ppat_var x0 ->
      Ast_502.Parsetree.Ppat_var (copy_loc (fun x -> x) x0)
  | Ast_501.Parsetree.Ppat_alias (x0, x1) ->
      Ast_502.Parsetree.Ppat_alias (copy_pattern x0, copy_loc (fun x -> x) x1)
  | Ast_501.Parsetree.Ppat_constant x0 ->
      Ast_502.Parsetree.Ppat_constant (copy_constant x0)
  | Ast_501.Parsetree.Ppat_interval (x0, x1) ->
      Ast_502.Parsetree.Ppat_interval (copy_constant x0, copy_constant x1)
  | Ast_501.Parsetree.Ppat_tuple x0 ->
      Ast_502.Parsetree.Ppat_tuple (List.map copy_pattern x0)
  | Ast_501.Parsetree.Ppat_construct (x0, x1) ->
      Ast_502.Parsetree.Ppat_construct
        ( copy_loc copy_Longident_t x0,
          Option.map
            (fun x ->
              let x0, x1 = x in
              (List.map (fun x -> copy_loc (fun x -> x) x) x0, copy_pattern x1))
            x1 )
  | Ast_501.Parsetree.Ppat_variant (x0, x1) ->
      Ast_502.Parsetree.Ppat_variant (copy_label x0, Option.map copy_pattern x1)
  | Ast_501.Parsetree.Ppat_record (x0, x1) ->
      Ast_502.Parsetree.Ppat_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_Longident_t x0, copy_pattern x1))
            x0,
          copy_closed_flag x1 )
  | Ast_501.Parsetree.Ppat_array x0 ->
      Ast_502.Parsetree.Ppat_array (List.map copy_pattern x0)
  | Ast_501.Parsetree.Ppat_or (x0, x1) ->
      Ast_502.Parsetree.Ppat_or (copy_pattern x0, copy_pattern x1)
  | Ast_501.Parsetree.Ppat_constraint (x0, x1) ->
      Ast_502.Parsetree.Ppat_constraint (copy_pattern x0, copy_core_type x1)
  | Ast_501.Parsetree.Ppat_type x0 ->
      Ast_502.Parsetree.Ppat_type (copy_loc copy_Longident_t x0)
  | Ast_501.Parsetree.Ppat_lazy x0 ->
      Ast_502.Parsetree.Ppat_lazy (copy_pattern x0)
  | Ast_501.Parsetree.Ppat_unpack x0 ->
      Ast_502.Parsetree.Ppat_unpack
        (copy_loc (fun x -> Option.map (fun x -> x) x) x0)
  | Ast_501.Parsetree.Ppat_exception x0 ->
      Ast_502.Parsetree.Ppat_exception (copy_pattern x0)
  | Ast_501.Parsetree.Ppat_extension x0 ->
      Ast_502.Parsetree.Ppat_extension (copy_extension x0)
  | Ast_501.Parsetree.Ppat_open (x0, x1) ->
      Ast_502.Parsetree.Ppat_open (copy_loc copy_Longident_t x0, copy_pattern x1)

and copy_value_constraint :
    Ast_501.Parsetree.value_constraint -> Ast_502.Parsetree.value_constraint =
  function
  | Ast_501.Parsetree.Pvc_constraint { locally_abstract_univars; typ } ->
      Ast_502.Parsetree.Pvc_constraint
        {
          locally_abstract_univars =
            List.map (copy_loc (fun x -> x)) locally_abstract_univars;
          typ = copy_core_type typ;
        }
  | Ast_501.Parsetree.Pvc_coercion { ground; coercion } ->
      Ast_502.Parsetree.Pvc_coercion
        {
          ground = Option.map copy_core_type ground;
          coercion = copy_core_type coercion;
        }

and copy_core_type : Ast_501.Parsetree.core_type -> Ast_502.Parsetree.core_type
    =
 fun {
       Ast_501.Parsetree.ptyp_desc;
       Ast_501.Parsetree.ptyp_loc;
       Ast_501.Parsetree.ptyp_loc_stack;
       Ast_501.Parsetree.ptyp_attributes;
     } ->
  {
    Ast_502.Parsetree.ptyp_desc = copy_core_type_desc ptyp_desc;
    Ast_502.Parsetree.ptyp_loc = copy_location ptyp_loc;
    Ast_502.Parsetree.ptyp_loc_stack = copy_location_stack ptyp_loc_stack;
    Ast_502.Parsetree.ptyp_attributes = copy_attributes ptyp_attributes;
  }

and copy_location_stack :
    Ast_501.Parsetree.location_stack -> Ast_502.Parsetree.location_stack =
 fun x -> List.map copy_location x

and copy_core_type_desc :
    Ast_501.Parsetree.core_type_desc -> Ast_502.Parsetree.core_type_desc =
  function
  | Ast_501.Parsetree.Ptyp_any -> Ast_502.Parsetree.Ptyp_any
  | Ast_501.Parsetree.Ptyp_var x0 -> Ast_502.Parsetree.Ptyp_var x0
  | Ast_501.Parsetree.Ptyp_arrow (x0, x1, x2) ->
      Ast_502.Parsetree.Ptyp_arrow
        (copy_arg_label x0, copy_core_type x1, copy_core_type x2)
  | Ast_501.Parsetree.Ptyp_tuple x0 ->
      Ast_502.Parsetree.Ptyp_tuple (List.map copy_core_type x0)
  | Ast_501.Parsetree.Ptyp_constr (x0, x1) ->
      Ast_502.Parsetree.Ptyp_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_501.Parsetree.Ptyp_object (x0, x1) ->
      Ast_502.Parsetree.Ptyp_object
        (List.map copy_object_field x0, copy_closed_flag x1)
  | Ast_501.Parsetree.Ptyp_class (x0, x1) ->
      Ast_502.Parsetree.Ptyp_class
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_501.Parsetree.Ptyp_alias (x0, x1) ->
      Ast_502.Parsetree.Ptyp_alias
        (copy_core_type x0, { Ast_502.Asttypes.txt = x1; loc = x0.ptyp_loc })
  | Ast_501.Parsetree.Ptyp_variant (x0, x1, x2) ->
      Ast_502.Parsetree.Ptyp_variant
        ( List.map copy_row_field x0,
          copy_closed_flag x1,
          Option.map (fun x -> List.map copy_label x) x2 )
  | Ast_501.Parsetree.Ptyp_poly (x0, x1) ->
      Ast_502.Parsetree.Ptyp_poly
        (List.map (fun x -> copy_loc (fun x -> x) x) x0, copy_core_type x1)
  | Ast_501.Parsetree.Ptyp_package x0 ->
      Ast_502.Parsetree.Ptyp_package (copy_package_type x0)
  | Ast_501.Parsetree.Ptyp_extension x0 ->
      Ast_502.Parsetree.Ptyp_extension (copy_extension x0)

and copy_package_type :
    Ast_501.Parsetree.package_type -> Ast_502.Parsetree.package_type =
 fun x ->
  let x0, x1 = x in
  ( copy_loc copy_Longident_t x0,
    List.map
      (fun x ->
        let x0, x1 = x in
        (copy_loc copy_Longident_t x0, copy_core_type x1))
      x1 )

and copy_row_field : Ast_501.Parsetree.row_field -> Ast_502.Parsetree.row_field
    =
 fun {
       Ast_501.Parsetree.prf_desc;
       Ast_501.Parsetree.prf_loc;
       Ast_501.Parsetree.prf_attributes;
     } ->
  {
    Ast_502.Parsetree.prf_desc = copy_row_field_desc prf_desc;
    Ast_502.Parsetree.prf_loc = copy_location prf_loc;
    Ast_502.Parsetree.prf_attributes = copy_attributes prf_attributes;
  }

and copy_row_field_desc :
    Ast_501.Parsetree.row_field_desc -> Ast_502.Parsetree.row_field_desc =
  function
  | Ast_501.Parsetree.Rtag (x0, x1, x2) ->
      Ast_502.Parsetree.Rtag
        (copy_loc copy_label x0, x1, List.map copy_core_type x2)
  | Ast_501.Parsetree.Rinherit x0 ->
      Ast_502.Parsetree.Rinherit (copy_core_type x0)

and copy_object_field :
    Ast_501.Parsetree.object_field -> Ast_502.Parsetree.object_field =
 fun {
       Ast_501.Parsetree.pof_desc;
       Ast_501.Parsetree.pof_loc;
       Ast_501.Parsetree.pof_attributes;
     } ->
  {
    Ast_502.Parsetree.pof_desc = copy_object_field_desc pof_desc;
    Ast_502.Parsetree.pof_loc = copy_location pof_loc;
    Ast_502.Parsetree.pof_attributes = copy_attributes pof_attributes;
  }

and copy_attributes :
    Ast_501.Parsetree.attributes -> Ast_502.Parsetree.attributes =
 fun x -> List.map copy_attribute x

and copy_attribute : Ast_501.Parsetree.attribute -> Ast_502.Parsetree.attribute
    =
 fun {
       Ast_501.Parsetree.attr_name;
       Ast_501.Parsetree.attr_payload;
       Ast_501.Parsetree.attr_loc;
     } ->
  {
    Ast_502.Parsetree.attr_name = copy_loc (fun x -> x) attr_name;
    Ast_502.Parsetree.attr_payload =
      (match attr_name.txt with
      | "ocaml.ppx.context" ->
          copy_payload (migrate_ppx_context_payload attr_payload)
      | _ -> copy_payload attr_payload);
    Ast_502.Parsetree.attr_loc = copy_location attr_loc;
  }

and copy_payload : Ast_501.Parsetree.payload -> Ast_502.Parsetree.payload =
  function
  | Ast_501.Parsetree.PStr x0 -> Ast_502.Parsetree.PStr (copy_structure x0)
  | Ast_501.Parsetree.PSig x0 -> Ast_502.Parsetree.PSig (copy_signature x0)
  | Ast_501.Parsetree.PTyp x0 -> Ast_502.Parsetree.PTyp (copy_core_type x0)
  | Ast_501.Parsetree.PPat (x0, x1) ->
      Ast_502.Parsetree.PPat (copy_pattern x0, Option.map copy_expression x1)

and copy_structure : Ast_501.Parsetree.structure -> Ast_502.Parsetree.structure
    =
 fun x -> List.map copy_structure_item x

and copy_structure_item :
    Ast_501.Parsetree.structure_item -> Ast_502.Parsetree.structure_item =
 fun { Ast_501.Parsetree.pstr_desc; Ast_501.Parsetree.pstr_loc } ->
  {
    Ast_502.Parsetree.pstr_desc = copy_structure_item_desc pstr_desc;
    Ast_502.Parsetree.pstr_loc = copy_location pstr_loc;
  }

and copy_structure_item_desc :
    Ast_501.Parsetree.structure_item_desc ->
    Ast_502.Parsetree.structure_item_desc = function
  | Ast_501.Parsetree.Pstr_eval (x0, x1) ->
      Ast_502.Parsetree.Pstr_eval (copy_expression x0, copy_attributes x1)
  | Ast_501.Parsetree.Pstr_value (x0, x1) ->
      Ast_502.Parsetree.Pstr_value
        (copy_rec_flag x0, List.map copy_value_binding x1)
  | Ast_501.Parsetree.Pstr_primitive x0 ->
      Ast_502.Parsetree.Pstr_primitive (copy_value_description x0)
  | Ast_501.Parsetree.Pstr_type (x0, x1) ->
      Ast_502.Parsetree.Pstr_type
        (copy_rec_flag x0, List.map copy_type_declaration x1)
  | Ast_501.Parsetree.Pstr_typext x0 ->
      Ast_502.Parsetree.Pstr_typext (copy_type_extension x0)
  | Ast_501.Parsetree.Pstr_exception x0 ->
      Ast_502.Parsetree.Pstr_exception (copy_type_exception x0)
  | Ast_501.Parsetree.Pstr_module x0 ->
      Ast_502.Parsetree.Pstr_module (copy_module_binding x0)
  | Ast_501.Parsetree.Pstr_recmodule x0 ->
      Ast_502.Parsetree.Pstr_recmodule (List.map copy_module_binding x0)
  | Ast_501.Parsetree.Pstr_modtype x0 ->
      Ast_502.Parsetree.Pstr_modtype (copy_module_type_declaration x0)
  | Ast_501.Parsetree.Pstr_open x0 ->
      Ast_502.Parsetree.Pstr_open (copy_open_declaration x0)
  | Ast_501.Parsetree.Pstr_class x0 ->
      Ast_502.Parsetree.Pstr_class (List.map copy_class_declaration x0)
  | Ast_501.Parsetree.Pstr_class_type x0 ->
      Ast_502.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_501.Parsetree.Pstr_include x0 ->
      Ast_502.Parsetree.Pstr_include (copy_include_declaration x0)
  | Ast_501.Parsetree.Pstr_attribute x0 ->
      Ast_502.Parsetree.Pstr_attribute (copy_attribute x0)
  | Ast_501.Parsetree.Pstr_extension (x0, x1) ->
      Ast_502.Parsetree.Pstr_extension (copy_extension x0, copy_attributes x1)

and copy_include_declaration :
    Ast_501.Parsetree.include_declaration ->
    Ast_502.Parsetree.include_declaration =
 fun x -> copy_include_infos copy_module_expr x

and copy_class_declaration :
    Ast_501.Parsetree.class_declaration -> Ast_502.Parsetree.class_declaration =
 fun x -> copy_class_infos copy_class_expr x

and copy_class_expr :
    Ast_501.Parsetree.class_expr -> Ast_502.Parsetree.class_expr =
 fun {
       Ast_501.Parsetree.pcl_desc;
       Ast_501.Parsetree.pcl_loc;
       Ast_501.Parsetree.pcl_attributes;
     } ->
  {
    Ast_502.Parsetree.pcl_desc = copy_class_expr_desc pcl_desc;
    Ast_502.Parsetree.pcl_loc = copy_location pcl_loc;
    Ast_502.Parsetree.pcl_attributes = copy_attributes pcl_attributes;
  }

and copy_class_expr_desc :
    Ast_501.Parsetree.class_expr_desc -> Ast_502.Parsetree.class_expr_desc =
  function
  | Ast_501.Parsetree.Pcl_constr (x0, x1) ->
      Ast_502.Parsetree.Pcl_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_501.Parsetree.Pcl_structure x0 ->
      Ast_502.Parsetree.Pcl_structure (copy_class_structure x0)
  | Ast_501.Parsetree.Pcl_fun (x0, x1, x2, x3) ->
      Ast_502.Parsetree.Pcl_fun
        ( copy_arg_label x0,
          Option.map copy_expression x1,
          copy_pattern x2,
          copy_class_expr x3 )
  | Ast_501.Parsetree.Pcl_apply (x0, x1) ->
      Ast_502.Parsetree.Pcl_apply
        ( copy_class_expr x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_501.Parsetree.Pcl_let (x0, x1, x2) ->
      Ast_502.Parsetree.Pcl_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_class_expr x2)
  | Ast_501.Parsetree.Pcl_constraint (x0, x1) ->
      Ast_502.Parsetree.Pcl_constraint (copy_class_expr x0, copy_class_type x1)
  | Ast_501.Parsetree.Pcl_extension x0 ->
      Ast_502.Parsetree.Pcl_extension (copy_extension x0)
  | Ast_501.Parsetree.Pcl_open (x0, x1) ->
      Ast_502.Parsetree.Pcl_open (copy_open_description x0, copy_class_expr x1)

and copy_class_structure :
    Ast_501.Parsetree.class_structure -> Ast_502.Parsetree.class_structure =
 fun { Ast_501.Parsetree.pcstr_self; Ast_501.Parsetree.pcstr_fields } ->
  {
    Ast_502.Parsetree.pcstr_self = copy_pattern pcstr_self;
    Ast_502.Parsetree.pcstr_fields = List.map copy_class_field pcstr_fields;
  }

and copy_class_field :
    Ast_501.Parsetree.class_field -> Ast_502.Parsetree.class_field =
 fun {
       Ast_501.Parsetree.pcf_desc;
       Ast_501.Parsetree.pcf_loc;
       Ast_501.Parsetree.pcf_attributes;
     } ->
  {
    Ast_502.Parsetree.pcf_desc = copy_class_field_desc pcf_desc;
    Ast_502.Parsetree.pcf_loc = copy_location pcf_loc;
    Ast_502.Parsetree.pcf_attributes = copy_attributes pcf_attributes;
  }

and copy_class_field_desc :
    Ast_501.Parsetree.class_field_desc -> Ast_502.Parsetree.class_field_desc =
  function
  | Ast_501.Parsetree.Pcf_inherit (x0, x1, x2) ->
      Ast_502.Parsetree.Pcf_inherit
        ( copy_override_flag x0,
          copy_class_expr x1,
          Option.map (fun x -> copy_loc (fun x -> x) x) x2 )
  | Ast_501.Parsetree.Pcf_val x0 ->
      Ast_502.Parsetree.Pcf_val
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_mutable_flag x1, copy_class_field_kind x2))
  | Ast_501.Parsetree.Pcf_method x0 ->
      Ast_502.Parsetree.Pcf_method
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_private_flag x1, copy_class_field_kind x2))
  | Ast_501.Parsetree.Pcf_constraint x0 ->
      Ast_502.Parsetree.Pcf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_501.Parsetree.Pcf_initializer x0 ->
      Ast_502.Parsetree.Pcf_initializer (copy_expression x0)
  | Ast_501.Parsetree.Pcf_attribute x0 ->
      Ast_502.Parsetree.Pcf_attribute (copy_attribute x0)
  | Ast_501.Parsetree.Pcf_extension x0 ->
      Ast_502.Parsetree.Pcf_extension (copy_extension x0)

and copy_class_field_kind :
    Ast_501.Parsetree.class_field_kind -> Ast_502.Parsetree.class_field_kind =
  function
  | Ast_501.Parsetree.Cfk_virtual x0 ->
      Ast_502.Parsetree.Cfk_virtual (copy_core_type x0)
  | Ast_501.Parsetree.Cfk_concrete (x0, x1) ->
      Ast_502.Parsetree.Cfk_concrete (copy_override_flag x0, copy_expression x1)

and copy_open_declaration :
    Ast_501.Parsetree.open_declaration -> Ast_502.Parsetree.open_declaration =
 fun x -> copy_open_infos copy_module_expr x

and copy_module_binding :
    Ast_501.Parsetree.module_binding -> Ast_502.Parsetree.module_binding =
 fun {
       Ast_501.Parsetree.pmb_name;
       Ast_501.Parsetree.pmb_expr;
       Ast_501.Parsetree.pmb_attributes;
       Ast_501.Parsetree.pmb_loc;
     } ->
  {
    Ast_502.Parsetree.pmb_name =
      copy_loc (fun x -> Option.map (fun x -> x) x) pmb_name;
    Ast_502.Parsetree.pmb_expr = copy_module_expr pmb_expr;
    Ast_502.Parsetree.pmb_attributes = copy_attributes pmb_attributes;
    Ast_502.Parsetree.pmb_loc = copy_location pmb_loc;
  }

and copy_module_expr :
    Ast_501.Parsetree.module_expr -> Ast_502.Parsetree.module_expr =
 fun {
       Ast_501.Parsetree.pmod_desc;
       Ast_501.Parsetree.pmod_loc;
       Ast_501.Parsetree.pmod_attributes;
     } ->
  {
    Ast_502.Parsetree.pmod_desc = copy_module_expr_desc pmod_desc;
    Ast_502.Parsetree.pmod_loc = copy_location pmod_loc;
    Ast_502.Parsetree.pmod_attributes = copy_attributes pmod_attributes;
  }

and copy_module_expr_desc :
    Ast_501.Parsetree.module_expr_desc -> Ast_502.Parsetree.module_expr_desc =
  function
  | Ast_501.Parsetree.Pmod_ident x0 ->
      Ast_502.Parsetree.Pmod_ident (copy_loc copy_Longident_t x0)
  | Ast_501.Parsetree.Pmod_structure x0 ->
      Ast_502.Parsetree.Pmod_structure (copy_structure x0)
  | Ast_501.Parsetree.Pmod_functor (x0, x1) ->
      Ast_502.Parsetree.Pmod_functor
        (copy_functor_parameter x0, copy_module_expr x1)
  | Ast_501.Parsetree.Pmod_apply (x0, x1) ->
      Ast_502.Parsetree.Pmod_apply (copy_module_expr x0, copy_module_expr x1)
  | Ast_501.Parsetree.Pmod_apply_unit x0 ->
      Ast_502.Parsetree.Pmod_apply_unit (copy_module_expr x0)
  | Ast_501.Parsetree.Pmod_constraint (x0, x1) ->
      Ast_502.Parsetree.Pmod_constraint
        (copy_module_expr x0, copy_module_type x1)
  | Ast_501.Parsetree.Pmod_unpack x0 ->
      Ast_502.Parsetree.Pmod_unpack (copy_expression x0)
  | Ast_501.Parsetree.Pmod_extension x0 ->
      Ast_502.Parsetree.Pmod_extension (copy_extension x0)

and copy_functor_parameter :
    Ast_501.Parsetree.functor_parameter -> Ast_502.Parsetree.functor_parameter =
  function
  | Ast_501.Parsetree.Unit -> Ast_502.Parsetree.Unit
  | Ast_501.Parsetree.Named (x0, x1) ->
      Ast_502.Parsetree.Named
        (copy_loc (fun x -> Option.map (fun x -> x) x) x0, copy_module_type x1)

and copy_module_type :
    Ast_501.Parsetree.module_type -> Ast_502.Parsetree.module_type =
 fun {
       Ast_501.Parsetree.pmty_desc;
       Ast_501.Parsetree.pmty_loc;
       Ast_501.Parsetree.pmty_attributes;
     } ->
  {
    Ast_502.Parsetree.pmty_desc = copy_module_type_desc pmty_desc;
    Ast_502.Parsetree.pmty_loc = copy_location pmty_loc;
    Ast_502.Parsetree.pmty_attributes = copy_attributes pmty_attributes;
  }

and copy_module_type_desc :
    Ast_501.Parsetree.module_type_desc -> Ast_502.Parsetree.module_type_desc =
  function
  | Ast_501.Parsetree.Pmty_ident x0 ->
      Ast_502.Parsetree.Pmty_ident (copy_loc copy_Longident_t x0)
  | Ast_501.Parsetree.Pmty_signature x0 ->
      Ast_502.Parsetree.Pmty_signature (copy_signature x0)
  | Ast_501.Parsetree.Pmty_functor (x0, x1) ->
      Ast_502.Parsetree.Pmty_functor
        (copy_functor_parameter x0, copy_module_type x1)
  | Ast_501.Parsetree.Pmty_with (x0, x1) ->
      Ast_502.Parsetree.Pmty_with
        (copy_module_type x0, List.map copy_with_constraint x1)
  | Ast_501.Parsetree.Pmty_typeof x0 ->
      Ast_502.Parsetree.Pmty_typeof (copy_module_expr x0)
  | Ast_501.Parsetree.Pmty_extension x0 ->
      Ast_502.Parsetree.Pmty_extension (copy_extension x0)
  | Ast_501.Parsetree.Pmty_alias x0 ->
      Ast_502.Parsetree.Pmty_alias (copy_loc copy_Longident_t x0)

and copy_with_constraint :
    Ast_501.Parsetree.with_constraint -> Ast_502.Parsetree.with_constraint =
  function
  | Ast_501.Parsetree.Pwith_type (x0, x1) ->
      Ast_502.Parsetree.Pwith_type
        (copy_loc copy_Longident_t x0, copy_type_declaration x1)
  | Ast_501.Parsetree.Pwith_module (x0, x1) ->
      Ast_502.Parsetree.Pwith_module
        (copy_loc copy_Longident_t x0, copy_loc copy_Longident_t x1)
  | Ast_501.Parsetree.Pwith_modtype (x0, x1) ->
      Ast_502.Parsetree.Pwith_modtype
        (copy_loc copy_Longident_t x0, copy_module_type x1)
  | Ast_501.Parsetree.Pwith_modtypesubst (x0, x1) ->
      Ast_502.Parsetree.Pwith_modtypesubst
        (copy_loc copy_Longident_t x0, copy_module_type x1)
  | Ast_501.Parsetree.Pwith_typesubst (x0, x1) ->
      Ast_502.Parsetree.Pwith_typesubst
        (copy_loc copy_Longident_t x0, copy_type_declaration x1)
  | Ast_501.Parsetree.Pwith_modsubst (x0, x1) ->
      Ast_502.Parsetree.Pwith_modsubst
        (copy_loc copy_Longident_t x0, copy_loc copy_Longident_t x1)

and copy_signature : Ast_501.Parsetree.signature -> Ast_502.Parsetree.signature
    =
 fun x -> List.map copy_signature_item x

and copy_signature_item :
    Ast_501.Parsetree.signature_item -> Ast_502.Parsetree.signature_item =
 fun { Ast_501.Parsetree.psig_desc; Ast_501.Parsetree.psig_loc } ->
  {
    Ast_502.Parsetree.psig_desc = copy_signature_item_desc psig_desc;
    Ast_502.Parsetree.psig_loc = copy_location psig_loc;
  }

and copy_signature_item_desc :
    Ast_501.Parsetree.signature_item_desc ->
    Ast_502.Parsetree.signature_item_desc = function
  | Ast_501.Parsetree.Psig_value x0 ->
      Ast_502.Parsetree.Psig_value (copy_value_description x0)
  | Ast_501.Parsetree.Psig_type (x0, x1) ->
      Ast_502.Parsetree.Psig_type
        (copy_rec_flag x0, List.map copy_type_declaration x1)
  | Ast_501.Parsetree.Psig_typesubst x0 ->
      Ast_502.Parsetree.Psig_typesubst (List.map copy_type_declaration x0)
  | Ast_501.Parsetree.Psig_typext x0 ->
      Ast_502.Parsetree.Psig_typext (copy_type_extension x0)
  | Ast_501.Parsetree.Psig_exception x0 ->
      Ast_502.Parsetree.Psig_exception (copy_type_exception x0)
  | Ast_501.Parsetree.Psig_module x0 ->
      Ast_502.Parsetree.Psig_module (copy_module_declaration x0)
  | Ast_501.Parsetree.Psig_modsubst x0 ->
      Ast_502.Parsetree.Psig_modsubst (copy_module_substitution x0)
  | Ast_501.Parsetree.Psig_recmodule x0 ->
      Ast_502.Parsetree.Psig_recmodule (List.map copy_module_declaration x0)
  | Ast_501.Parsetree.Psig_modtype x0 ->
      Ast_502.Parsetree.Psig_modtype (copy_module_type_declaration x0)
  | Ast_501.Parsetree.Psig_modtypesubst x0 ->
      Ast_502.Parsetree.Psig_modtypesubst (copy_module_type_declaration x0)
  | Ast_501.Parsetree.Psig_open x0 ->
      Ast_502.Parsetree.Psig_open (copy_open_description x0)
  | Ast_501.Parsetree.Psig_include x0 ->
      Ast_502.Parsetree.Psig_include (copy_include_description x0)
  | Ast_501.Parsetree.Psig_class x0 ->
      Ast_502.Parsetree.Psig_class (List.map copy_class_description x0)
  | Ast_501.Parsetree.Psig_class_type x0 ->
      Ast_502.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_501.Parsetree.Psig_attribute x0 ->
      Ast_502.Parsetree.Psig_attribute (copy_attribute x0)
  | Ast_501.Parsetree.Psig_extension (x0, x1) ->
      Ast_502.Parsetree.Psig_extension (copy_extension x0, copy_attributes x1)

and copy_class_type_declaration :
    Ast_501.Parsetree.class_type_declaration ->
    Ast_502.Parsetree.class_type_declaration =
 fun x -> copy_class_infos copy_class_type x

and copy_class_description :
    Ast_501.Parsetree.class_description -> Ast_502.Parsetree.class_description =
 fun x -> copy_class_infos copy_class_type x

and copy_class_type :
    Ast_501.Parsetree.class_type -> Ast_502.Parsetree.class_type =
 fun {
       Ast_501.Parsetree.pcty_desc;
       Ast_501.Parsetree.pcty_loc;
       Ast_501.Parsetree.pcty_attributes;
     } ->
  {
    Ast_502.Parsetree.pcty_desc = copy_class_type_desc pcty_desc;
    Ast_502.Parsetree.pcty_loc = copy_location pcty_loc;
    Ast_502.Parsetree.pcty_attributes = copy_attributes pcty_attributes;
  }

and copy_class_type_desc :
    Ast_501.Parsetree.class_type_desc -> Ast_502.Parsetree.class_type_desc =
  function
  | Ast_501.Parsetree.Pcty_constr (x0, x1) ->
      Ast_502.Parsetree.Pcty_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_501.Parsetree.Pcty_signature x0 ->
      Ast_502.Parsetree.Pcty_signature (copy_class_signature x0)
  | Ast_501.Parsetree.Pcty_arrow (x0, x1, x2) ->
      Ast_502.Parsetree.Pcty_arrow
        (copy_arg_label x0, copy_core_type x1, copy_class_type x2)
  | Ast_501.Parsetree.Pcty_extension x0 ->
      Ast_502.Parsetree.Pcty_extension (copy_extension x0)
  | Ast_501.Parsetree.Pcty_open (x0, x1) ->
      Ast_502.Parsetree.Pcty_open (copy_open_description x0, copy_class_type x1)

and copy_class_signature :
    Ast_501.Parsetree.class_signature -> Ast_502.Parsetree.class_signature =
 fun { Ast_501.Parsetree.pcsig_self; Ast_501.Parsetree.pcsig_fields } ->
  {
    Ast_502.Parsetree.pcsig_self = copy_core_type pcsig_self;
    Ast_502.Parsetree.pcsig_fields = List.map copy_class_type_field pcsig_fields;
  }

and copy_class_type_field :
    Ast_501.Parsetree.class_type_field -> Ast_502.Parsetree.class_type_field =
 fun {
       Ast_501.Parsetree.pctf_desc;
       Ast_501.Parsetree.pctf_loc;
       Ast_501.Parsetree.pctf_attributes;
     } ->
  {
    Ast_502.Parsetree.pctf_desc = copy_class_type_field_desc pctf_desc;
    Ast_502.Parsetree.pctf_loc = copy_location pctf_loc;
    Ast_502.Parsetree.pctf_attributes = copy_attributes pctf_attributes;
  }

and copy_class_type_field_desc :
    Ast_501.Parsetree.class_type_field_desc ->
    Ast_502.Parsetree.class_type_field_desc = function
  | Ast_501.Parsetree.Pctf_inherit x0 ->
      Ast_502.Parsetree.Pctf_inherit (copy_class_type x0)
  | Ast_501.Parsetree.Pctf_val x0 ->
      Ast_502.Parsetree.Pctf_val
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_mutable_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_501.Parsetree.Pctf_method x0 ->
      Ast_502.Parsetree.Pctf_method
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_private_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_501.Parsetree.Pctf_constraint x0 ->
      Ast_502.Parsetree.Pctf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_501.Parsetree.Pctf_attribute x0 ->
      Ast_502.Parsetree.Pctf_attribute (copy_attribute x0)
  | Ast_501.Parsetree.Pctf_extension x0 ->
      Ast_502.Parsetree.Pctf_extension (copy_extension x0)

and copy_extension : Ast_501.Parsetree.extension -> Ast_502.Parsetree.extension
    =
 fun x ->
  let x0, x1 = x in
  (copy_loc (fun x -> x) x0, copy_payload x1)

and copy_class_infos :
      'f0 'g0.
      ('f0 -> 'g0) ->
      'f0 Ast_501.Parsetree.class_infos ->
      'g0 Ast_502.Parsetree.class_infos =
 fun f0
     {
       Ast_501.Parsetree.pci_virt;
       Ast_501.Parsetree.pci_params;
       Ast_501.Parsetree.pci_name;
       Ast_501.Parsetree.pci_expr;
       Ast_501.Parsetree.pci_loc;
       Ast_501.Parsetree.pci_attributes;
     } ->
  {
    Ast_502.Parsetree.pci_virt = copy_virtual_flag pci_virt;
    Ast_502.Parsetree.pci_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        pci_params;
    Ast_502.Parsetree.pci_name = copy_loc (fun x -> x) pci_name;
    Ast_502.Parsetree.pci_expr = f0 pci_expr;
    Ast_502.Parsetree.pci_loc = copy_location pci_loc;
    Ast_502.Parsetree.pci_attributes = copy_attributes pci_attributes;
  }

and copy_virtual_flag :
    Ast_501.Asttypes.virtual_flag -> Ast_502.Asttypes.virtual_flag = function
  | Ast_501.Asttypes.Virtual -> Ast_502.Asttypes.Virtual
  | Ast_501.Asttypes.Concrete -> Ast_502.Asttypes.Concrete

and copy_include_description :
    Ast_501.Parsetree.include_description ->
    Ast_502.Parsetree.include_description =
 fun x -> copy_include_infos copy_module_type x

and copy_include_infos :
      'f0 'g0.
      ('f0 -> 'g0) ->
      'f0 Ast_501.Parsetree.include_infos ->
      'g0 Ast_502.Parsetree.include_infos =
 fun f0
     {
       Ast_501.Parsetree.pincl_mod;
       Ast_501.Parsetree.pincl_loc;
       Ast_501.Parsetree.pincl_attributes;
     } ->
  {
    Ast_502.Parsetree.pincl_mod = f0 pincl_mod;
    Ast_502.Parsetree.pincl_loc = copy_location pincl_loc;
    Ast_502.Parsetree.pincl_attributes = copy_attributes pincl_attributes;
  }

and copy_open_description :
    Ast_501.Parsetree.open_description -> Ast_502.Parsetree.open_description =
 fun x -> copy_open_infos (fun x -> copy_loc copy_Longident_t x) x

and copy_open_infos :
      'f0 'g0.
      ('f0 -> 'g0) ->
      'f0 Ast_501.Parsetree.open_infos ->
      'g0 Ast_502.Parsetree.open_infos =
 fun f0
     {
       Ast_501.Parsetree.popen_expr;
       Ast_501.Parsetree.popen_override;
       Ast_501.Parsetree.popen_loc;
       Ast_501.Parsetree.popen_attributes;
     } ->
  {
    Ast_502.Parsetree.popen_expr = f0 popen_expr;
    Ast_502.Parsetree.popen_override = copy_override_flag popen_override;
    Ast_502.Parsetree.popen_loc = copy_location popen_loc;
    Ast_502.Parsetree.popen_attributes = copy_attributes popen_attributes;
  }

and copy_override_flag :
    Ast_501.Asttypes.override_flag -> Ast_502.Asttypes.override_flag = function
  | Ast_501.Asttypes.Override -> Ast_502.Asttypes.Override
  | Ast_501.Asttypes.Fresh -> Ast_502.Asttypes.Fresh

and copy_module_type_declaration :
    Ast_501.Parsetree.module_type_declaration ->
    Ast_502.Parsetree.module_type_declaration =
 fun {
       Ast_501.Parsetree.pmtd_name;
       Ast_501.Parsetree.pmtd_type;
       Ast_501.Parsetree.pmtd_attributes;
       Ast_501.Parsetree.pmtd_loc;
     } ->
  {
    Ast_502.Parsetree.pmtd_name = copy_loc (fun x -> x) pmtd_name;
    Ast_502.Parsetree.pmtd_type = Option.map copy_module_type pmtd_type;
    Ast_502.Parsetree.pmtd_attributes = copy_attributes pmtd_attributes;
    Ast_502.Parsetree.pmtd_loc = copy_location pmtd_loc;
  }

and copy_module_substitution :
    Ast_501.Parsetree.module_substitution ->
    Ast_502.Parsetree.module_substitution =
 fun {
       Ast_501.Parsetree.pms_name;
       Ast_501.Parsetree.pms_manifest;
       Ast_501.Parsetree.pms_attributes;
       Ast_501.Parsetree.pms_loc;
     } ->
  {
    Ast_502.Parsetree.pms_name = copy_loc (fun x -> x) pms_name;
    Ast_502.Parsetree.pms_manifest = copy_loc copy_Longident_t pms_manifest;
    Ast_502.Parsetree.pms_attributes = copy_attributes pms_attributes;
    Ast_502.Parsetree.pms_loc = copy_location pms_loc;
  }

and copy_module_declaration :
    Ast_501.Parsetree.module_declaration -> Ast_502.Parsetree.module_declaration
    =
 fun {
       Ast_501.Parsetree.pmd_name;
       Ast_501.Parsetree.pmd_type;
       Ast_501.Parsetree.pmd_attributes;
       Ast_501.Parsetree.pmd_loc;
     } ->
  {
    Ast_502.Parsetree.pmd_name =
      copy_loc (fun x -> Option.map (fun x -> x) x) pmd_name;
    Ast_502.Parsetree.pmd_type = copy_module_type pmd_type;
    Ast_502.Parsetree.pmd_attributes = copy_attributes pmd_attributes;
    Ast_502.Parsetree.pmd_loc = copy_location pmd_loc;
  }

and copy_type_exception :
    Ast_501.Parsetree.type_exception -> Ast_502.Parsetree.type_exception =
 fun {
       Ast_501.Parsetree.ptyexn_constructor;
       Ast_501.Parsetree.ptyexn_loc;
       Ast_501.Parsetree.ptyexn_attributes;
     } ->
  {
    Ast_502.Parsetree.ptyexn_constructor =
      copy_extension_constructor ptyexn_constructor;
    Ast_502.Parsetree.ptyexn_loc = copy_location ptyexn_loc;
    Ast_502.Parsetree.ptyexn_attributes = copy_attributes ptyexn_attributes;
  }

and copy_type_extension :
    Ast_501.Parsetree.type_extension -> Ast_502.Parsetree.type_extension =
 fun {
       Ast_501.Parsetree.ptyext_path;
       Ast_501.Parsetree.ptyext_params;
       Ast_501.Parsetree.ptyext_constructors;
       Ast_501.Parsetree.ptyext_private;
       Ast_501.Parsetree.ptyext_loc;
       Ast_501.Parsetree.ptyext_attributes;
     } ->
  {
    Ast_502.Parsetree.ptyext_path = copy_loc copy_Longident_t ptyext_path;
    Ast_502.Parsetree.ptyext_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        ptyext_params;
    Ast_502.Parsetree.ptyext_constructors =
      List.map copy_extension_constructor ptyext_constructors;
    Ast_502.Parsetree.ptyext_private = copy_private_flag ptyext_private;
    Ast_502.Parsetree.ptyext_loc = copy_location ptyext_loc;
    Ast_502.Parsetree.ptyext_attributes = copy_attributes ptyext_attributes;
  }

and copy_extension_constructor :
    Ast_501.Parsetree.extension_constructor ->
    Ast_502.Parsetree.extension_constructor =
 fun {
       Ast_501.Parsetree.pext_name;
       Ast_501.Parsetree.pext_kind;
       Ast_501.Parsetree.pext_loc;
       Ast_501.Parsetree.pext_attributes;
     } ->
  {
    Ast_502.Parsetree.pext_name = copy_loc (fun x -> x) pext_name;
    Ast_502.Parsetree.pext_kind = copy_extension_constructor_kind pext_kind;
    Ast_502.Parsetree.pext_loc = copy_location pext_loc;
    Ast_502.Parsetree.pext_attributes = copy_attributes pext_attributes;
  }

and copy_extension_constructor_kind :
    Ast_501.Parsetree.extension_constructor_kind ->
    Ast_502.Parsetree.extension_constructor_kind = function
  | Ast_501.Parsetree.Pext_decl (x0, x1, x2) ->
      Ast_502.Parsetree.Pext_decl
        ( List.map (fun x -> copy_loc (fun x -> x) x) x0,
          copy_constructor_arguments x1,
          Option.map copy_core_type x2 )
  | Ast_501.Parsetree.Pext_rebind x0 ->
      Ast_502.Parsetree.Pext_rebind (copy_loc copy_Longident_t x0)

and copy_type_declaration :
    Ast_501.Parsetree.type_declaration -> Ast_502.Parsetree.type_declaration =
 fun {
       Ast_501.Parsetree.ptype_name;
       Ast_501.Parsetree.ptype_params;
       Ast_501.Parsetree.ptype_cstrs;
       Ast_501.Parsetree.ptype_kind;
       Ast_501.Parsetree.ptype_private;
       Ast_501.Parsetree.ptype_manifest;
       Ast_501.Parsetree.ptype_attributes;
       Ast_501.Parsetree.ptype_loc;
     } ->
  {
    Ast_502.Parsetree.ptype_name = copy_loc (fun x -> x) ptype_name;
    Ast_502.Parsetree.ptype_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        ptype_params;
    Ast_502.Parsetree.ptype_cstrs =
      List.map
        (fun x ->
          let x0, x1, x2 = x in
          (copy_core_type x0, copy_core_type x1, copy_location x2))
        ptype_cstrs;
    Ast_502.Parsetree.ptype_kind = copy_type_kind ptype_kind;
    Ast_502.Parsetree.ptype_private = copy_private_flag ptype_private;
    Ast_502.Parsetree.ptype_manifest = Option.map copy_core_type ptype_manifest;
    Ast_502.Parsetree.ptype_attributes = copy_attributes ptype_attributes;
    Ast_502.Parsetree.ptype_loc = copy_location ptype_loc;
  }

and copy_private_flag :
    Ast_501.Asttypes.private_flag -> Ast_502.Asttypes.private_flag = function
  | Ast_501.Asttypes.Private -> Ast_502.Asttypes.Private
  | Ast_501.Asttypes.Public -> Ast_502.Asttypes.Public

and copy_type_kind : Ast_501.Parsetree.type_kind -> Ast_502.Parsetree.type_kind
    = function
  | Ast_501.Parsetree.Ptype_abstract -> Ast_502.Parsetree.Ptype_abstract
  | Ast_501.Parsetree.Ptype_variant x0 ->
      Ast_502.Parsetree.Ptype_variant (List.map copy_constructor_declaration x0)
  | Ast_501.Parsetree.Ptype_record x0 ->
      Ast_502.Parsetree.Ptype_record (List.map copy_label_declaration x0)
  | Ast_501.Parsetree.Ptype_open -> Ast_502.Parsetree.Ptype_open

and copy_constructor_declaration :
    Ast_501.Parsetree.constructor_declaration ->
    Ast_502.Parsetree.constructor_declaration =
 fun {
       Ast_501.Parsetree.pcd_name;
       Ast_501.Parsetree.pcd_vars;
       Ast_501.Parsetree.pcd_args;
       Ast_501.Parsetree.pcd_res;
       Ast_501.Parsetree.pcd_loc;
       Ast_501.Parsetree.pcd_attributes;
     } ->
  {
    Ast_502.Parsetree.pcd_name = copy_loc (fun x -> x) pcd_name;
    Ast_502.Parsetree.pcd_vars =
      List.map (fun x -> copy_loc (fun x -> x) x) pcd_vars;
    Ast_502.Parsetree.pcd_args = copy_constructor_arguments pcd_args;
    Ast_502.Parsetree.pcd_res = Option.map copy_core_type pcd_res;
    Ast_502.Parsetree.pcd_loc = copy_location pcd_loc;
    Ast_502.Parsetree.pcd_attributes = copy_attributes pcd_attributes;
  }

and copy_constructor_arguments :
    Ast_501.Parsetree.constructor_arguments ->
    Ast_502.Parsetree.constructor_arguments = function
  | Ast_501.Parsetree.Pcstr_tuple x0 ->
      Ast_502.Parsetree.Pcstr_tuple (List.map copy_core_type x0)
  | Ast_501.Parsetree.Pcstr_record x0 ->
      Ast_502.Parsetree.Pcstr_record (List.map copy_label_declaration x0)

and copy_label_declaration :
    Ast_501.Parsetree.label_declaration -> Ast_502.Parsetree.label_declaration =
 fun {
       Ast_501.Parsetree.pld_name;
       Ast_501.Parsetree.pld_mutable;
       Ast_501.Parsetree.pld_type;
       Ast_501.Parsetree.pld_loc;
       Ast_501.Parsetree.pld_attributes;
     } ->
  {
    Ast_502.Parsetree.pld_name = copy_loc (fun x -> x) pld_name;
    Ast_502.Parsetree.pld_mutable = copy_mutable_flag pld_mutable;
    Ast_502.Parsetree.pld_type = copy_core_type pld_type;
    Ast_502.Parsetree.pld_loc = copy_location pld_loc;
    Ast_502.Parsetree.pld_attributes = copy_attributes pld_attributes;
  }

and copy_mutable_flag :
    Ast_501.Asttypes.mutable_flag -> Ast_502.Asttypes.mutable_flag = function
  | Ast_501.Asttypes.Immutable -> Ast_502.Asttypes.Immutable
  | Ast_501.Asttypes.Mutable -> Ast_502.Asttypes.Mutable

and copy_injectivity :
    Ast_501.Asttypes.injectivity -> Ast_502.Asttypes.injectivity = function
  | Ast_501.Asttypes.Injective -> Ast_502.Asttypes.Injective
  | Ast_501.Asttypes.NoInjectivity -> Ast_502.Asttypes.NoInjectivity

and copy_variance : Ast_501.Asttypes.variance -> Ast_502.Asttypes.variance =
  function
  | Ast_501.Asttypes.Covariant -> Ast_502.Asttypes.Covariant
  | Ast_501.Asttypes.Contravariant -> Ast_502.Asttypes.Contravariant
  | Ast_501.Asttypes.NoVariance -> Ast_502.Asttypes.NoVariance

and copy_value_description :
    Ast_501.Parsetree.value_description -> Ast_502.Parsetree.value_description =
 fun {
       Ast_501.Parsetree.pval_name;
       Ast_501.Parsetree.pval_type;
       Ast_501.Parsetree.pval_prim;
       Ast_501.Parsetree.pval_attributes;
       Ast_501.Parsetree.pval_loc;
     } ->
  {
    Ast_502.Parsetree.pval_name = copy_loc (fun x -> x) pval_name;
    Ast_502.Parsetree.pval_type = copy_core_type pval_type;
    Ast_502.Parsetree.pval_prim = List.map (fun x -> x) pval_prim;
    Ast_502.Parsetree.pval_attributes = copy_attributes pval_attributes;
    Ast_502.Parsetree.pval_loc = copy_location pval_loc;
  }

and copy_object_field_desc :
    Ast_501.Parsetree.object_field_desc -> Ast_502.Parsetree.object_field_desc =
  function
  | Ast_501.Parsetree.Otag (x0, x1) ->
      Ast_502.Parsetree.Otag (copy_loc copy_label x0, copy_core_type x1)
  | Ast_501.Parsetree.Oinherit x0 ->
      Ast_502.Parsetree.Oinherit (copy_core_type x0)

and copy_arg_label : Ast_501.Asttypes.arg_label -> Ast_502.Asttypes.arg_label =
  function
  | Ast_501.Asttypes.Nolabel -> Ast_502.Asttypes.Nolabel
  | Ast_501.Asttypes.Labelled x0 -> Ast_502.Asttypes.Labelled x0
  | Ast_501.Asttypes.Optional x0 -> Ast_502.Asttypes.Optional x0

and copy_closed_flag :
    Ast_501.Asttypes.closed_flag -> Ast_502.Asttypes.closed_flag = function
  | Ast_501.Asttypes.Closed -> Ast_502.Asttypes.Closed
  | Ast_501.Asttypes.Open -> Ast_502.Asttypes.Open

and copy_label : Ast_501.Asttypes.label -> Ast_502.Asttypes.label = fun x -> x

and copy_rec_flag : Ast_501.Asttypes.rec_flag -> Ast_502.Asttypes.rec_flag =
  function
  | Ast_501.Asttypes.Nonrecursive -> Ast_502.Asttypes.Nonrecursive
  | Ast_501.Asttypes.Recursive -> Ast_502.Asttypes.Recursive

and copy_constant : Ast_501.Parsetree.constant -> Ast_502.Parsetree.constant =
  function
  | Ast_501.Parsetree.Pconst_integer (x0, x1) ->
      Ast_502.Parsetree.Pconst_integer (x0, Option.map (fun x -> x) x1)
  | Ast_501.Parsetree.Pconst_char x0 -> Ast_502.Parsetree.Pconst_char x0
  | Ast_501.Parsetree.Pconst_string (x0, x1, x2) ->
      Ast_502.Parsetree.Pconst_string
        (x0, copy_location x1, Option.map (fun x -> x) x2)
  | Ast_501.Parsetree.Pconst_float (x0, x1) ->
      Ast_502.Parsetree.Pconst_float (x0, Option.map (fun x -> x) x1)

and copy_Longident_t : Longident.t -> Longident.t = function
  | Longident.Lident x0 -> Longident.Lident x0
  | Longident.Ldot (x0, x1) -> Longident.Ldot (copy_Longident_t x0, x1)
  | Longident.Lapply (x0, x1) ->
      Longident.Lapply (copy_Longident_t x0, copy_Longident_t x1)

and copy_loc :
      'f0 'g0.
      ('f0 -> 'g0) -> 'f0 Ast_501.Asttypes.loc -> 'g0 Ast_502.Asttypes.loc =
 fun f0 { Ast_501.Asttypes.txt; Ast_501.Asttypes.loc } ->
  { Ast_502.Asttypes.txt = f0 txt; Ast_502.Asttypes.loc = copy_location loc }

and copy_location : Location.t -> Location.t = fun x -> x
