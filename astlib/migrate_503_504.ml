open Stdlib0
module From = Ast_503
module To = Ast_504

let rec copy_toplevel_phrase :
    Ast_503.Parsetree.toplevel_phrase -> Ast_504.Parsetree.toplevel_phrase =
  function
  | Ast_503.Parsetree.Ptop_def x0 ->
      Ast_504.Parsetree.Ptop_def (copy_structure x0)
  | Ast_503.Parsetree.Ptop_dir x0 ->
      Ast_504.Parsetree.Ptop_dir (copy_toplevel_directive x0)

and copy_toplevel_directive :
    Ast_503.Parsetree.toplevel_directive -> Ast_504.Parsetree.toplevel_directive
    =
 fun {
       Ast_503.Parsetree.pdir_name;
       Ast_503.Parsetree.pdir_arg;
       Ast_503.Parsetree.pdir_loc;
     } ->
  {
    Ast_504.Parsetree.pdir_name = copy_loc (fun x -> x) pdir_name;
    Ast_504.Parsetree.pdir_arg = Option.map copy_directive_argument pdir_arg;
    Ast_504.Parsetree.pdir_loc = copy_location pdir_loc;
  }

and copy_directive_argument :
    Ast_503.Parsetree.directive_argument -> Ast_504.Parsetree.directive_argument
    =
 fun { Ast_503.Parsetree.pdira_desc; Ast_503.Parsetree.pdira_loc } ->
  {
    Ast_504.Parsetree.pdira_desc = copy_directive_argument_desc pdira_desc;
    Ast_504.Parsetree.pdira_loc = copy_location pdira_loc;
  }

and copy_directive_argument_desc :
    Ast_503.Parsetree.directive_argument_desc ->
    Ast_504.Parsetree.directive_argument_desc = function
  | Ast_503.Parsetree.Pdir_string x0 -> Ast_504.Parsetree.Pdir_string x0
  | Ast_503.Parsetree.Pdir_int (x0, x1) ->
      Ast_504.Parsetree.Pdir_int (x0, Option.map (fun x -> x) x1)
  | Ast_503.Parsetree.Pdir_ident x0 ->
      Ast_504.Parsetree.Pdir_ident (copy_Longident_t x0)
  | Ast_503.Parsetree.Pdir_bool x0 -> Ast_504.Parsetree.Pdir_bool x0

and copy_expression :
    Ast_503.Parsetree.expression -> Ast_504.Parsetree.expression =
 fun {
       Ast_503.Parsetree.pexp_desc;
       Ast_503.Parsetree.pexp_loc;
       Ast_503.Parsetree.pexp_loc_stack;
       Ast_503.Parsetree.pexp_attributes;
     } ->
  let loc = copy_location pexp_loc in
  {
    Ast_504.Parsetree.pexp_desc = copy_expression_desc_with_loc ~loc pexp_desc;
    Ast_504.Parsetree.pexp_loc = loc;
    Ast_504.Parsetree.pexp_loc_stack = copy_location_stack pexp_loc_stack;
    Ast_504.Parsetree.pexp_attributes = copy_attributes pexp_attributes;
  }

and copy_expression_desc :
    Ast_503.Parsetree.expression_desc -> Ast_504.Parsetree.expression_desc =
 fun desc -> copy_expression_desc_with_loc ~loc:Location.none desc

and copy_expression_desc_with_loc :
    loc:Location.t ->
    Ast_503.Parsetree.expression_desc ->
    Ast_504.Parsetree.expression_desc =
 fun ~loc desc ->
  match desc with
  | Ast_503.Parsetree.Pexp_ident x0 ->
      Ast_504.Parsetree.Pexp_ident (copy_loc copy_Longident_t x0)
  | Ast_503.Parsetree.Pexp_constant x0 ->
      let loc = { loc with loc_ghost = true } in
      let constant = { (copy_constant x0) with pconst_loc = loc } in
      Ast_504.Parsetree.Pexp_constant constant
  | Ast_503.Parsetree.Pexp_let (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_expression x2)
  | Ast_503.Parsetree.Pexp_function (params, typ_constraint, fun_body) ->
      let params = List.map copy_function_param params in
      let typ_constraint = Option.map copy_type_constraint typ_constraint in
      let fun_body = copy_function_body fun_body in
      Ast_504.Parsetree.Pexp_function (params, typ_constraint, fun_body)
  | Ast_503.Parsetree.Pexp_apply (x0, x1) ->
      Ast_504.Parsetree.Pexp_apply
        ( copy_expression x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_503.Parsetree.Pexp_match (x0, x1) ->
      Ast_504.Parsetree.Pexp_match (copy_expression x0, List.map copy_case x1)
  | Ast_503.Parsetree.Pexp_try (x0, x1) ->
      Ast_504.Parsetree.Pexp_try (copy_expression x0, List.map copy_case x1)
  | Ast_503.Parsetree.Pexp_tuple x0 ->
      Ast_504.Parsetree.Pexp_tuple
        (List.map (fun v -> (None, copy_expression v)) x0)
  | Ast_503.Parsetree.Pexp_construct (x0, x1) ->
      Ast_504.Parsetree.Pexp_construct
        (copy_loc copy_Longident_t x0, Option.map copy_expression x1)
  | Ast_503.Parsetree.Pexp_variant (x0, x1) ->
      Ast_504.Parsetree.Pexp_variant
        (copy_label x0, Option.map copy_expression x1)
  | Ast_503.Parsetree.Pexp_record (x0, x1) ->
      Ast_504.Parsetree.Pexp_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_Longident_t x0, copy_expression x1))
            x0,
          Option.map copy_expression x1 )
  | Ast_503.Parsetree.Pexp_field (x0, x1) ->
      Ast_504.Parsetree.Pexp_field
        (copy_expression x0, copy_loc copy_Longident_t x1)
  | Ast_503.Parsetree.Pexp_setfield (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_setfield
        (copy_expression x0, copy_loc copy_Longident_t x1, copy_expression x2)
  | Ast_503.Parsetree.Pexp_array x0 ->
      Ast_504.Parsetree.Pexp_array (List.map copy_expression x0)
  | Ast_503.Parsetree.Pexp_ifthenelse (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_ifthenelse
        (copy_expression x0, copy_expression x1, Option.map copy_expression x2)
  | Ast_503.Parsetree.Pexp_sequence (x0, x1) ->
      Ast_504.Parsetree.Pexp_sequence (copy_expression x0, copy_expression x1)
  | Ast_503.Parsetree.Pexp_while (x0, x1) ->
      Ast_504.Parsetree.Pexp_while (copy_expression x0, copy_expression x1)
  | Ast_503.Parsetree.Pexp_for (x0, x1, x2, x3, x4) ->
      Ast_504.Parsetree.Pexp_for
        ( copy_pattern x0,
          copy_expression x1,
          copy_expression x2,
          copy_direction_flag x3,
          copy_expression x4 )
  | Ast_503.Parsetree.Pexp_constraint (x0, x1) ->
      Ast_504.Parsetree.Pexp_constraint (copy_expression x0, copy_core_type x1)
  | Ast_503.Parsetree.Pexp_coerce (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_coerce
        (copy_expression x0, Option.map copy_core_type x1, copy_core_type x2)
  | Ast_503.Parsetree.Pexp_send (x0, x1) ->
      Ast_504.Parsetree.Pexp_send (copy_expression x0, copy_loc copy_label x1)
  | Ast_503.Parsetree.Pexp_new x0 ->
      Ast_504.Parsetree.Pexp_new (copy_loc copy_Longident_t x0)
  | Ast_503.Parsetree.Pexp_setinstvar (x0, x1) ->
      Ast_504.Parsetree.Pexp_setinstvar
        (copy_loc copy_label x0, copy_expression x1)
  | Ast_503.Parsetree.Pexp_override x0 ->
      Ast_504.Parsetree.Pexp_override
        (List.map
           (fun x ->
             let x0, x1 = x in
             (copy_loc copy_label x0, copy_expression x1))
           x0)
  | Ast_503.Parsetree.Pexp_letmodule (x0, x1, x2) ->
      Ast_504.Parsetree.Pexp_letmodule
        ( copy_loc (fun x -> Option.map (fun x -> x) x) x0,
          copy_module_expr x1,
          copy_expression x2 )
  | Ast_503.Parsetree.Pexp_letexception (x0, x1) ->
      Ast_504.Parsetree.Pexp_letexception
        (copy_extension_constructor x0, copy_expression x1)
  | Ast_503.Parsetree.Pexp_assert x0 ->
      Ast_504.Parsetree.Pexp_assert (copy_expression x0)
  | Ast_503.Parsetree.Pexp_lazy x0 ->
      Ast_504.Parsetree.Pexp_lazy (copy_expression x0)
  | Ast_503.Parsetree.Pexp_poly (x0, x1) ->
      Ast_504.Parsetree.Pexp_poly
        (copy_expression x0, Option.map copy_core_type x1)
  | Ast_503.Parsetree.Pexp_object x0 ->
      Ast_504.Parsetree.Pexp_object (copy_class_structure x0)
  | Ast_503.Parsetree.Pexp_newtype (x0, x1) ->
      Ast_504.Parsetree.Pexp_newtype
        (copy_loc (fun x -> x) x0, copy_expression x1)
  | Ast_503.Parsetree.Pexp_pack x0 ->
      Ast_504.Parsetree.Pexp_pack (copy_module_expr x0, None)
  | Ast_503.Parsetree.Pexp_open (x0, x1) ->
      Ast_504.Parsetree.Pexp_open (copy_open_declaration x0, copy_expression x1)
  | Ast_503.Parsetree.Pexp_letop x0 ->
      Ast_504.Parsetree.Pexp_letop (copy_letop x0)
  | Ast_503.Parsetree.Pexp_extension x0 ->
      Ast_504.Parsetree.Pexp_extension (copy_extension x0)
  | Ast_503.Parsetree.Pexp_unreachable -> Ast_504.Parsetree.Pexp_unreachable

and copy_letop : Ast_503.Parsetree.letop -> Ast_504.Parsetree.letop =
 fun { Ast_503.Parsetree.let_; Ast_503.Parsetree.ands; Ast_503.Parsetree.body } ->
  {
    Ast_504.Parsetree.let_ = copy_binding_op let_;
    Ast_504.Parsetree.ands = List.map copy_binding_op ands;
    Ast_504.Parsetree.body = copy_expression body;
  }

and copy_binding_op :
    Ast_503.Parsetree.binding_op -> Ast_504.Parsetree.binding_op =
 fun {
       Ast_503.Parsetree.pbop_op;
       Ast_503.Parsetree.pbop_pat;
       Ast_503.Parsetree.pbop_exp;
       Ast_503.Parsetree.pbop_loc;
     } ->
  {
    Ast_504.Parsetree.pbop_op = copy_loc (fun x -> x) pbop_op;
    Ast_504.Parsetree.pbop_pat = copy_pattern pbop_pat;
    Ast_504.Parsetree.pbop_exp = copy_expression pbop_exp;
    Ast_504.Parsetree.pbop_loc = copy_location pbop_loc;
  }

and copy_function_param_desc :
    Ast_503.Parsetree.function_param_desc ->
    Ast_504.Parsetree.function_param_desc = function
  | Ast_503.Parsetree.Pparam_val (l, e, p) ->
      Ast_504.Parsetree.Pparam_val
        (copy_arg_label l, Option.map copy_expression e, copy_pattern p)
  | Ast_503.Parsetree.Pparam_newtype x ->
      Ast_504.Parsetree.Pparam_newtype (copy_loc (fun x -> x) x)

and copy_function_param :
    Ast_503.Parsetree.function_param -> Ast_504.Parsetree.function_param =
 fun { Ast_503.Parsetree.pparam_loc; pparam_desc } ->
  {
    Ast_504.Parsetree.pparam_loc = copy_location pparam_loc;
    pparam_desc = copy_function_param_desc pparam_desc;
  }

and copy_function_body :
    Ast_503.Parsetree.function_body -> Ast_504.Parsetree.function_body =
  function
  | Ast_503.Parsetree.Pfunction_body e ->
      Ast_504.Parsetree.Pfunction_body (copy_expression e)
  | Ast_503.Parsetree.Pfunction_cases (cases, loc, attributes) ->
      Ast_504.Parsetree.Pfunction_cases
        (List.map copy_case cases, copy_location loc, copy_attributes attributes)

and copy_type_constraint :
    Ast_503.Parsetree.type_constraint -> Ast_504.Parsetree.type_constraint =
  function
  | Ast_503.Parsetree.Pconstraint t ->
      Ast_504.Parsetree.Pconstraint (copy_core_type t)
  | Ast_503.Parsetree.Pcoerce (t1, t2) ->
      Ast_504.Parsetree.Pcoerce (Option.map copy_core_type t1, copy_core_type t2)

and copy_direction_flag :
    Ast_503.Asttypes.direction_flag -> Ast_504.Asttypes.direction_flag =
  function
  | Ast_503.Asttypes.Upto -> Ast_504.Asttypes.Upto
  | Ast_503.Asttypes.Downto -> Ast_504.Asttypes.Downto

and copy_case : Ast_503.Parsetree.case -> Ast_504.Parsetree.case =
 fun {
       Ast_503.Parsetree.pc_lhs;
       Ast_503.Parsetree.pc_guard;
       Ast_503.Parsetree.pc_rhs;
     } ->
  {
    Ast_504.Parsetree.pc_lhs = copy_pattern pc_lhs;
    Ast_504.Parsetree.pc_guard = Option.map copy_expression pc_guard;
    Ast_504.Parsetree.pc_rhs = copy_expression pc_rhs;
  }

and copy_value_binding :
    Ast_503.Parsetree.value_binding -> Ast_504.Parsetree.value_binding =
 fun {
       Ast_503.Parsetree.pvb_pat;
       Ast_503.Parsetree.pvb_expr;
       Ast_503.Parsetree.pvb_constraint;
       Ast_503.Parsetree.pvb_attributes;
       Ast_503.Parsetree.pvb_loc;
     } ->
  {
    Ast_504.Parsetree.pvb_pat = copy_pattern pvb_pat;
    Ast_504.Parsetree.pvb_expr = copy_expression pvb_expr;
    Ast_504.Parsetree.pvb_constraint =
      Option.map copy_value_constraint pvb_constraint;
    Ast_504.Parsetree.pvb_attributes = copy_attributes pvb_attributes;
    Ast_504.Parsetree.pvb_loc = copy_location pvb_loc;
  }

and copy_pattern : Ast_503.Parsetree.pattern -> Ast_504.Parsetree.pattern =
 fun {
       Ast_503.Parsetree.ppat_desc;
       Ast_503.Parsetree.ppat_loc;
       Ast_503.Parsetree.ppat_loc_stack;
       Ast_503.Parsetree.ppat_attributes;
     } ->
  let loc = copy_location ppat_loc in
  {
    Ast_504.Parsetree.ppat_desc = copy_pattern_desc_with_loc ~loc ppat_desc;
    Ast_504.Parsetree.ppat_loc = loc;
    Ast_504.Parsetree.ppat_loc_stack = copy_location_stack ppat_loc_stack;
    Ast_504.Parsetree.ppat_attributes = copy_attributes ppat_attributes;
  }

and copy_pattern_desc :
    Ast_503.Parsetree.pattern_desc -> Ast_504.Parsetree.pattern_desc =
 fun desc -> copy_pattern_desc_with_loc ~loc:Location.none desc

and copy_pattern_desc_with_loc :
    loc:Location.t ->
    Ast_503.Parsetree.pattern_desc ->
    Ast_504.Parsetree.pattern_desc =
 fun ~loc desc ->
  match desc with
  | Ast_503.Parsetree.Ppat_any -> Ast_504.Parsetree.Ppat_any
  | Ast_503.Parsetree.Ppat_var x0 ->
      Ast_504.Parsetree.Ppat_var (copy_loc (fun x -> x) x0)
  | Ast_503.Parsetree.Ppat_alias (x0, x1) ->
      Ast_504.Parsetree.Ppat_alias (copy_pattern x0, copy_loc (fun x -> x) x1)
  | Ast_503.Parsetree.Ppat_constant x0 ->
      let loc = { loc with loc_ghost = true } in
      let constant = { (copy_constant x0) with pconst_loc = loc } in
      Ast_504.Parsetree.Ppat_constant constant
  | Ast_503.Parsetree.Ppat_interval (x0, x1) ->
      let loc = { loc with loc_ghost = true } in
      let constant0 = { (copy_constant x0) with pconst_loc = loc } in
      let constant1 = { (copy_constant x1) with pconst_loc = loc } in
      Ast_504.Parsetree.Ppat_interval (constant0, constant1)
  | Ast_503.Parsetree.Ppat_tuple x0 ->
      Ast_504.Parsetree.Ppat_tuple
        (List.map (fun v -> (None, copy_pattern v)) x0, Closed)
  | Ast_503.Parsetree.Ppat_construct (x0, x1) ->
      Ast_504.Parsetree.Ppat_construct
        ( copy_loc copy_Longident_t x0,
          Option.map
            (fun x ->
              let x0, x1 = x in
              (List.map (fun x -> copy_loc (fun x -> x) x) x0, copy_pattern x1))
            x1 )
  | Ast_503.Parsetree.Ppat_variant (x0, x1) ->
      Ast_504.Parsetree.Ppat_variant (copy_label x0, Option.map copy_pattern x1)
  | Ast_503.Parsetree.Ppat_record (x0, x1) ->
      Ast_504.Parsetree.Ppat_record
        ( List.map
            (fun x ->
              let x0, x1 = x in
              (copy_loc copy_Longident_t x0, copy_pattern x1))
            x0,
          copy_closed_flag x1 )
  | Ast_503.Parsetree.Ppat_array x0 ->
      Ast_504.Parsetree.Ppat_array (List.map copy_pattern x0)
  | Ast_503.Parsetree.Ppat_or (x0, x1) ->
      Ast_504.Parsetree.Ppat_or (copy_pattern x0, copy_pattern x1)
  | Ast_503.Parsetree.Ppat_constraint (x0, x1) ->
      Ast_504.Parsetree.Ppat_constraint (copy_pattern x0, copy_core_type x1)
  | Ast_503.Parsetree.Ppat_type x0 ->
      Ast_504.Parsetree.Ppat_type (copy_loc copy_Longident_t x0)
  | Ast_503.Parsetree.Ppat_lazy x0 ->
      Ast_504.Parsetree.Ppat_lazy (copy_pattern x0)
  | Ast_503.Parsetree.Ppat_unpack x0 ->
      Ast_504.Parsetree.Ppat_unpack
        (copy_loc (fun x -> Option.map (fun x -> x) x) x0)
  | Ast_503.Parsetree.Ppat_exception x0 ->
      Ast_504.Parsetree.Ppat_exception (copy_pattern x0)
  | Ast_503.Parsetree.Ppat_extension
      ( { txt = "ppxlib.migration.ppat_effect"; _ },
        PPat ({ ppat_desc = Ppat_tuple [ e; c ]; _ }, None) ) ->
      Ast_504.Parsetree.Ppat_effect (copy_pattern e, copy_pattern c)
  | Ast_503.Parsetree.Ppat_extension x0 ->
      Ast_504.Parsetree.Ppat_extension (copy_extension x0)
  | Ast_503.Parsetree.Ppat_open (x0, x1) ->
      Ast_504.Parsetree.Ppat_open (copy_loc copy_Longident_t x0, copy_pattern x1)
  | Ast_503.Parsetree.Ppat_effect (x0, x1) ->
      Ast_504.Parsetree.Ppat_effect (copy_pattern x0, copy_pattern x1)

and copy_value_constraint :
    Ast_503.Parsetree.value_constraint -> Ast_504.Parsetree.value_constraint =
  function
  | Ast_503.Parsetree.Pvc_constraint { locally_abstract_univars; typ } ->
      Ast_504.Parsetree.Pvc_constraint
        {
          locally_abstract_univars =
            List.map (copy_loc (fun x -> x)) locally_abstract_univars;
          typ = copy_core_type typ;
        }
  | Ast_503.Parsetree.Pvc_coercion { ground; coercion } ->
      Ast_504.Parsetree.Pvc_coercion
        {
          ground = Option.map copy_core_type ground;
          coercion = copy_core_type coercion;
        }

and copy_core_type : Ast_503.Parsetree.core_type -> Ast_504.Parsetree.core_type
    =
 fun {
       Ast_503.Parsetree.ptyp_desc;
       Ast_503.Parsetree.ptyp_loc;
       Ast_503.Parsetree.ptyp_loc_stack;
       Ast_503.Parsetree.ptyp_attributes;
     } ->
  {
    Ast_504.Parsetree.ptyp_desc = copy_core_type_desc ptyp_desc;
    Ast_504.Parsetree.ptyp_loc = copy_location ptyp_loc;
    Ast_504.Parsetree.ptyp_loc_stack = copy_location_stack ptyp_loc_stack;
    Ast_504.Parsetree.ptyp_attributes = copy_attributes ptyp_attributes;
  }

and copy_location_stack :
    Ast_503.Parsetree.location_stack -> Ast_504.Parsetree.location_stack =
 fun x -> List.map copy_location x

and copy_core_type_desc :
    Ast_503.Parsetree.core_type_desc -> Ast_504.Parsetree.core_type_desc =
  function
  | Ast_503.Parsetree.Ptyp_any -> Ast_504.Parsetree.Ptyp_any
  | Ast_503.Parsetree.Ptyp_var x0 -> Ast_504.Parsetree.Ptyp_var x0
  | Ast_503.Parsetree.Ptyp_arrow (x0, x1, x2) ->
      Ast_504.Parsetree.Ptyp_arrow
        (copy_arg_label x0, copy_core_type x1, copy_core_type x2)
  | Ast_503.Parsetree.Ptyp_tuple x0 ->
      Ast_504.Parsetree.Ptyp_tuple
        (List.map (fun v -> (None, copy_core_type v)) x0)
  | Ast_503.Parsetree.Ptyp_constr (x0, x1) ->
      Ast_504.Parsetree.Ptyp_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_503.Parsetree.Ptyp_object (x0, x1) ->
      Ast_504.Parsetree.Ptyp_object
        (List.map copy_object_field x0, copy_closed_flag x1)
  | Ast_503.Parsetree.Ptyp_class (x0, x1) ->
      Ast_504.Parsetree.Ptyp_class
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_503.Parsetree.Ptyp_alias (x0, x1) ->
      Ast_504.Parsetree.Ptyp_alias (copy_core_type x0, copy_loc (fun x -> x) x1)
  | Ast_503.Parsetree.Ptyp_variant (x0, x1, x2) ->
      Ast_504.Parsetree.Ptyp_variant
        ( List.map copy_row_field x0,
          copy_closed_flag x1,
          Option.map (fun x -> List.map copy_label x) x2 )
  | Ast_503.Parsetree.Ptyp_poly (x0, x1) ->
      Ast_504.Parsetree.Ptyp_poly
        (List.map (fun x -> copy_loc (fun x -> x) x) x0, copy_core_type x1)
  | Ast_503.Parsetree.Ptyp_package x0 ->
      Ast_504.Parsetree.Ptyp_package (copy_package_type x0)
  | Ast_503.Parsetree.Ptyp_open (x0, ty) ->
      Ast_504.Parsetree.Ptyp_open
        (copy_loc copy_Longident_t x0, copy_core_type ty)
  | Ast_503.Parsetree.Ptyp_extension x0 ->
      Ast_504.Parsetree.Ptyp_extension (copy_extension x0)

and copy_package_type :
    Ast_503.Parsetree.package_type -> Ast_504.Parsetree.package_type =
 fun x ->
  let x0, x1 = x in
  {
    ppt_path = copy_loc copy_Longident_t x0;
    ppt_cstrs =
      List.map
        (fun x ->
          let x0, x1 = x in
          (copy_loc copy_Longident_t x0, copy_core_type x1))
        x1;
    ppt_loc = Location.none;
    ppt_attrs = [];
  }

and copy_row_field : Ast_503.Parsetree.row_field -> Ast_504.Parsetree.row_field
    =
 fun {
       Ast_503.Parsetree.prf_desc;
       Ast_503.Parsetree.prf_loc;
       Ast_503.Parsetree.prf_attributes;
     } ->
  {
    Ast_504.Parsetree.prf_desc = copy_row_field_desc prf_desc;
    Ast_504.Parsetree.prf_loc = copy_location prf_loc;
    Ast_504.Parsetree.prf_attributes = copy_attributes prf_attributes;
  }

and copy_row_field_desc :
    Ast_503.Parsetree.row_field_desc -> Ast_504.Parsetree.row_field_desc =
  function
  | Ast_503.Parsetree.Rtag (x0, x1, x2) ->
      Ast_504.Parsetree.Rtag
        (copy_loc copy_label x0, x1, List.map copy_core_type x2)
  | Ast_503.Parsetree.Rinherit x0 ->
      Ast_504.Parsetree.Rinherit (copy_core_type x0)

and copy_object_field :
    Ast_503.Parsetree.object_field -> Ast_504.Parsetree.object_field =
 fun {
       Ast_503.Parsetree.pof_desc;
       Ast_503.Parsetree.pof_loc;
       Ast_503.Parsetree.pof_attributes;
     } ->
  {
    Ast_504.Parsetree.pof_desc = copy_object_field_desc pof_desc;
    Ast_504.Parsetree.pof_loc = copy_location pof_loc;
    Ast_504.Parsetree.pof_attributes = copy_attributes pof_attributes;
  }

and copy_attributes :
    Ast_503.Parsetree.attributes -> Ast_504.Parsetree.attributes =
 fun x -> List.map copy_attribute x

and copy_attribute : Ast_503.Parsetree.attribute -> Ast_504.Parsetree.attribute
    =
 fun {
       Ast_503.Parsetree.attr_name;
       Ast_503.Parsetree.attr_payload;
       Ast_503.Parsetree.attr_loc;
     } ->
  {
    Ast_504.Parsetree.attr_name = copy_loc (fun x -> x) attr_name;
    Ast_504.Parsetree.attr_payload = copy_payload attr_payload;
    Ast_504.Parsetree.attr_loc = copy_location attr_loc;
  }

and copy_payload : Ast_503.Parsetree.payload -> Ast_504.Parsetree.payload =
  function
  | Ast_503.Parsetree.PStr x0 -> Ast_504.Parsetree.PStr (copy_structure x0)
  | Ast_503.Parsetree.PSig x0 -> Ast_504.Parsetree.PSig (copy_signature x0)
  | Ast_503.Parsetree.PTyp x0 -> Ast_504.Parsetree.PTyp (copy_core_type x0)
  | Ast_503.Parsetree.PPat (x0, x1) ->
      Ast_504.Parsetree.PPat (copy_pattern x0, Option.map copy_expression x1)

and copy_structure : Ast_503.Parsetree.structure -> Ast_504.Parsetree.structure
    =
 fun x -> List.map copy_structure_item x

and copy_structure_item :
    Ast_503.Parsetree.structure_item -> Ast_504.Parsetree.structure_item =
 fun { Ast_503.Parsetree.pstr_desc; Ast_503.Parsetree.pstr_loc } ->
  {
    Ast_504.Parsetree.pstr_desc = copy_structure_item_desc pstr_desc;
    Ast_504.Parsetree.pstr_loc = copy_location pstr_loc;
  }

and copy_structure_item_desc :
    Ast_503.Parsetree.structure_item_desc ->
    Ast_504.Parsetree.structure_item_desc = function
  | Ast_503.Parsetree.Pstr_eval (x0, x1) ->
      Ast_504.Parsetree.Pstr_eval (copy_expression x0, copy_attributes x1)
  | Ast_503.Parsetree.Pstr_value (x0, x1) ->
      Ast_504.Parsetree.Pstr_value
        (copy_rec_flag x0, List.map copy_value_binding x1)
  | Ast_503.Parsetree.Pstr_primitive x0 ->
      Ast_504.Parsetree.Pstr_primitive (copy_value_description x0)
  | Ast_503.Parsetree.Pstr_type (x0, x1) ->
      Ast_504.Parsetree.Pstr_type
        (copy_rec_flag x0, List.map copy_type_declaration x1)
  | Ast_503.Parsetree.Pstr_typext x0 ->
      Ast_504.Parsetree.Pstr_typext (copy_type_extension x0)
  | Ast_503.Parsetree.Pstr_exception x0 ->
      Ast_504.Parsetree.Pstr_exception (copy_type_exception x0)
  | Ast_503.Parsetree.Pstr_module x0 ->
      Ast_504.Parsetree.Pstr_module (copy_module_binding x0)
  | Ast_503.Parsetree.Pstr_recmodule x0 ->
      Ast_504.Parsetree.Pstr_recmodule (List.map copy_module_binding x0)
  | Ast_503.Parsetree.Pstr_modtype x0 ->
      Ast_504.Parsetree.Pstr_modtype (copy_module_type_declaration x0)
  | Ast_503.Parsetree.Pstr_open x0 ->
      Ast_504.Parsetree.Pstr_open (copy_open_declaration x0)
  | Ast_503.Parsetree.Pstr_class x0 ->
      Ast_504.Parsetree.Pstr_class (List.map copy_class_declaration x0)
  | Ast_503.Parsetree.Pstr_class_type x0 ->
      Ast_504.Parsetree.Pstr_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_503.Parsetree.Pstr_include x0 ->
      Ast_504.Parsetree.Pstr_include (copy_include_declaration x0)
  | Ast_503.Parsetree.Pstr_attribute x0 ->
      Ast_504.Parsetree.Pstr_attribute (copy_attribute x0)
  | Ast_503.Parsetree.Pstr_extension (x0, x1) ->
      Ast_504.Parsetree.Pstr_extension (copy_extension x0, copy_attributes x1)

and copy_include_declaration :
    Ast_503.Parsetree.include_declaration ->
    Ast_504.Parsetree.include_declaration =
 fun x -> copy_include_infos copy_module_expr x

and copy_class_declaration :
    Ast_503.Parsetree.class_declaration -> Ast_504.Parsetree.class_declaration =
 fun x -> copy_class_infos copy_class_expr x

and copy_class_expr :
    Ast_503.Parsetree.class_expr -> Ast_504.Parsetree.class_expr =
 fun {
       Ast_503.Parsetree.pcl_desc;
       Ast_503.Parsetree.pcl_loc;
       Ast_503.Parsetree.pcl_attributes;
     } ->
  {
    Ast_504.Parsetree.pcl_desc = copy_class_expr_desc pcl_desc;
    Ast_504.Parsetree.pcl_loc = copy_location pcl_loc;
    Ast_504.Parsetree.pcl_attributes = copy_attributes pcl_attributes;
  }

and copy_class_expr_desc :
    Ast_503.Parsetree.class_expr_desc -> Ast_504.Parsetree.class_expr_desc =
  function
  | Ast_503.Parsetree.Pcl_constr (x0, x1) ->
      Ast_504.Parsetree.Pcl_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_503.Parsetree.Pcl_structure x0 ->
      Ast_504.Parsetree.Pcl_structure (copy_class_structure x0)
  | Ast_503.Parsetree.Pcl_fun (x0, x1, x2, x3) ->
      Ast_504.Parsetree.Pcl_fun
        ( copy_arg_label x0,
          Option.map copy_expression x1,
          copy_pattern x2,
          copy_class_expr x3 )
  | Ast_503.Parsetree.Pcl_apply (x0, x1) ->
      Ast_504.Parsetree.Pcl_apply
        ( copy_class_expr x0,
          List.map
            (fun x ->
              let x0, x1 = x in
              (copy_arg_label x0, copy_expression x1))
            x1 )
  | Ast_503.Parsetree.Pcl_let (x0, x1, x2) ->
      Ast_504.Parsetree.Pcl_let
        (copy_rec_flag x0, List.map copy_value_binding x1, copy_class_expr x2)
  | Ast_503.Parsetree.Pcl_constraint (x0, x1) ->
      Ast_504.Parsetree.Pcl_constraint (copy_class_expr x0, copy_class_type x1)
  | Ast_503.Parsetree.Pcl_extension x0 ->
      Ast_504.Parsetree.Pcl_extension (copy_extension x0)
  | Ast_503.Parsetree.Pcl_open (x0, x1) ->
      Ast_504.Parsetree.Pcl_open (copy_open_description x0, copy_class_expr x1)

and copy_class_structure :
    Ast_503.Parsetree.class_structure -> Ast_504.Parsetree.class_structure =
 fun { Ast_503.Parsetree.pcstr_self; Ast_503.Parsetree.pcstr_fields } ->
  {
    Ast_504.Parsetree.pcstr_self = copy_pattern pcstr_self;
    Ast_504.Parsetree.pcstr_fields = List.map copy_class_field pcstr_fields;
  }

and copy_class_field :
    Ast_503.Parsetree.class_field -> Ast_504.Parsetree.class_field =
 fun {
       Ast_503.Parsetree.pcf_desc;
       Ast_503.Parsetree.pcf_loc;
       Ast_503.Parsetree.pcf_attributes;
     } ->
  {
    Ast_504.Parsetree.pcf_desc = copy_class_field_desc pcf_desc;
    Ast_504.Parsetree.pcf_loc = copy_location pcf_loc;
    Ast_504.Parsetree.pcf_attributes = copy_attributes pcf_attributes;
  }

and copy_class_field_desc :
    Ast_503.Parsetree.class_field_desc -> Ast_504.Parsetree.class_field_desc =
  function
  | Ast_503.Parsetree.Pcf_inherit (x0, x1, x2) ->
      Ast_504.Parsetree.Pcf_inherit
        ( copy_override_flag x0,
          copy_class_expr x1,
          Option.map (fun x -> copy_loc (fun x -> x) x) x2 )
  | Ast_503.Parsetree.Pcf_val x0 ->
      Ast_504.Parsetree.Pcf_val
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_mutable_flag x1, copy_class_field_kind x2))
  | Ast_503.Parsetree.Pcf_method x0 ->
      Ast_504.Parsetree.Pcf_method
        (let x0, x1, x2 = x0 in
         (copy_loc copy_label x0, copy_private_flag x1, copy_class_field_kind x2))
  | Ast_503.Parsetree.Pcf_constraint x0 ->
      Ast_504.Parsetree.Pcf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_503.Parsetree.Pcf_initializer x0 ->
      Ast_504.Parsetree.Pcf_initializer (copy_expression x0)
  | Ast_503.Parsetree.Pcf_attribute x0 ->
      Ast_504.Parsetree.Pcf_attribute (copy_attribute x0)
  | Ast_503.Parsetree.Pcf_extension x0 ->
      Ast_504.Parsetree.Pcf_extension (copy_extension x0)

and copy_class_field_kind :
    Ast_503.Parsetree.class_field_kind -> Ast_504.Parsetree.class_field_kind =
  function
  | Ast_503.Parsetree.Cfk_virtual x0 ->
      Ast_504.Parsetree.Cfk_virtual (copy_core_type x0)
  | Ast_503.Parsetree.Cfk_concrete (x0, x1) ->
      Ast_504.Parsetree.Cfk_concrete (copy_override_flag x0, copy_expression x1)

and copy_open_declaration :
    Ast_503.Parsetree.open_declaration -> Ast_504.Parsetree.open_declaration =
 fun x -> copy_open_infos copy_module_expr x

and copy_module_binding :
    Ast_503.Parsetree.module_binding -> Ast_504.Parsetree.module_binding =
 fun {
       Ast_503.Parsetree.pmb_name;
       Ast_503.Parsetree.pmb_expr;
       Ast_503.Parsetree.pmb_attributes;
       Ast_503.Parsetree.pmb_loc;
     } ->
  {
    Ast_504.Parsetree.pmb_name =
      copy_loc (fun x -> Option.map (fun x -> x) x) pmb_name;
    Ast_504.Parsetree.pmb_expr = copy_module_expr pmb_expr;
    Ast_504.Parsetree.pmb_attributes = copy_attributes pmb_attributes;
    Ast_504.Parsetree.pmb_loc = copy_location pmb_loc;
  }

and copy_module_expr :
    Ast_503.Parsetree.module_expr -> Ast_504.Parsetree.module_expr =
 fun {
       Ast_503.Parsetree.pmod_desc;
       Ast_503.Parsetree.pmod_loc;
       Ast_503.Parsetree.pmod_attributes;
     } ->
  {
    Ast_504.Parsetree.pmod_desc = copy_module_expr_desc pmod_desc;
    Ast_504.Parsetree.pmod_loc = copy_location pmod_loc;
    Ast_504.Parsetree.pmod_attributes = copy_attributes pmod_attributes;
  }

and copy_module_expr_desc :
    Ast_503.Parsetree.module_expr_desc -> Ast_504.Parsetree.module_expr_desc =
  function
  | Ast_503.Parsetree.Pmod_ident x0 ->
      Ast_504.Parsetree.Pmod_ident (copy_loc copy_Longident_t x0)
  | Ast_503.Parsetree.Pmod_structure x0 ->
      Ast_504.Parsetree.Pmod_structure (copy_structure x0)
  | Ast_503.Parsetree.Pmod_functor (x0, x1) ->
      Ast_504.Parsetree.Pmod_functor
        (copy_functor_parameter x0, copy_module_expr x1)
  | Ast_503.Parsetree.Pmod_apply (x0, x1) ->
      Ast_504.Parsetree.Pmod_apply (copy_module_expr x0, copy_module_expr x1)
  | Ast_503.Parsetree.Pmod_apply_unit x0 ->
      Ast_504.Parsetree.Pmod_apply_unit (copy_module_expr x0)
  | Ast_503.Parsetree.Pmod_constraint (x0, x1) ->
      Ast_504.Parsetree.Pmod_constraint
        (copy_module_expr x0, copy_module_type x1)
  | Ast_503.Parsetree.Pmod_unpack x0 ->
      Ast_504.Parsetree.Pmod_unpack (copy_expression x0)
  | Ast_503.Parsetree.Pmod_extension x0 ->
      Ast_504.Parsetree.Pmod_extension (copy_extension x0)

and copy_functor_parameter :
    Ast_503.Parsetree.functor_parameter -> Ast_504.Parsetree.functor_parameter =
  function
  | Ast_503.Parsetree.Unit -> Ast_504.Parsetree.Unit
  | Ast_503.Parsetree.Named (x0, x1) ->
      Ast_504.Parsetree.Named
        (copy_loc (fun x -> Option.map (fun x -> x) x) x0, copy_module_type x1)

and copy_module_type :
    Ast_503.Parsetree.module_type -> Ast_504.Parsetree.module_type =
 fun {
       Ast_503.Parsetree.pmty_desc;
       Ast_503.Parsetree.pmty_loc;
       Ast_503.Parsetree.pmty_attributes;
     } ->
  {
    Ast_504.Parsetree.pmty_desc = copy_module_type_desc pmty_desc;
    Ast_504.Parsetree.pmty_loc = copy_location pmty_loc;
    Ast_504.Parsetree.pmty_attributes = copy_attributes pmty_attributes;
  }

and copy_module_type_desc :
    Ast_503.Parsetree.module_type_desc -> Ast_504.Parsetree.module_type_desc =
  function
  | Ast_503.Parsetree.Pmty_ident x0 ->
      Ast_504.Parsetree.Pmty_ident (copy_loc copy_Longident_t x0)
  | Ast_503.Parsetree.Pmty_signature x0 ->
      Ast_504.Parsetree.Pmty_signature (copy_signature x0)
  | Ast_503.Parsetree.Pmty_functor (x0, x1) ->
      Ast_504.Parsetree.Pmty_functor
        (copy_functor_parameter x0, copy_module_type x1)
  | Ast_503.Parsetree.Pmty_with (x0, x1) ->
      Ast_504.Parsetree.Pmty_with
        (copy_module_type x0, List.map copy_with_constraint x1)
  | Ast_503.Parsetree.Pmty_typeof x0 ->
      Ast_504.Parsetree.Pmty_typeof (copy_module_expr x0)
  | Ast_503.Parsetree.Pmty_extension x0 ->
      Ast_504.Parsetree.Pmty_extension (copy_extension x0)
  | Ast_503.Parsetree.Pmty_alias x0 ->
      Ast_504.Parsetree.Pmty_alias (copy_loc copy_Longident_t x0)

and copy_with_constraint :
    Ast_503.Parsetree.with_constraint -> Ast_504.Parsetree.with_constraint =
  function
  | Ast_503.Parsetree.Pwith_type (x0, x1) ->
      Ast_504.Parsetree.Pwith_type
        (copy_loc copy_Longident_t x0, copy_type_declaration x1)
  | Ast_503.Parsetree.Pwith_module (x0, x1) ->
      Ast_504.Parsetree.Pwith_module
        (copy_loc copy_Longident_t x0, copy_loc copy_Longident_t x1)
  | Ast_503.Parsetree.Pwith_modtype (x0, x1) ->
      Ast_504.Parsetree.Pwith_modtype
        (copy_loc copy_Longident_t x0, copy_module_type x1)
  | Ast_503.Parsetree.Pwith_modtypesubst (x0, x1) ->
      Ast_504.Parsetree.Pwith_modtypesubst
        (copy_loc copy_Longident_t x0, copy_module_type x1)
  | Ast_503.Parsetree.Pwith_typesubst (x0, x1) ->
      Ast_504.Parsetree.Pwith_typesubst
        (copy_loc copy_Longident_t x0, copy_type_declaration x1)
  | Ast_503.Parsetree.Pwith_modsubst (x0, x1) ->
      Ast_504.Parsetree.Pwith_modsubst
        (copy_loc copy_Longident_t x0, copy_loc copy_Longident_t x1)

and copy_signature : Ast_503.Parsetree.signature -> Ast_504.Parsetree.signature
    =
 fun x -> List.map copy_signature_item x

and copy_signature_item :
    Ast_503.Parsetree.signature_item -> Ast_504.Parsetree.signature_item =
 fun { Ast_503.Parsetree.psig_desc; Ast_503.Parsetree.psig_loc } ->
  {
    Ast_504.Parsetree.psig_desc = copy_signature_item_desc psig_desc;
    Ast_504.Parsetree.psig_loc = copy_location psig_loc;
  }

and copy_signature_item_desc :
    Ast_503.Parsetree.signature_item_desc ->
    Ast_504.Parsetree.signature_item_desc = function
  | Ast_503.Parsetree.Psig_value x0 ->
      Ast_504.Parsetree.Psig_value (copy_value_description x0)
  | Ast_503.Parsetree.Psig_type (x0, x1) ->
      Ast_504.Parsetree.Psig_type
        (copy_rec_flag x0, List.map copy_type_declaration x1)
  | Ast_503.Parsetree.Psig_typesubst x0 ->
      Ast_504.Parsetree.Psig_typesubst (List.map copy_type_declaration x0)
  | Ast_503.Parsetree.Psig_typext x0 ->
      Ast_504.Parsetree.Psig_typext (copy_type_extension x0)
  | Ast_503.Parsetree.Psig_exception x0 ->
      Ast_504.Parsetree.Psig_exception (copy_type_exception x0)
  | Ast_503.Parsetree.Psig_module x0 ->
      Ast_504.Parsetree.Psig_module (copy_module_declaration x0)
  | Ast_503.Parsetree.Psig_modsubst x0 ->
      Ast_504.Parsetree.Psig_modsubst (copy_module_substitution x0)
  | Ast_503.Parsetree.Psig_recmodule x0 ->
      Ast_504.Parsetree.Psig_recmodule (List.map copy_module_declaration x0)
  | Ast_503.Parsetree.Psig_modtype x0 ->
      Ast_504.Parsetree.Psig_modtype (copy_module_type_declaration x0)
  | Ast_503.Parsetree.Psig_modtypesubst x0 ->
      Ast_504.Parsetree.Psig_modtypesubst (copy_module_type_declaration x0)
  | Ast_503.Parsetree.Psig_open x0 ->
      Ast_504.Parsetree.Psig_open (copy_open_description x0)
  | Ast_503.Parsetree.Psig_include x0 ->
      Ast_504.Parsetree.Psig_include (copy_include_description x0)
  | Ast_503.Parsetree.Psig_class x0 ->
      Ast_504.Parsetree.Psig_class (List.map copy_class_description x0)
  | Ast_503.Parsetree.Psig_class_type x0 ->
      Ast_504.Parsetree.Psig_class_type
        (List.map copy_class_type_declaration x0)
  | Ast_503.Parsetree.Psig_attribute x0 ->
      Ast_504.Parsetree.Psig_attribute (copy_attribute x0)
  | Ast_503.Parsetree.Psig_extension (x0, x1) ->
      Ast_504.Parsetree.Psig_extension (copy_extension x0, copy_attributes x1)

and copy_class_type_declaration :
    Ast_503.Parsetree.class_type_declaration ->
    Ast_504.Parsetree.class_type_declaration =
 fun x -> copy_class_infos copy_class_type x

and copy_class_description :
    Ast_503.Parsetree.class_description -> Ast_504.Parsetree.class_description =
 fun x -> copy_class_infos copy_class_type x

and copy_class_type :
    Ast_503.Parsetree.class_type -> Ast_504.Parsetree.class_type =
 fun {
       Ast_503.Parsetree.pcty_desc;
       Ast_503.Parsetree.pcty_loc;
       Ast_503.Parsetree.pcty_attributes;
     } ->
  {
    Ast_504.Parsetree.pcty_desc = copy_class_type_desc pcty_desc;
    Ast_504.Parsetree.pcty_loc = copy_location pcty_loc;
    Ast_504.Parsetree.pcty_attributes = copy_attributes pcty_attributes;
  }

and copy_class_type_desc :
    Ast_503.Parsetree.class_type_desc -> Ast_504.Parsetree.class_type_desc =
  function
  | Ast_503.Parsetree.Pcty_constr (x0, x1) ->
      Ast_504.Parsetree.Pcty_constr
        (copy_loc copy_Longident_t x0, List.map copy_core_type x1)
  | Ast_503.Parsetree.Pcty_signature x0 ->
      Ast_504.Parsetree.Pcty_signature (copy_class_signature x0)
  | Ast_503.Parsetree.Pcty_arrow (x0, x1, x2) ->
      Ast_504.Parsetree.Pcty_arrow
        (copy_arg_label x0, copy_core_type x1, copy_class_type x2)
  | Ast_503.Parsetree.Pcty_extension x0 ->
      Ast_504.Parsetree.Pcty_extension (copy_extension x0)
  | Ast_503.Parsetree.Pcty_open (x0, x1) ->
      Ast_504.Parsetree.Pcty_open (copy_open_description x0, copy_class_type x1)

and copy_class_signature :
    Ast_503.Parsetree.class_signature -> Ast_504.Parsetree.class_signature =
 fun { Ast_503.Parsetree.pcsig_self; Ast_503.Parsetree.pcsig_fields } ->
  {
    Ast_504.Parsetree.pcsig_self = copy_core_type pcsig_self;
    Ast_504.Parsetree.pcsig_fields = List.map copy_class_type_field pcsig_fields;
  }

and copy_class_type_field :
    Ast_503.Parsetree.class_type_field -> Ast_504.Parsetree.class_type_field =
 fun {
       Ast_503.Parsetree.pctf_desc;
       Ast_503.Parsetree.pctf_loc;
       Ast_503.Parsetree.pctf_attributes;
     } ->
  {
    Ast_504.Parsetree.pctf_desc = copy_class_type_field_desc pctf_desc;
    Ast_504.Parsetree.pctf_loc = copy_location pctf_loc;
    Ast_504.Parsetree.pctf_attributes = copy_attributes pctf_attributes;
  }

and copy_class_type_field_desc :
    Ast_503.Parsetree.class_type_field_desc ->
    Ast_504.Parsetree.class_type_field_desc = function
  | Ast_503.Parsetree.Pctf_inherit x0 ->
      Ast_504.Parsetree.Pctf_inherit (copy_class_type x0)
  | Ast_503.Parsetree.Pctf_val x0 ->
      Ast_504.Parsetree.Pctf_val
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_mutable_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_503.Parsetree.Pctf_method x0 ->
      Ast_504.Parsetree.Pctf_method
        (let x0, x1, x2, x3 = x0 in
         ( copy_loc copy_label x0,
           copy_private_flag x1,
           copy_virtual_flag x2,
           copy_core_type x3 ))
  | Ast_503.Parsetree.Pctf_constraint x0 ->
      Ast_504.Parsetree.Pctf_constraint
        (let x0, x1 = x0 in
         (copy_core_type x0, copy_core_type x1))
  | Ast_503.Parsetree.Pctf_attribute x0 ->
      Ast_504.Parsetree.Pctf_attribute (copy_attribute x0)
  | Ast_503.Parsetree.Pctf_extension x0 ->
      Ast_504.Parsetree.Pctf_extension (copy_extension x0)

and copy_extension : Ast_503.Parsetree.extension -> Ast_504.Parsetree.extension
    =
 fun x ->
  let x0, x1 = x in
  (copy_loc (fun x -> x) x0, copy_payload x1)

and copy_class_infos :
    'f0 'g0.
    ('f0 -> 'g0) ->
    'f0 Ast_503.Parsetree.class_infos ->
    'g0 Ast_504.Parsetree.class_infos =
 fun f0
     {
       Ast_503.Parsetree.pci_virt;
       Ast_503.Parsetree.pci_params;
       Ast_503.Parsetree.pci_name;
       Ast_503.Parsetree.pci_expr;
       Ast_503.Parsetree.pci_loc;
       Ast_503.Parsetree.pci_attributes;
     } ->
  {
    Ast_504.Parsetree.pci_virt = copy_virtual_flag pci_virt;
    Ast_504.Parsetree.pci_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        pci_params;
    Ast_504.Parsetree.pci_name = copy_loc (fun x -> x) pci_name;
    Ast_504.Parsetree.pci_expr = f0 pci_expr;
    Ast_504.Parsetree.pci_loc = copy_location pci_loc;
    Ast_504.Parsetree.pci_attributes = copy_attributes pci_attributes;
  }

and copy_virtual_flag :
    Ast_503.Asttypes.virtual_flag -> Ast_504.Asttypes.virtual_flag = function
  | Ast_503.Asttypes.Virtual -> Ast_504.Asttypes.Virtual
  | Ast_503.Asttypes.Concrete -> Ast_504.Asttypes.Concrete

and copy_include_description :
    Ast_503.Parsetree.include_description ->
    Ast_504.Parsetree.include_description =
 fun x -> copy_include_infos copy_module_type x

and copy_include_infos :
    'f0 'g0.
    ('f0 -> 'g0) ->
    'f0 Ast_503.Parsetree.include_infos ->
    'g0 Ast_504.Parsetree.include_infos =
 fun f0
     {
       Ast_503.Parsetree.pincl_mod;
       Ast_503.Parsetree.pincl_loc;
       Ast_503.Parsetree.pincl_attributes;
     } ->
  {
    Ast_504.Parsetree.pincl_mod = f0 pincl_mod;
    Ast_504.Parsetree.pincl_loc = copy_location pincl_loc;
    Ast_504.Parsetree.pincl_attributes = copy_attributes pincl_attributes;
  }

and copy_open_description :
    Ast_503.Parsetree.open_description -> Ast_504.Parsetree.open_description =
 fun x -> copy_open_infos (fun x -> copy_loc copy_Longident_t x) x

and copy_open_infos :
    'f0 'g0.
    ('f0 -> 'g0) ->
    'f0 Ast_503.Parsetree.open_infos ->
    'g0 Ast_504.Parsetree.open_infos =
 fun f0
     {
       Ast_503.Parsetree.popen_expr;
       Ast_503.Parsetree.popen_override;
       Ast_503.Parsetree.popen_loc;
       Ast_503.Parsetree.popen_attributes;
     } ->
  {
    Ast_504.Parsetree.popen_expr = f0 popen_expr;
    Ast_504.Parsetree.popen_override = copy_override_flag popen_override;
    Ast_504.Parsetree.popen_loc = copy_location popen_loc;
    Ast_504.Parsetree.popen_attributes = copy_attributes popen_attributes;
  }

and copy_override_flag :
    Ast_503.Asttypes.override_flag -> Ast_504.Asttypes.override_flag = function
  | Ast_503.Asttypes.Override -> Ast_504.Asttypes.Override
  | Ast_503.Asttypes.Fresh -> Ast_504.Asttypes.Fresh

and copy_module_type_declaration :
    Ast_503.Parsetree.module_type_declaration ->
    Ast_504.Parsetree.module_type_declaration =
 fun {
       Ast_503.Parsetree.pmtd_name;
       Ast_503.Parsetree.pmtd_type;
       Ast_503.Parsetree.pmtd_attributes;
       Ast_503.Parsetree.pmtd_loc;
     } ->
  {
    Ast_504.Parsetree.pmtd_name = copy_loc (fun x -> x) pmtd_name;
    Ast_504.Parsetree.pmtd_type = Option.map copy_module_type pmtd_type;
    Ast_504.Parsetree.pmtd_attributes = copy_attributes pmtd_attributes;
    Ast_504.Parsetree.pmtd_loc = copy_location pmtd_loc;
  }

and copy_module_substitution :
    Ast_503.Parsetree.module_substitution ->
    Ast_504.Parsetree.module_substitution =
 fun {
       Ast_503.Parsetree.pms_name;
       Ast_503.Parsetree.pms_manifest;
       Ast_503.Parsetree.pms_attributes;
       Ast_503.Parsetree.pms_loc;
     } ->
  {
    Ast_504.Parsetree.pms_name = copy_loc (fun x -> x) pms_name;
    Ast_504.Parsetree.pms_manifest = copy_loc copy_Longident_t pms_manifest;
    Ast_504.Parsetree.pms_attributes = copy_attributes pms_attributes;
    Ast_504.Parsetree.pms_loc = copy_location pms_loc;
  }

and copy_module_declaration :
    Ast_503.Parsetree.module_declaration -> Ast_504.Parsetree.module_declaration
    =
 fun {
       Ast_503.Parsetree.pmd_name;
       Ast_503.Parsetree.pmd_type;
       Ast_503.Parsetree.pmd_attributes;
       Ast_503.Parsetree.pmd_loc;
     } ->
  {
    Ast_504.Parsetree.pmd_name =
      copy_loc (fun x -> Option.map (fun x -> x) x) pmd_name;
    Ast_504.Parsetree.pmd_type = copy_module_type pmd_type;
    Ast_504.Parsetree.pmd_attributes = copy_attributes pmd_attributes;
    Ast_504.Parsetree.pmd_loc = copy_location pmd_loc;
  }

and copy_type_exception :
    Ast_503.Parsetree.type_exception -> Ast_504.Parsetree.type_exception =
 fun {
       Ast_503.Parsetree.ptyexn_constructor;
       Ast_503.Parsetree.ptyexn_loc;
       Ast_503.Parsetree.ptyexn_attributes;
     } ->
  {
    Ast_504.Parsetree.ptyexn_constructor =
      copy_extension_constructor ptyexn_constructor;
    Ast_504.Parsetree.ptyexn_loc = copy_location ptyexn_loc;
    Ast_504.Parsetree.ptyexn_attributes = copy_attributes ptyexn_attributes;
  }

and copy_type_extension :
    Ast_503.Parsetree.type_extension -> Ast_504.Parsetree.type_extension =
 fun {
       Ast_503.Parsetree.ptyext_path;
       Ast_503.Parsetree.ptyext_params;
       Ast_503.Parsetree.ptyext_constructors;
       Ast_503.Parsetree.ptyext_private;
       Ast_503.Parsetree.ptyext_loc;
       Ast_503.Parsetree.ptyext_attributes;
     } ->
  {
    Ast_504.Parsetree.ptyext_path = copy_loc copy_Longident_t ptyext_path;
    Ast_504.Parsetree.ptyext_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        ptyext_params;
    Ast_504.Parsetree.ptyext_constructors =
      List.map copy_extension_constructor ptyext_constructors;
    Ast_504.Parsetree.ptyext_private = copy_private_flag ptyext_private;
    Ast_504.Parsetree.ptyext_loc = copy_location ptyext_loc;
    Ast_504.Parsetree.ptyext_attributes = copy_attributes ptyext_attributes;
  }

and copy_extension_constructor :
    Ast_503.Parsetree.extension_constructor ->
    Ast_504.Parsetree.extension_constructor =
 fun {
       Ast_503.Parsetree.pext_name;
       Ast_503.Parsetree.pext_kind;
       Ast_503.Parsetree.pext_loc;
       Ast_503.Parsetree.pext_attributes;
     } ->
  {
    Ast_504.Parsetree.pext_name = copy_loc (fun x -> x) pext_name;
    Ast_504.Parsetree.pext_kind = copy_extension_constructor_kind pext_kind;
    Ast_504.Parsetree.pext_loc = copy_location pext_loc;
    Ast_504.Parsetree.pext_attributes = copy_attributes pext_attributes;
  }

and copy_extension_constructor_kind :
    Ast_503.Parsetree.extension_constructor_kind ->
    Ast_504.Parsetree.extension_constructor_kind = function
  | Ast_503.Parsetree.Pext_decl (x0, x1, x2) ->
      Ast_504.Parsetree.Pext_decl
        ( List.map (fun x -> copy_loc (fun x -> x) x) x0,
          copy_constructor_arguments x1,
          Option.map copy_core_type x2 )
  | Ast_503.Parsetree.Pext_rebind x0 ->
      Ast_504.Parsetree.Pext_rebind (copy_loc copy_Longident_t x0)

and copy_type_declaration :
    Ast_503.Parsetree.type_declaration -> Ast_504.Parsetree.type_declaration =
 fun {
       Ast_503.Parsetree.ptype_name;
       Ast_503.Parsetree.ptype_params;
       Ast_503.Parsetree.ptype_cstrs;
       Ast_503.Parsetree.ptype_kind;
       Ast_503.Parsetree.ptype_private;
       Ast_503.Parsetree.ptype_manifest;
       Ast_503.Parsetree.ptype_attributes;
       Ast_503.Parsetree.ptype_loc;
     } ->
  {
    Ast_504.Parsetree.ptype_name = copy_loc (fun x -> x) ptype_name;
    Ast_504.Parsetree.ptype_params =
      List.map
        (fun x ->
          let x0, x1 = x in
          ( copy_core_type x0,
            let x0, x1 = x1 in
            (copy_variance x0, copy_injectivity x1) ))
        ptype_params;
    Ast_504.Parsetree.ptype_cstrs =
      List.map
        (fun x ->
          let x0, x1, x2 = x in
          (copy_core_type x0, copy_core_type x1, copy_location x2))
        ptype_cstrs;
    Ast_504.Parsetree.ptype_kind = copy_type_kind ptype_kind;
    Ast_504.Parsetree.ptype_private = copy_private_flag ptype_private;
    Ast_504.Parsetree.ptype_manifest = Option.map copy_core_type ptype_manifest;
    Ast_504.Parsetree.ptype_attributes = copy_attributes ptype_attributes;
    Ast_504.Parsetree.ptype_loc = copy_location ptype_loc;
  }

and copy_private_flag :
    Ast_503.Asttypes.private_flag -> Ast_504.Asttypes.private_flag = function
  | Ast_503.Asttypes.Private -> Ast_504.Asttypes.Private
  | Ast_503.Asttypes.Public -> Ast_504.Asttypes.Public

and copy_type_kind : Ast_503.Parsetree.type_kind -> Ast_504.Parsetree.type_kind
    = function
  | Ast_503.Parsetree.Ptype_abstract -> Ast_504.Parsetree.Ptype_abstract
  | Ast_503.Parsetree.Ptype_variant x0 ->
      Ast_504.Parsetree.Ptype_variant (List.map copy_constructor_declaration x0)
  | Ast_503.Parsetree.Ptype_record x0 ->
      Ast_504.Parsetree.Ptype_record (List.map copy_label_declaration x0)
  | Ast_503.Parsetree.Ptype_open -> Ast_504.Parsetree.Ptype_open

and copy_constructor_declaration :
    Ast_503.Parsetree.constructor_declaration ->
    Ast_504.Parsetree.constructor_declaration =
 fun {
       Ast_503.Parsetree.pcd_name;
       Ast_503.Parsetree.pcd_vars;
       Ast_503.Parsetree.pcd_args;
       Ast_503.Parsetree.pcd_res;
       Ast_503.Parsetree.pcd_loc;
       Ast_503.Parsetree.pcd_attributes;
     } ->
  {
    Ast_504.Parsetree.pcd_name = copy_loc (fun x -> x) pcd_name;
    Ast_504.Parsetree.pcd_vars =
      List.map (fun x -> copy_loc (fun x -> x) x) pcd_vars;
    Ast_504.Parsetree.pcd_args = copy_constructor_arguments pcd_args;
    Ast_504.Parsetree.pcd_res = Option.map copy_core_type pcd_res;
    Ast_504.Parsetree.pcd_loc = copy_location pcd_loc;
    Ast_504.Parsetree.pcd_attributes = copy_attributes pcd_attributes;
  }

and copy_constructor_arguments :
    Ast_503.Parsetree.constructor_arguments ->
    Ast_504.Parsetree.constructor_arguments = function
  | Ast_503.Parsetree.Pcstr_tuple x0 ->
      Ast_504.Parsetree.Pcstr_tuple (List.map copy_core_type x0)
  | Ast_503.Parsetree.Pcstr_record x0 ->
      Ast_504.Parsetree.Pcstr_record (List.map copy_label_declaration x0)

and copy_label_declaration :
    Ast_503.Parsetree.label_declaration -> Ast_504.Parsetree.label_declaration =
 fun {
       Ast_503.Parsetree.pld_name;
       Ast_503.Parsetree.pld_mutable;
       Ast_503.Parsetree.pld_type;
       Ast_503.Parsetree.pld_loc;
       Ast_503.Parsetree.pld_attributes;
     } ->
  {
    Ast_504.Parsetree.pld_name = copy_loc (fun x -> x) pld_name;
    Ast_504.Parsetree.pld_mutable = copy_mutable_flag pld_mutable;
    Ast_504.Parsetree.pld_type = copy_core_type pld_type;
    Ast_504.Parsetree.pld_loc = copy_location pld_loc;
    Ast_504.Parsetree.pld_attributes = copy_attributes pld_attributes;
  }

and copy_mutable_flag :
    Ast_503.Asttypes.mutable_flag -> Ast_504.Asttypes.mutable_flag = function
  | Ast_503.Asttypes.Immutable -> Ast_504.Asttypes.Immutable
  | Ast_503.Asttypes.Mutable -> Ast_504.Asttypes.Mutable

and copy_injectivity :
    Ast_503.Asttypes.injectivity -> Ast_504.Asttypes.injectivity = function
  | Ast_503.Asttypes.Injective -> Ast_504.Asttypes.Injective
  | Ast_503.Asttypes.NoInjectivity -> Ast_504.Asttypes.NoInjectivity

and copy_variance : Ast_503.Asttypes.variance -> Ast_504.Asttypes.variance =
  function
  | Ast_503.Asttypes.Covariant -> Ast_504.Asttypes.Covariant
  | Ast_503.Asttypes.Contravariant -> Ast_504.Asttypes.Contravariant
  | Ast_503.Asttypes.NoVariance -> Ast_504.Asttypes.NoVariance

and copy_value_description :
    Ast_503.Parsetree.value_description -> Ast_504.Parsetree.value_description =
 fun {
       Ast_503.Parsetree.pval_name;
       Ast_503.Parsetree.pval_type;
       Ast_503.Parsetree.pval_prim;
       Ast_503.Parsetree.pval_attributes;
       Ast_503.Parsetree.pval_loc;
     } ->
  {
    Ast_504.Parsetree.pval_name = copy_loc (fun x -> x) pval_name;
    Ast_504.Parsetree.pval_type = copy_core_type pval_type;
    Ast_504.Parsetree.pval_prim = List.map (fun x -> x) pval_prim;
    Ast_504.Parsetree.pval_attributes = copy_attributes pval_attributes;
    Ast_504.Parsetree.pval_loc = copy_location pval_loc;
  }

and copy_object_field_desc :
    Ast_503.Parsetree.object_field_desc -> Ast_504.Parsetree.object_field_desc =
  function
  | Ast_503.Parsetree.Otag (x0, x1) ->
      Ast_504.Parsetree.Otag (copy_loc copy_label x0, copy_core_type x1)
  | Ast_503.Parsetree.Oinherit x0 ->
      Ast_504.Parsetree.Oinherit (copy_core_type x0)

and copy_arg_label : Ast_503.Asttypes.arg_label -> Ast_504.Asttypes.arg_label =
  function
  | Ast_503.Asttypes.Nolabel -> Ast_504.Asttypes.Nolabel
  | Ast_503.Asttypes.Labelled x0 -> Ast_504.Asttypes.Labelled x0
  | Ast_503.Asttypes.Optional x0 -> Ast_504.Asttypes.Optional x0

and copy_closed_flag :
    Ast_503.Asttypes.closed_flag -> Ast_504.Asttypes.closed_flag = function
  | Ast_503.Asttypes.Closed -> Ast_504.Asttypes.Closed
  | Ast_503.Asttypes.Open -> Ast_504.Asttypes.Open

and copy_label : Ast_503.Asttypes.label -> Ast_504.Asttypes.label = fun x -> x

and copy_rec_flag : Ast_503.Asttypes.rec_flag -> Ast_504.Asttypes.rec_flag =
  function
  | Ast_503.Asttypes.Nonrecursive -> Ast_504.Asttypes.Nonrecursive
  | Ast_503.Asttypes.Recursive -> Ast_504.Asttypes.Recursive

and copy_constant : Ast_503.Parsetree.constant -> Ast_504.Parsetree.constant =
 fun c ->
  let pconst_desc =
    match c.pconst_desc with
    | Ast_503.Parsetree.Pconst_integer (x0, x1) ->
        Ast_504.Parsetree.Pconst_integer (x0, Option.map (fun x -> x) x1)
    | Ast_503.Parsetree.Pconst_char x0 -> Ast_504.Parsetree.Pconst_char x0
    | Ast_503.Parsetree.Pconst_string (x0, x1, x2) ->
        Ast_504.Parsetree.Pconst_string
          (x0, copy_location x1, Option.map (fun x -> x) x2)
    | Ast_503.Parsetree.Pconst_float (x0, x1) ->
        Ast_504.Parsetree.Pconst_float (x0, Option.map (fun x -> x) x1)
  in
  { pconst_desc; pconst_loc = copy_location c.pconst_loc }

and copy_Longident_t : Longident.t -> Ast_504.Longident.t = function
  | Longident.Lident x0 -> Ast_504.Longident.Lident x0
  | Longident.Ldot (x0, x1) ->
      Ast_504.Longident.Ldot
        ( { txt = copy_Longident_t x0; loc = Location.none },
          { txt = x1; loc = Location.none } )
  | Longident.Lapply (x0, x1) ->
      let x0 = Location.{ txt = copy_Longident_t x0; loc = Location.none } in
      let x1 = Location.{ txt = copy_Longident_t x1; loc = Location.none } in
      Ast_504.Longident.Lapply (x0, x1)

and copy_loc :
    'f0 'g0.
    ('f0 -> 'g0) -> 'f0 Ast_503.Asttypes.loc -> 'g0 Ast_504.Asttypes.loc =
 fun f0 { Ast_503.Asttypes.txt; Ast_503.Asttypes.loc } ->
  { Ast_504.Asttypes.txt = f0 txt; Ast_504.Asttypes.loc = copy_location loc }

and copy_location : Location.t -> Location.t = fun x -> x
