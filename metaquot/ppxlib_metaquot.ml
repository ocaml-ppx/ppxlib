open Ppxlib
open Ast_builder.Default
module E = Extension
module A = Ast_pattern

type quoted_attributes =
  { quoted_attributes : attributes
  (* The attributes that appear quoted, e.g. [@foo] in [%expr [%e e] [@foo]] *)
  ; field_name : string
  (* The field name where attributes are stored for the kind of ASt the quoted attributes
     are placed on, e.g. pexp_attributes. *)
  }

module Make (M : sig
  type result

  val annotate : result -> core_type -> result

  val cast
    :  < attributes : attributes -> result; .. >
      (* The instance of the [std_lifters] class being used. *)
    -> extension
    -> quoted_attributes option
    -> result

  val location : location -> result
  val location_stack : (location -> result) option
  val attributes : (location -> result) option

  class std_lifters : location -> [result] Ppxlib_traverse_builtins.std_lifters
end) =
struct
  let lift loc =
    object (self)
      inherit [M.result] Ast_traverse.lift as super
      inherit! M.std_lifters loc

      method typed ast name =
        let loc = { loc with loc_ghost = true } in
        M.annotate ast
          (ptyp_constr ~loc
             { loc; txt = Ldot (Ldot (Lident "Ppxlib_ast", "Ast"), name) }
             [])

      method! attribute x =
        Attribute.mark_as_handled_manually x;
        super#attribute x

      method! location _ = M.location loc

      method! attributes x =
        match M.attributes with
        | None -> super#attributes x
        | Some f ->
            assert_no_attributes x;
            f loc

      method! location_stack x =
        match M.location_stack with
        | None -> super#location_stack x
        | Some f -> f loc

      method! expression e =
        match e.pexp_desc with
        | Pexp_extension (({ txt = "e"; _ }, _) as ext) ->
            let attributes =
              { quoted_attributes = e.pexp_attributes
              ; field_name = "pexp_attributes"
              }
            in
            self#typed (M.cast self ext (Some attributes)) "expression"
        | _ -> super#expression e

      method! pattern p =
        match p.ppat_desc with
        | Ppat_extension (({ txt = "p"; _ }, _) as ext) ->
            let attributes =
              { quoted_attributes = p.ppat_attributes
              ; field_name = "ppat_attributes"
              }
            in
            self#typed (M.cast self ext (Some attributes)) "pattern"
        | _ -> super#pattern p

      method! core_type t =
        match t.ptyp_desc with
        | Ptyp_extension (({ txt = "t"; _ }, _) as ext) ->
            let attributes =
              { quoted_attributes = t.ptyp_attributes
              ; field_name = "ptyp_attributes"
              }
            in
            self#typed (M.cast self ext (Some attributes)) "core_type"
        | _ -> super#core_type t

      method! module_expr m =
        match m.pmod_desc with
        | Pmod_extension (({ txt = "m"; _ }, _) as ext) ->
            let attributes =
              { quoted_attributes = m.pmod_attributes
              ; field_name = "pmod_attributes"
              }
            in
            self#typed (M.cast self ext (Some attributes)) "module_expr"
        | _ -> super#module_expr m

      method! module_type m =
        match m.pmty_desc with
        | Pmty_extension (({ txt = "m"; _ }, _) as ext) ->
            let attributes =
              { quoted_attributes = m.pmty_attributes
              ; field_name = "pmty_attributes"
              }
            in
            self#typed (M.cast self ext (Some attributes)) "module_type"
        | _ -> super#module_type m

      method! structure_item i =
        match i.pstr_desc with
        | Pstr_extension ((({ txt = "i"; _ }, _) as ext), attrs) ->
            assert_no_attributes attrs;
            self#typed (M.cast self ext None) "structure_item"
        | _ -> super#structure_item i

      method! signature_item i =
        match i.psig_desc with
        | Psig_extension ((({ txt = "i"; _ }, _) as ext), attrs) ->
            assert_no_attributes attrs;
            self#typed (M.cast self ext None) "signature_item"
        | _ -> super#signature_item i
    end
end


module Expr = Make (struct
  type result = expression

  let location loc = evar ~loc:{ loc with loc_ghost = true } "loc"
  let location_stack = None
  let attributes = None

  class std_lifters = Ppxlib_metaquot_lifters.expression_lifters

  let annotate e core_type = pexp_constraint ~loc:core_type.ptyp_loc e core_type

  let fresh_name =
    let counter = ref 0 in
    fun () ->
      let var = "_ppx_metaquot_helper_var%d" ^ Int.to_string !counter in
      incr counter;
      var

  (* Append the quoted attributes to the attributes present on the
     antiquoted construct. Take this as example:

     [%expr [%e e] [@attr]]

     Suppose e has pexp_attributes = [attr1]. Then the resulting attributes
     are [ attr1; [@attr] ]. The decision to put outer attributes (here,
     [@attr]) at the end of the list is consistent with other parts of ppxlib
     that accumulate attributes.
  *)
  let add_quoted_attributes self e { quoted_attributes; field_name } ~loc =
    match quoted_attributes with
    | [] -> e
    | _ :: _ ->
      let open Ppxlib_ast.Ast_helper in
      let loc = { loc with loc_ghost = true } in
      let mkloc x = Located.mk x ~loc in
      let var = fresh_name () in
      let var_expr = Exp.ident (mkloc (Lident var)) in
      let field_name = mkloc (Lident field_name) in
      let reified_attrs = self#attributes quoted_attributes in
      (* append arg1 arg2 = [%expr Stdlib.List.append [%e arg1] [%e arg2]] *)
      let append arg1 arg2 =
        Exp.apply
          (Exp.ident
             (mkloc (Ldot (Ldot (Lident "Stdlib", "List"), "append"))))
          [ Nolabel, arg1; Nolabel, arg2 ]
      in
      (* [%expr
         let [%p var] = [%e e] in
         { [%e var] with field_name =
         [%e append [%expr [%e var].field_name] reified_attrs]
         ]

         This comment lies a little bit: field_name is actually some other
         literal string.
      *)
      Exp.let_
        Nonrecursive
        [ Vb.mk (Pat.var (mkloc var)) e ]
        (Exp.record
           [ field_name, append (Exp.field var_expr field_name) reified_attrs ]
           (Some var_expr))

  let cast self ext attrs =
    match snd ext with
    | PStr [ { pstr_desc = Pstr_eval (e, inner_attrs); _ } ] ->
      assert_no_attributes inner_attrs;
      (match attrs with
       | None -> e
       | Some quoted_attrs ->
         add_quoted_attributes self e quoted_attrs
           ~loc:(loc_of_extension ext))
    | _ ->
        Ast_builder.Default.(
          pexp_extension ~loc:(loc_of_extension ext)
            (Location.error_extensionf ~loc:(loc_of_extension ext)
               "expression expected"))
end)

module Patt = Make (struct
  type result = pattern

  let location loc = ppat_any ~loc:{ loc with loc_ghost = true }

  let location_stack =
    Some (fun loc -> ppat_any ~loc:{ loc with loc_ghost = true })

  let attributes = Some (fun loc -> ppat_any ~loc:{ loc with loc_ghost = true })

  class std_lifters = Ppxlib_metaquot_lifters.pattern_lifters

  let annotate p core_type = ppat_constraint ~loc:core_type.ptyp_loc p core_type

  let cast _ ext attrs =
    begin
      match attrs with
      | None -> ()
      | Some { quoted_attributes; field_name = _ } ->
          (* In theory, we could create a pattern where [quoted_attributes]
             is consed to the front of [p.ppat_attributes]. But this is
             inconsistent with [Expression.add_quoted_attributes], which appends
             quoted attributes to the end -- and this wouldn't be a legal
             pattern.
          *)
          assert_no_attributes quoted_attributes
    end;
    match snd ext with
    | PPat (p, None) -> p
    | PPat (_, Some e) ->
        Ast_builder.Default.(
          ppat_extension ~loc:e.pexp_loc
            (Location.error_extensionf ~loc:e.pexp_loc "guard not expected here"))
    | _ ->
        Ast_builder.Default.(
          ppat_extension ~loc:(loc_of_extension ext)
            (Location.error_extensionf ~loc:(loc_of_extension ext)
               "pattern expected"))
end)

let () =
  let extensions ctx lifter =
    [
      E.declare "metaquot.expr" ctx
        A.(single_expr_payload __)
        (fun ~loc ~path:_ e ->
          let lift = lifter loc in
          lift#typed (lift#expression e) "expression");
      E.declare "metaquot.pat" ctx
        A.(ppat __ none)
        (fun ~loc ~path:_ p ->
          let lift = lifter loc in
          lift#typed (lift#pattern p) "pattern");
      E.declare "metaquot.str" ctx
        A.(pstr __)
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#structure s) "structure");
      E.declare "metaquot.stri" ctx
        A.(pstr (__ ^:: nil))
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#structure_item s) "structure_item");
      E.declare "metaquot.sig" ctx
        A.(psig __)
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#signature s) "signature");
      E.declare "metaquot.sigi" ctx
        A.(psig (__ ^:: nil))
        (fun ~loc ~path:_ s ->
          let lift = lifter loc in
          lift#typed (lift#signature_item s) "signature_item");
      E.declare "metaquot.type" ctx
        A.(ptyp __)
        (fun ~loc ~path:_ t ->
          let lift = lifter loc in
          lift#typed (lift#core_type t) "core_type");
    ]
  in
  let extensions =
    extensions Expression Expr.lift @ extensions Pattern Patt.lift
  in
  Driver.register_transformation "metaquot" ~extensions
