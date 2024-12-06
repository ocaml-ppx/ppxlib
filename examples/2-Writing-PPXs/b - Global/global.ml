open Ppxlib
open Ast_builder.Default

let enum_tag = "enum"

(* This function is well explained in the Context Free Section *)
let enum ~loc ?(opt = false) ast () =
  match ast with
  | _, [ { ptype_kind = Ptype_variant variants; _ } ] ->
      let expr_string = Ast_builder.Default.estring ~loc in
      let to_string_expr =
        [%stri
          let[@warning "-32"] to_string value =
            [%e
              pexp_match ~loc [%expr value]
                (List.map
                   (fun { pcd_name = { txt = value; _ }; _ } ->
                     case
                       ~lhs:
                         (ppat_construct ~loc (Located.lident ~loc value) None)
                       ~guard:None ~rhs:(expr_string value))
                   variants)]]
      in
      let else_case =
        case
          ~lhs:[%pat? [%p ppat_any ~loc]]
          ~guard:None
          ~rhs:
            (match opt with
            | true -> [%expr None]
            | _ ->
                [%expr
                  raise (Invalid_argument "Argument doesn't match variants")])
      in
      let from_string_expr =
        [%stri
          let[@warning "-32"] from_string value =
            [%e
              pexp_match ~loc [%expr value]
                (List.map
                   (fun { pcd_name = { txt = value; _ }; _ } ->
                     case
                       ~lhs:
                         (ppat_constant ~loc (Pconst_string (value, loc, None)))
                       ~guard:None
                       ~rhs:
                         (match opt with
                         | true ->
                             [%expr
                               Some
                                 [%e
                                   pexp_construct ~loc
                                     (Located.lident ~loc value)
                                     None]]
                         | _ ->
                             pexp_construct ~loc
                               (Located.lident ~loc value)
                               None))
                   variants
                @ [ else_case ])]]
      in
      [ from_string_expr; to_string_expr ]
  | _ ->
      [%str [%ocaml.error "Ops, enum must be a type with variant without args"]]

module Lint = struct
  let traverse =
    object
      inherit [Driver.Lint_error.t list] Ast_traverse.fold

      method! value_binding mb acc =
        let loc = mb.pvb_loc in
        match mb.pvb_pat.ppat_desc with
        | Ppat_var { txt = name; _ } ->
            if String.starts_with name ~prefix:"demo_" then acc
            else
              Driver.Lint_error.of_string loc
                "Ops, variable name must not start with demo_"
              :: acc
        | _ -> acc
    end
end

let _ =
  Driver.register_transformation "enum2" ~lint_impl:(fun st ->
      Lint.traverse#structure st [])

module PreProcess = struct
  let traverse =
    object (_ : Ast_traverse.map)
      inherit Ast_traverse.map as super

      method! module_expr mod_exp =
        let mod_exp = super#module_expr mod_exp in
        match mod_exp.pmod_attributes with
        | [ { attr_name = { txt = "enum"; _ }; _ } ] -> (
            match mod_exp.pmod_desc with
            | Pmod_structure
                ([ { pstr_desc = Pstr_type (name, variants); _ } ] as str) ->
                let type_ = enum ~loc:mod_exp.pmod_loc (name, variants) () in
                Ast_builder.Default.pmod_structure ~loc:mod_exp.pmod_loc
                  (str @ type_)
            | _ -> mod_exp)
        | _ -> mod_exp
    end
end

let _ =
  Driver.register_transformation "enum" ~impl:PreProcess.traverse#structure

module Global = struct
  let traverse =
    object (_ : Ast_traverse.map)
      inherit Ast_traverse.map as super

      method! module_expr mod_exp =
        let mod_exp = super#module_expr mod_exp in
        match mod_exp.pmod_attributes with
        | [ { attr_name = { txt = "enum2"; _ }; attr_payload = payload; _ } ]
          -> (
            let opt =
              match payload with PStr [%str opt] -> true | _ -> false
            in
            match mod_exp.pmod_desc with
            | Pmod_structure
                ([ { pstr_desc = Pstr_type (name, variants); _ } ] as str) ->
                let type_ =
                  enum ~loc:mod_exp.pmod_loc ~opt (name, variants) ()
                in
                Ast_builder.Default.pmod_structure ~loc:mod_exp.pmod_loc
                  (str @ type_)
            | _ -> mod_exp)
        | _ -> mod_exp
    end
end

let _ = Driver.register_transformation "enum2" ~impl:Global.traverse#structure
