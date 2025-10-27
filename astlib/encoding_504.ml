module Ext_name = struct
  let ptyp_labeled_tuple = "ppxlib.migration.ptyp_labeled_tuple_504"
  let pexp_labeled_tuple = "ppxlib.migration.pexp_labeled_tuple_504"
end

let invalid_encoding ~loc name =
  Location.raise_errorf ~loc "Invalid %s encoding" name

module type AST = sig
  type payload
  type core_type
  type core_type_desc
  type expression
  type expression_desc

  module Construct : sig
    val ptyp_extension_desc : string Location.loc -> payload -> core_type_desc
    val ptyp_tuple : loc:Location.t -> core_type list -> core_type
    val ptyp_var : loc:Location.t -> string -> core_type
    val ptyp_any : loc:Location.t -> core_type
    val ptyp : core_type -> payload
    val pexp_extension_desc : string Location.loc -> payload -> expression_desc
    val pexp_tuple : loc:Location.t -> expression list -> expression

    val pexp_variant :
      loc:Location.t -> string -> expression option -> expression

    val pstr_eval : loc:Location.t -> expression -> payload
  end

  module Destruct : sig
    val ptyp : payload -> core_type option
    val ptyp_tuple : core_type -> core_type list option
    val ptyp_var : core_type -> string option
    val ptyp_any : core_type -> unit option
    val pstr_eval : payload -> expression option
    val pexp_tuple : expression -> expression list option
    val pexp_variant : expression -> (string * expression option) option
  end
end

module Make (X : AST) = struct
  let encode_ptyp_labeled_tuple ~loc args =
    let payload =
      let l =
        List.map
          (fun (label_opt, typ) ->
            let label =
              match label_opt with
              | None -> X.Construct.ptyp_any ~loc
              | Some s -> X.Construct.ptyp_var ~loc s
            in
            X.Construct.ptyp_tuple ~loc [ label; typ ])
          args
      in
      X.Construct.ptyp_tuple ~loc l
    in
    X.Construct.ptyp_extension_desc
      { txt = Ext_name.ptyp_labeled_tuple; loc }
      (X.Construct.ptyp payload)

  let decode_ptyp_labeled_tuple ~loc payload =
    let open Stdlib0.Option.Op in
    let res =
      let* typ = X.Destruct.ptyp payload in
      let* typ_list = X.Destruct.ptyp_tuple typ in
      Stdlib0.Option.List.map typ_list ~f:(fun typ ->
          let* typ_pair = X.Destruct.ptyp_tuple typ in
          match typ_pair with
          | [ label; typ ] -> (
              match (X.Destruct.ptyp_var label, X.Destruct.ptyp_any label) with
              | Some s, _ -> Some (Some s, typ)
              | _, Some () -> Some (None, typ)
              | None, None -> None)
          | _ -> None)
    in
    match res with
    | Some res -> res
    | None -> invalid_encoding ~loc Ext_name.ptyp_labeled_tuple

  let encode_pexp_labeled_tuple ~loc args =
    let payload =
      let l =
        List.map
          (fun (label_opt, expr) ->
            let label =
              match label_opt with
              | None -> X.Construct.pexp_variant ~loc "None" None
              | Some s ->
                  let string_as_variant =
                    X.Construct.pexp_variant ~loc s None
                  in
                  X.Construct.pexp_variant ~loc "Some" (Some string_as_variant)
            in
            X.Construct.pexp_tuple ~loc [ label; expr ])
          args
      in
      X.Construct.pexp_tuple ~loc l
    in
    X.Construct.pexp_extension_desc
      { txt = Ext_name.pexp_labeled_tuple; loc }
      (X.Construct.pstr_eval ~loc payload)

  let decode_pexp_labeled_tuple ~loc payload =
    let open Stdlib0.Option.Op in
    let res =
      let* exp = X.Destruct.pstr_eval payload in
      let* exp_list = X.Destruct.pexp_tuple exp in
      Stdlib0.Option.List.map exp_list ~f:(fun exp ->
          let* exp_pair = X.Destruct.pexp_tuple exp in
          match exp_pair with
          | [ label; exp ] -> (
              let* opt_variant = X.Destruct.pexp_variant label in
              match opt_variant with
              | "None", None -> Some (None, exp)
              | "Some", Some exp' -> (
                  let* label_variant = X.Destruct.pexp_variant exp' in
                  match label_variant with
                  | s, None -> Some (Some s, exp)
                  | _, _ -> None)
              | _ -> None)
          | _ -> None)
    in
    match res with
    | Some res -> res
    | None -> invalid_encoding ~loc Ext_name.pexp_labeled_tuple
end

module Ast_503 = struct
  include Ast_503.Parsetree

  module Construct = struct
    let core_type ~loc ptyp_desc =
      { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

    let expression ~loc pexp_desc =
      { pexp_desc; pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [] }

    let ptyp_extension_desc name payload = Ptyp_extension (name, payload)
    let ptyp_tuple ~loc typs = core_type ~loc (Ptyp_tuple typs)
    let ptyp_var ~loc s = core_type ~loc (Ptyp_var s)
    let ptyp_any ~loc = core_type ~loc Ptyp_any
    let ptyp typ = PTyp typ
    let pexp_extension_desc name payload = Pexp_extension (name, payload)
    let pexp_tuple ~loc l = expression ~loc (Pexp_tuple l)

    let pexp_variant ~loc v exp_opt =
      expression ~loc (Pexp_variant (v, exp_opt))

    let pstr_eval ~loc expr =
      PStr [ { pstr_desc = Pstr_eval (expr, []); pstr_loc = loc } ]
  end

  module Destruct = struct
    let ptyp = function PTyp typ -> Some typ | _ -> None

    let ptyp_tuple = function
      | { ptyp_desc = Ptyp_tuple typs; _ } -> Some typs
      | _ -> None

    let ptyp_var = function
      | { ptyp_desc = Ptyp_var s; _ } -> Some s
      | _ -> None

    let ptyp_any = function { ptyp_desc = Ptyp_any; _ } -> Some () | _ -> None

    let pstr_eval = function
      | PStr [ { pstr_desc = Pstr_eval (expr, []); _ } ] -> Some expr
      | _ -> None

    let pexp_tuple = function
      | { pexp_desc = Pexp_tuple l; _ } -> Some l
      | _ -> None

    let pexp_variant = function
      | { pexp_desc = Pexp_variant (s, e); _ } -> Some (s, e)
      | _ -> None
  end
end

module Ast_502 = struct
  include Ast_502.Parsetree

  module Construct = struct
    let core_type ~loc ptyp_desc =
      { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

    let expression ~loc pexp_desc =
      { pexp_desc; pexp_loc = loc; pexp_attributes = []; pexp_loc_stack = [] }

    let ptyp_extension_desc name payload = Ptyp_extension (name, payload)
    let ptyp_tuple ~loc typs = core_type ~loc (Ptyp_tuple typs)
    let ptyp_var ~loc s = core_type ~loc (Ptyp_var s)
    let ptyp_any ~loc = core_type ~loc Ptyp_any
    let ptyp typ = PTyp typ
    let pexp_extension_desc name payload = Pexp_extension (name, payload)
    let pexp_tuple ~loc l = expression ~loc (Pexp_tuple l)

    let pexp_variant ~loc v exp_opt =
      expression ~loc (Pexp_variant (v, exp_opt))

    let pstr_eval ~loc expr =
      PStr [ { pstr_desc = Pstr_eval (expr, []); pstr_loc = loc } ]
  end

  module Destruct = struct
    let ptyp = function PTyp typ -> Some typ | _ -> None

    let ptyp_tuple = function
      | { ptyp_desc = Ptyp_tuple typs; _ } -> Some typs
      | _ -> None

    let ptyp_var = function
      | { ptyp_desc = Ptyp_var s; _ } -> Some s
      | _ -> None

    let ptyp_any = function { ptyp_desc = Ptyp_any; _ } -> Some () | _ -> None

    let pstr_eval = function
      | PStr [ { pstr_desc = Pstr_eval (expr, []); _ } ] -> Some expr
      | _ -> None

    let pexp_tuple = function
      | { pexp_desc = Pexp_tuple l; _ } -> Some l
      | _ -> None

    let pexp_variant = function
      | { pexp_desc = Pexp_variant (s, e); _ } -> Some (s, e)
      | _ -> None
  end
end

module To_503 = struct
  include Make (Ast_503)
end

module To_502 = struct
  include Make (Ast_502)
end
