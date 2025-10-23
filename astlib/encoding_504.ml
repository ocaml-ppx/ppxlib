module Ext_name = struct
  let ptyp_labeled_tuple = "ppxlib.migration.ptyp_labeled_tuple_504"
end

module type AST = sig
  type payload
  type core_type
  type core_type_desc

  module Construct : sig
    val ptyp_extension_desc : string Location.loc -> payload -> core_type_desc
    val ptyp_tuple : loc:Location.t -> core_type list -> core_type
    val ptyp_var : loc:Location.t -> string -> core_type
    val ptyp_any : loc:Location.t -> core_type
    val ptyp : core_type -> payload
  end

  module Destruct : sig
    val ptyp : payload -> core_type option
    val ptyp_tuple : core_type -> core_type list option
    val ptyp_var : core_type -> string option
    val ptyp_any : core_type -> unit option
  end
end

module type S = sig
  type payload
  type core_type
  type core_type_desc

  val encode_ptyp_labeled_tuple :
    loc:Location.t -> (string option * core_type) list -> core_type_desc

  val decode_ptyp_labeled_tuple :
    loc:Location.t -> payload -> (string option * core_type) list
end

module Make (X : AST) :
  S
    with type core_type = X.core_type
     and type core_type_desc = X.core_type_desc
     and type payload = X.payload = struct
  type payload = X.payload
  type core_type = X.core_type
  type core_type_desc = X.core_type_desc

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
    | None ->
        Location.raise_errorf ~loc "Invalid %s encoding"
          Ext_name.ptyp_labeled_tuple
end

module Ast_503 = struct
  include Ast_503.Parsetree

  module Construct = struct
    let core_type ~loc ptyp_desc =
      { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

    let ptyp_extension_desc name payload = Ptyp_extension (name, payload)
    let ptyp_tuple ~loc typs = core_type ~loc (Ptyp_tuple typs)
    let ptyp_var ~loc s = core_type ~loc (Ptyp_var s)
    let ptyp_any ~loc = core_type ~loc Ptyp_any
    let ptyp typ = PTyp typ
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
  end
end

module Ast_502 = struct
  include Ast_502.Parsetree

  module Construct = struct
    let core_type ~loc ptyp_desc =
      { ptyp_desc; ptyp_loc = loc; ptyp_attributes = []; ptyp_loc_stack = [] }

    let ptyp_extension_desc name payload = Ptyp_extension (name, payload)
    let ptyp_tuple ~loc typs = core_type ~loc (Ptyp_tuple typs)
    let ptyp_var ~loc s = core_type ~loc (Ptyp_var s)
    let ptyp_any ~loc = core_type ~loc Ptyp_any
    let ptyp typ = PTyp typ
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
  end
end

module To_503 = struct
  include Make (Ast_503)
end

module To_502 = struct
  include Make (Ast_502)
end
