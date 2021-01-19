open Import

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn -> Out_channel.with_file fn ~binary ~f
;;

module Kind = struct
  type t = Intf | Impl

  let of_filename fn : t option =
    if Caml.Filename.check_suffix fn ".ml" then
      Some Impl
    else if Caml.Filename.check_suffix fn ".mli" then
      Some Intf
    else
      None
  ;;

  let describe = function
    | Impl -> "implementation"
    | Intf -> "interface"
  ;;

  let equal : t -> t -> bool = Poly.equal
end

module Ast_io = struct
  type t =
    | Intf of Compiler_ast.Parsetree.signature
    | Impl of Compiler_ast.Parsetree.structure

  type read_error =
    | Not_a_binary_ast of string
    | Unknown_version of string

  let magic_length = String.length Ocaml_common.Config.ast_impl_magic_number

  let read_magic ic =
    let buf = Bytes.create magic_length in
    let len = input ic buf 0 magic_length in
    let s = Bytes.sub_string buf ~pos:0 ~len in
    if len = magic_length then
      Ok s
    else
      Error s

  let read ic =
    match read_magic ic with
    | Error s -> Error (Not_a_binary_ast s)
    | Ok s ->
      if String.equal s Ocaml_common.Config.ast_impl_magic_number then
        let filename : string = input_value ic in
        let payload = Impl (input_value ic) in
        Ok (filename, payload)
      else if String.equal s Ocaml_common.Config.ast_intf_magic_number then
        let filename : string = input_value ic in
        let payload = Intf (input_value ic) in
        Ok (filename, payload)
      else
      if String.equal
           (String.sub s ~pos:0 ~len:9)
           (String.sub Ocaml_common.Config.ast_impl_magic_number ~pos:0 ~len:9)
      || String.equal
           (String.sub s ~pos:0 ~len:9)
           (String.sub Ocaml_common.Config.ast_intf_magic_number ~pos:0 ~len:9)
      then
        Error (Unknown_version s)
      else
        Error (Not_a_binary_ast s)

  let write oc (filename : string) x =
    match x with
    | Intf x ->
      output_string oc Ocaml_common.Config.ast_intf_magic_number;
      output_value oc filename;
      output_value oc x
    | Impl x ->
      output_string oc Ocaml_common.Config.ast_impl_magic_number;
      output_value oc filename;
      output_value oc x
end

module Intf_or_impl = struct
  type t =
    | Intf of signature
    | Impl of structure

  let map t (map : Ast_traverse.map) =
    match t with
    | Impl x -> Impl (map#structure x)
    | Intf x -> Intf (map#signature x)
  ;;

  let map_with_context t (map : _ Ast_traverse.map_with_context) ctx =
    match t with
    | Impl x -> Impl (map#structure ctx x)
    | Intf x -> Intf (map#signature ctx x)
  ;;

  let kind : _ -> Kind.t = function
    | Intf _ -> Intf
    | Impl _ -> Impl

  let of_ast_io ast : t =
    match ast with
    | Ast_io.Intf sg -> Intf (Selected_ast.Of_ocaml.copy_signature sg)
    | Ast_io.Impl st -> Impl (Selected_ast.Of_ocaml.copy_structure st)

  let to_ast_io (ast : t) ~add_ppx_context =
    match ast with
    | Intf sg ->
      let sg = Selected_ast.To_ocaml.copy_signature sg in
      let sg =
        if add_ppx_context then
          Ocaml_common.Ast_mapper.add_ppx_context_sig ~tool_name:"ppx_driver" sg
        else
          sg
      in
      Ast_io.Intf sg
    | Impl st ->
      let st = Selected_ast.To_ocaml.copy_structure st in
      let st =
        if add_ppx_context then
          Ocaml_common.Ast_mapper.add_ppx_context_str ~tool_name:"ppx_driver" st
        else
          st
      in
      Ast_io.Impl st
end
