open Stdppx
open Ppxlib

let pprint_ctxt ctxt =
  let tool_name = Expansion_context.Base.tool_name ctxt in
  let input_name = Expansion_context.Base.input_name ctxt in
  let file_path =
    Code_path.file_path @@ Expansion_context.Base.code_path @@ ctxt
  in
  Printf.printf "tool_name: %s\ninput_name: %s\nfile_path: %s\n" tool_name
    input_name file_path

let side_print_ctxt =
  object
    inherit Ast_traverse.map_with_expansion_context_and_errors as super

    method! structure ctxt st =
      pprint_ctxt ctxt;
      super#structure ctxt st

    method! signature ctxt sg =
      pprint_ctxt ctxt;
      super#signature ctxt sg
  end

let () =
  Driver.V2.(
    register_transformation
      ~impl:(fun ctxt structure ->
        let structure, errors = side_print_ctxt#structure ctxt structure in
        (errors
        |> List.map ~f:Location.Error.to_extension
        |> List.map ~f:(Extension.Context.node_of_extension Structure_item))
        @ structure)
      ~intf:(fun ctxt signature ->
        let signature, errors = side_print_ctxt#signature ctxt signature in
        (errors
        |> List.map ~f:Location.Error.to_extension
        |> List.map ~f:(Extension.Context.node_of_extension Signature_item))
        @ signature)
      "print_ctxt")
