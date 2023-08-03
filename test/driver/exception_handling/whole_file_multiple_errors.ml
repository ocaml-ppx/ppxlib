open Ppxlib

(* let () =
   let instrument =
     let transformation ctxt str =
       let loc =
         match str with
         | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
         | hd :: _ -> hd.pstr_loc
       in
       Location.raise_errorf ~loc
         "Raising a located exception during the first instrumentation phase"
     in
     Driver.Instrument.V2.make ~position:Driver.Instrument.Before transformation
   in
   Driver.V2.(register_transformation ~instrument "a_raise_exc") *)

let expand ~ctxt env_var =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  match Sys.getenv env_var with
  | value -> Ast_builder.Default.estring ~loc value
  | exception Not_found ->
      let ext =
        Location.raise_errorf ~loc "The environement variable %s is unbound"
          env_var
      in
      Ast_builder.Default.pexp_extension ~loc ext

let my_extension =
  Extension.V3.declare "get_env" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "get_env"
(* let () =
     Driver.V2.(
       register_transformation
         ~impl:(fun ctxt str ->
           let loc =
             match str with
             | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
             | hd :: _ -> hd.pstr_loc
           in
           Location.raise_errorf ~loc
             "Raising a located exception during the Global transformation phase")
         "b_raise_exc_second")

   let () =
     let instrument =
       let transformation ctxt str =
         let loc =
           match str with
           | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
           | hd :: _ -> hd.pstr_loc
         in
         Location.raise_errorf ~loc
           "Raising a located exception during the Last instrumentation phase"
       in
       Driver.Instrument.V2.make ~position:Driver.Instrument.After transformation
     in
     Driver.V2.(register_transformation ~instrument "c_raise_exc")
*)

let () = Ppxlib.Driver.standalone ()
