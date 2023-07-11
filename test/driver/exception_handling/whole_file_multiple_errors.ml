open Ppxlib

let () =
  Driver.V2.(
    register_transformation
      ~impl:(fun ctxt str ->
        let loc =
          match str with
          | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
          | hd :: _ -> hd.pstr_loc
        in
        Location.raise_errorf ~loc
          "A third located error in a whole codes transformations")
      "c_raise_exc_second")

let () =
  Driver.V2.(
    register_transformation
      ~impl:(fun ctxt str ->
        let loc =
          match str with
          | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
          | hd :: _ -> hd.pstr_loc
        in
        Location.raise_errorf ~loc
          "A second located error in a whole codes transformations")
      "a_raise_exc_second")

let () =
  Driver.V2.(
    register_transformation
      ~impl:(fun ctxt str ->
        let loc =
          match str with
          | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
          | hd :: _ -> hd.pstr_loc
        in
        Location.raise_errorf ~loc "A located error in a whole file transform")
      "b_raise_exc")

let () = Ppxlib.Driver.standalone ()
