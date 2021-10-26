open Ppxlib

let () =
  Driver.(
    let f ctxt str =
      let loc =
        match str with
        | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
        | hd :: _ -> hd.pstr_loc
      in
      Location.raise_errorf ~loc "A located error in a preprocess"
    in
    let instrument = Instrument.V2.make f ~position:Instrument.Before in
    register_transformation ~instrument "raise_in_instrument")

let () = Ppxlib.Driver.standalone ()
