open Ppxlib

let () =
  let lexing_loc1 =
    Lexing.{ pos_fname = "impl.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 3 }
  and lexing_loc2 =
    Lexing.{ pos_fname = "impl.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 7 }
  in
  let loc =
    Location.
      { loc_start = lexing_loc1; loc_end = lexing_loc2; loc_ghost = false }
  in
  Driver.V2.(
    register_transformation
      ~impl:(fun _ _ ->
        Location.raise_errorf ~loc "A located error in a whole file transform")
      "raise_exc")

let () = Ppxlib.Driver.standalone ()
