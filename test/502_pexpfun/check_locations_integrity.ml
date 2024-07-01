module To_before_502 =
  Ppxlib_ast.Convert (Ppxlib_ast.Js) (Ppxlib_ast__.Versions.OCaml_501)

module OCaml_501 = Ppxlib_ast__.Versions.OCaml_501.Ast

let is_valid_location ~(parent : Location.t) ~(child : Location.t) =
  parent.loc_start <= child.loc_start && parent.loc_end >= child.loc_end

let string_of_loc (loc : Location.t) : string =
  Printf.sprintf "File \"%s\", line %d, characters %d-%d"
    loc.loc_start.pos_fname loc.loc_start.pos_lnum
    (loc.loc_start.pos_cnum - loc.loc_start.pos_bol)
    (loc.loc_end.pos_cnum - loc.loc_start.pos_bol)

let rec check_locations (expr : OCaml_501.Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_newtype (_label, exp) ->
      let parent = expr.pexp_loc in
      let child = exp.pexp_loc in
      if not (is_valid_location ~parent ~child) then
        Location.raise_errorf ~loc:parent
          "Function's location is not larger than its body\n\
           Parent location: %s\n\
           Child location: %s" (string_of_loc parent) (string_of_loc child);
      check_locations exp
  | Pexp_fun (_, _, _, body) ->
      let parent = expr.pexp_loc in
      let child = body.pexp_loc in
      if not (is_valid_location ~parent ~child) then
        Location.raise_errorf ~loc:parent
          "Function's location is not larger than its body\n\
           Parent location: %s\n\
           Child location: %s" (string_of_loc parent) (string_of_loc child);
      check_locations body
  | _ -> ()

let check_stri_locations stri =
  let open OCaml_501.Parsetree in
  match stri.pstr_desc with
  | Pstr_value (_rec_flag, [ { pvb_expr; _ } ]) -> check_locations pvb_expr
  | _ -> ()

let impl _ctxt str =
  let before_502_ast = To_before_502.copy_structure str in
  List.iter check_stri_locations before_502_ast;
  str

let () = Ppxlib.Driver.V2.register_transformation ~impl "foo"
let () = Ppxlib.Driver.standalone ()
