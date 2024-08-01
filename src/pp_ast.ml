open Import

type simple_val =
  | Unit
  | Int of int
  | String of string
  | Bool of bool
  | Char of char
  | Array of simple_val list
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Record of (string * simple_val) list
  | Constr of string * simple_val list
  | Tuple of simple_val list
  | List of simple_val list
  | Special of string

let pp_collection ~pp_elm ~open_ ~close ~sep fmt l =
  match l with
  | [] -> Format.fprintf fmt "%s%s" open_ close
  | hd :: tl ->
      Format.fprintf fmt "@[<hv>%s %a@," open_ pp_elm hd;
      List.iter tl ~f:(fun sv -> Format.fprintf fmt "%s %a@," sep pp_elm sv);
      Format.fprintf fmt "%s@]" close

let rec pp_simple_val fmt simple_val =
  match simple_val with
  | Unit -> Format.fprintf fmt "()"
  | Int i -> Format.fprintf fmt "%i" i
  | String s -> Format.fprintf fmt "%S" s
  | Special s -> Format.fprintf fmt "%s" s
  | Bool b -> Format.fprintf fmt "%B" b
  | Char c -> Format.fprintf fmt "%c" c
  | Float f -> Format.fprintf fmt "%f" f
  | Int32 i32 -> Format.fprintf fmt "%li" i32
  | Int64 i64 -> Format.fprintf fmt "%Li" i64
  | Nativeint ni -> Format.fprintf fmt "%ni" ni
  | Array l ->
      pp_collection ~pp_elm:pp_simple_val ~open_:"[|" ~close:"|]" ~sep:";" fmt l
  | Tuple l ->
      pp_collection ~pp_elm:pp_simple_val ~open_:"(" ~close:")" ~sep:"," fmt l
  | List l ->
      pp_collection ~pp_elm:pp_simple_val ~open_:"[" ~close:"]" ~sep:";" fmt l
  | Record fields ->
      pp_collection ~pp_elm:pp_field ~open_:"{" ~close:"}" ~sep:";" fmt fields
  | Constr (cname, []) -> Format.fprintf fmt "%s" cname
  | Constr (cname, [ (Constr (_, _ :: _) as x) ]) ->
      Format.fprintf fmt "@[<hv 2>%s@ (%a)@]" cname pp_simple_val x
  | Constr (cname, [ x ]) ->
      Format.fprintf fmt "@[<hv 2>%s@ %a@]" cname pp_simple_val x
  | Constr (cname, l) ->
      Format.fprintf fmt "@[<hv 2>%s@ %a@]" cname pp_simple_val (Tuple l)

and pp_field fmt (fname, simple_val) =
  Format.fprintf fmt "@[<hv 2>%s =@ %a@]" fname pp_simple_val simple_val

class lift_simple_val =
  object (self)
    inherit [simple_val] Ast_traverse.lift
    method unit () = Unit
    method int i = Int i
    method string s = String s
    method bool b = Bool b
    method char c = Char c
    method float f = Float f
    method int32 i32 = Int32 i32
    method int64 i64 = Int64 i64
    method nativeint ni = Nativeint ni
    method! list lift_a list = List (List.map ~f:lift_a list)
    method tuple res_list = Tuple res_list
    method record fields = Record fields
    method constr ctr res_list = Constr (ctr, res_list)

    method array lift_a array =
      Array (Array.map ~f:lift_a array |> Array.to_list)

    method other _a = Special "__"
    method! location _loc = Special "__loc"
    method! location_stack _ls = Special "__lstack"
    method! position _p = Special "__pos"
    method! attributes _a = Special "__attrs"
    method! loc lift_a a_loc = lift_a a_loc.txt
    method! core_type ct = self#core_type_desc ct.ptyp_desc
    method! row_field rf = self#row_field_desc rf.prf_desc
    method! object_field obf = self#object_field_desc obf.pof_desc
    method! pattern pat = self#pattern_desc pat.ppat_desc
    method! expression exp = self#expression_desc exp.pexp_desc
    method! class_type cty = self#class_type_desc cty.pcty_desc
    method! class_type_field ctf = self#class_type_field_desc ctf.pctf_desc
    method! class_expr cl = self#class_expr_desc cl.pcl_desc
    method! class_field cf = self#class_field_desc cf.pcf_desc
    method! module_type mty = self#module_type_desc mty.pmty_desc
    method! signature_item sigi = self#signature_item_desc sigi.psig_desc
    method! module_expr mod_ = self#module_expr_desc mod_.pmod_desc
    method! structure_item stri = self#structure_item_desc stri.pstr_desc

    method! directive_argument dira =
      self#directive_argument_desc dira.pdira_desc

    method! rec_flag rec_flag =
      match rec_flag with
      | Nonrecursive -> Constr ("Nonrecursive", [])
      | Recursive -> Constr ("Recursive", [])

    method! direction_flag direction_flag =
      match direction_flag with
      | Upto -> Constr ("Upto", [])
      | Downto -> Constr ("Downto", [])

    method! private_flag private_flag =
      match private_flag with
      | Private -> Constr ("Private", [])
      | Public -> Constr ("Public", [])

    method! mutable_flag mutable_flag =
      match mutable_flag with
      | Mutable -> Constr ("Mutable", [])
      | Immutable -> Constr ("Immutable", [])

    method! virtual_flag virtual_flag =
      match virtual_flag with
      | Virtual -> Constr ("Virtual", [])
      | Concrete -> Constr ("Concrete", [])

    method! override_flag override_flag =
      match override_flag with
      | Override -> Constr ("Override", [])
      | Fresh -> Constr ("Fresh", [])

    method! closed_flag closed_flag =
      match closed_flag with
      | Closed -> Constr ("Closed", [])
      | Open -> Constr ("Open", [])

    method! variance variance =
      match variance with
      | Covariant -> Constr ("Covariant", [])
      | Contravariant -> Constr ("Contravariant", [])
      | NoVariance -> Constr ("NoVariance", [])

    method! injectivity injectivity =
      match injectivity with
      | Injective -> Constr ("Injective", [])
      | NoInjectivity -> Constr ("NoInjectivity", [])
  end

let lift_simple_val = new lift_simple_val
let structure fmt str = pp_simple_val fmt (lift_simple_val#structure str)
let signature fmt str = pp_simple_val fmt (lift_simple_val#signature str)
let expression fmt str = pp_simple_val fmt (lift_simple_val#expression str)
let pattern fmt str = pp_simple_val fmt (lift_simple_val#pattern str)
let core_type fmt str = pp_simple_val fmt (lift_simple_val#core_type str)
