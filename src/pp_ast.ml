open Import

module Config = struct
  type loc_mode = [ `Short | `Full ]
  type t = { show_attrs : bool; show_locs : bool; loc_mode : loc_mode }

  module Default = struct
    let show_attrs = false
    let show_locs = false
    let loc_mode = `Short
  end

  let default =
    let open Default in
    { show_attrs; show_locs; loc_mode }

  let make ?(show_attrs = Default.show_attrs) ?(show_locs = Default.show_locs)
      ?(loc_mode = Default.loc_mode) () =
    { show_attrs; show_locs; loc_mode }
end

let cnum (pos : Lexing.position) = pos.pos_cnum - pos.pos_bol

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
    inherit [simple_val] Ast_traverse.lift as super
    val mutable config = Config.default
    method set_config new_config = config <- new_config
    method get_config () = config
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
    method! location_stack _ls = Special "__lstack"

    method! position pos =
      match (config.Config.show_locs, config.Config.loc_mode) with
      | true, `Full -> super#position pos
      | _, _ -> Special "__pos"

    method! loc lift_a a_loc =
      match config.Config.show_locs with
      | true -> super#loc lift_a a_loc
      | false -> lift_a a_loc.txt

    method! location loc =
      match (config.Config.show_locs, config.Config.loc_mode) with
      | false, _ -> Special "__loc"
      | true, `Full -> super#location loc
      | true, `Short ->
          let begin_line = loc.loc_start.pos_lnum in
          let begin_char = cnum loc.loc_start in
          let end_line = loc.loc_end.pos_lnum in
          let end_char = cnum loc.loc_end in
          let repr =
            if Int.equal begin_line end_line then
              Format.sprintf "l%ic%i..%i" begin_line begin_char end_char
            else
              Format.sprintf "l%ic%i..l%ic%i" begin_line begin_char end_line
                end_char
          in
          let with_ghost = if loc.loc_ghost then repr ^ "(g)" else repr in
          Special with_ghost

    method! attributes attrs =
      match config.Config.show_attrs with
      | false -> Special "__attrs"
      | true -> super#attributes attrs

    method lift_record_with_desc
        : 'record 'desc.
          lift_desc:('desc -> simple_val) ->
          lift_record:('record -> simple_val) ->
          desc:'desc ->
          attrs:attributes ->
          'record ->
          simple_val =
      fun ~lift_desc ~lift_record ~desc ~attrs x ->
        match (config.show_locs, config.show_attrs, attrs) with
        | false, false, _ | false, true, [] -> lift_desc desc
        | _, true, _ | true, _, _ -> lift_record x

    method! core_type ct =
      self#lift_record_with_desc ~lift_desc:self#core_type_desc
        ~lift_record:super#core_type ~desc:ct.ptyp_desc
        ~attrs:ct.ptyp_attributes ct

    method! row_field rf =
      self#lift_record_with_desc ~lift_desc:self#row_field_desc
        ~lift_record:super#row_field ~desc:rf.prf_desc ~attrs:rf.prf_attributes
        rf

    method! object_field obf =
      self#lift_record_with_desc ~lift_desc:self#object_field_desc
        ~lift_record:super#object_field ~desc:obf.pof_desc
        ~attrs:obf.pof_attributes obf

    method! pattern pat =
      self#lift_record_with_desc ~lift_desc:self#pattern_desc
        ~lift_record:super#pattern ~desc:pat.ppat_desc
        ~attrs:pat.ppat_attributes pat

    method! expression exp =
      self#lift_record_with_desc ~lift_desc:self#expression_desc
        ~lift_record:super#expression ~desc:exp.pexp_desc
        ~attrs:exp.pexp_attributes exp

    method! class_type cty =
      self#lift_record_with_desc ~lift_desc:self#class_type_desc
        ~lift_record:super#class_type ~desc:cty.pcty_desc
        ~attrs:cty.pcty_attributes cty

    method! class_type_field ctf =
      self#lift_record_with_desc ~lift_desc:self#class_type_field_desc
        ~lift_record:super#class_type_field ~desc:ctf.pctf_desc
        ~attrs:ctf.pctf_attributes ctf

    method! class_expr cl =
      self#lift_record_with_desc ~lift_desc:self#class_expr_desc
        ~lift_record:super#class_expr ~desc:cl.pcl_desc ~attrs:cl.pcl_attributes
        cl

    method! class_field cf =
      self#lift_record_with_desc ~lift_desc:self#class_field_desc
        ~lift_record:super#class_field ~desc:cf.pcf_desc
        ~attrs:cf.pcf_attributes cf

    method! module_type mty =
      self#lift_record_with_desc ~lift_desc:self#module_type_desc
        ~lift_record:super#module_type ~desc:mty.pmty_desc
        ~attrs:mty.pmty_attributes mty

    method! module_expr mod_ =
      self#lift_record_with_desc ~lift_desc:self#module_expr_desc
        ~lift_record:super#module_expr ~desc:mod_.pmod_desc
        ~attrs:mod_.pmod_attributes mod_

    method! structure_item stri = self#structure_item_desc stri.pstr_desc
    method! signature_item sigi = self#signature_item_desc sigi.psig_desc

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

type 'node pp = ?config:Config.t -> Format.formatter -> 'node -> unit

let with_config ~config ~f =
  let old_config = lift_simple_val#get_config () in
  lift_simple_val#set_config config;
  let res = f () in
  lift_simple_val#set_config old_config;
  res

let pp_with_config (type a) (lifter : a -> simple_val)
    ?(config = Config.default) fmt (x : a) =
  with_config ~config ~f:(fun () -> pp_simple_val fmt (lifter x))

let structure = pp_with_config lift_simple_val#structure
let structure_item = pp_with_config lift_simple_val#structure_item
let signature = pp_with_config lift_simple_val#signature
let signature_item = pp_with_config lift_simple_val#signature_item
let expression = pp_with_config lift_simple_val#expression
let pattern = pp_with_config lift_simple_val#pattern
let core_type = pp_with_config lift_simple_val#core_type

module Kind = struct
  type t = Signature | Structure | Expression | Pattern | Core_type
end

module Ast = struct
  type t =
    | Str of structure
    | Sig of signature
    | Exp of expression
    | Pat of pattern
    | Typ of core_type
end

let parse_node ~kind lexbuf =
  match (kind : Kind.t) with
  | Expression -> Ast.Exp (Parse.expression lexbuf)
  | Pattern -> Ast.Pat (Parse.pattern lexbuf)
  | Core_type -> Ast.Typ (Parse.core_type lexbuf)
  | Structure -> Ast.Str (Parse.implementation lexbuf)
  | Signature -> Ast.Sig (Parse.interface lexbuf)

let pp_ast ~config ast formatter =
  match (ast : Ast.t) with
  | Str str -> structure ~config formatter str
  | Sig sig_ -> signature ~config formatter sig_
  | Exp exp -> expression ~config formatter exp
  | Pat pat -> pattern ~config formatter pat
  | Typ typ -> core_type ~config formatter typ

let sprint ?(show_attrs = false) ?(show_locs = false) ?(loc_mode = `Short)
    ?(kind = Kind.Expression) input =
  let buffer = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buffer in
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "none" };
  Astlib.Location.set_input_lexbuf (Some lexbuf);
  let ast = parse_node ~kind lexbuf in
  let config = Config.make ~show_attrs ~show_locs ~loc_mode () in
  pp_ast ~config ast formatter;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer
