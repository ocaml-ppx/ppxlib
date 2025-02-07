type t =
  { x : int
  ; y : u
  }

and u = A of int | B of t
[@@deriving traverse]
[%%expect{|
type t = { x : int; y : u; }
and u = A of int | B of t
class virtual map :
  object
    method virtual int : int -> int
    method t : t -> t
    method u : u -> u
  end
class virtual iter :
  object
    method virtual int : int -> unit
    method t : t -> unit
    method u : u -> unit
  end
class virtual ['acc] fold :
  object
    method virtual int : int -> 'acc -> 'acc
    method t : t -> 'acc -> 'acc
    method u : u -> 'acc -> 'acc
  end
class virtual ['acc] fold_map :
  object
    method virtual int : int -> 'acc -> int * 'acc
    method t : t -> 'acc -> t * 'acc
    method u : u -> 'acc -> u * 'acc
  end
class virtual ['ctx] map_with_context :
  object
    method virtual int : 'ctx -> int -> int
    method t : 'ctx -> t -> t
    method u : 'ctx -> u -> u
  end
class virtual ['res] lift :
  object
    method virtual constr : string -> 'res list -> 'res
    method virtual int : int -> 'res
    method virtual record : (string * 'res) list -> 'res
    method t : t -> 'res
    method u : u -> 'res
  end
class virtual ['ctx, 'res] lift_map_with_context :
  object
    method virtual constr : 'ctx -> string -> 'res list -> 'res
    method virtual int : 'ctx -> int -> int * 'res
    method virtual record : 'ctx -> (string * 'res) list -> 'res
    method t : 'ctx -> t -> t * 'res
    method u : 'ctx -> u -> u * 'res
  end
|}]

type t =
  { a : int
  ; b : Int.t
  ; c : (int, bool) Stdlib.Result.t
  ; d : int Map.Make(Int).t
  }
[@@deriving traverse_iter]
[%%expect{|
type t = {
  a : int;
  b : int;
  c : (int, bool) result;
  d : int Map.Make(Int).t;
}
class virtual iter :
  object
    method virtual bool : bool -> unit
    method virtual int : int -> unit
    method virtual int__t : int -> unit
    method virtual map__make_'int'__t :
      ('a -> unit) -> 'a Map.Make(Int).t -> unit
    method virtual stdlib__result__t :
      ('a -> unit) -> ('b -> unit) -> ('a, 'b) result -> unit
    method t : t -> unit
  end
|}]

type t = Inline of { a : string option; b : t }
[@@deriving traverse]
[%%expect{|
type t = Inline of { a : string option; b : t; }
class virtual map :
  object
    method virtual option : ('a -> 'a) -> 'a option -> 'a option
    method virtual string : string -> string
    method t : t -> t
  end
class virtual iter :
  object
    method virtual option : ('a -> unit) -> 'a option -> unit
    method virtual string : string -> unit
    method t : t -> unit
  end
class virtual ['acc] fold :
  object
    method virtual option : ('a -> 'acc -> 'acc) -> 'a option -> 'acc -> 'acc
    method virtual string : string -> 'acc -> 'acc
    method t : t -> 'acc -> 'acc
  end
class virtual ['acc] fold_map :
  object
    method virtual option :
      ('a -> 'acc -> 'a * 'acc) -> 'a option -> 'acc -> 'a option * 'acc
    method virtual string : string -> 'acc -> string * 'acc
    method t : t -> 'acc -> t * 'acc
  end
class virtual ['ctx] map_with_context :
  object
    method virtual option :
      ('ctx -> 'a -> 'a) -> 'ctx -> 'a option -> 'a option
    method virtual string : 'ctx -> string -> string
    method t : 'ctx -> t -> t
  end
class virtual ['res] lift :
  object
    method virtual constr : string -> 'res list -> 'res
    method virtual option : ('a -> 'res) -> 'a option -> 'res
    method virtual record : (string * 'res) list -> 'res
    method virtual string : string -> 'res
    method t : t -> 'res
  end
class virtual ['ctx, 'res] lift_map_with_context :
  object
    method virtual constr : 'ctx -> string -> 'res list -> 'res
    method virtual option :
      ('ctx -> 'a -> 'a * 'res) -> 'ctx -> 'a option -> 'a option * 'res
    method virtual record : 'ctx -> (string * 'res) list -> 'res
    method virtual string : 'ctx -> string -> string * 'res
    method t : 'ctx -> t -> t * 'res
  end
|}]

(* Test [Ast_traverse.sexp_of] and compare it visually to [Pprintast]. *)
let via_pprintast, via_sexp_of =
  let open Stdppx in
  (* Pretty-print a string by turning it into a multi-line list, all padded to the same
     length. This forces the value printer to split every string onto its own line.
     Otherwise it may put multiple indented strings onto one line, which is unreadable. *)
  let pretty string =
    let lines = String.split_on_char string ~sep:'\n' in
    let len =
      List.fold_left lines ~init:0 ~f:(fun acc string ->
        Int.max acc (String.length string))
    in
    List.map lines ~f:(fun string ->
      string ^ String.make (len - String.length string) ' ')
  in
  (* Tests dotted identifier, infix operator, attributes, and [Location.none]. *)
  let expr =
    let loc = Ppxlib.Location.none in
    [%expr
      function
      | 0 -> true
      | 1 -> false
      | n -> (f [@tailcall]) (Stdlib.Int.( - ) n 2)]
  in
  (* Tests locations and [loc_ghost]. *)
  let structure =
    let loc : Ppxlib.Location.t =
      {
        loc_ghost = true;
        loc_start = { pos_fname = "file.ml"; pos_lnum = 2; pos_bol = 1; pos_cnum = 2 };
        loc_end = { pos_fname = "file.ml"; pos_lnum = 4; pos_bol = 6; pos_cnum = 9 };
      }
    in
    [%str
      module M = struct
        let rec f = [%e expr]
      end]
  in
  (* Render two different ways. *)
  let via_pprintast = Ppxlib.Pprintast.string_of_structure structure |> pretty in
  let via_sexp_of =
    structure
    |> Ppxlib.Ast_traverse.sexp_of#structure
    |> Sexp.to_string_hum
    |> pretty
  in
  via_pprintast, via_sexp_of
[%%expect{|
val via_pprintast : string list =
  ["module M =                                         ";
   "  struct                                           ";
   "    let rec f =                                    ";
   "      function                                     ";
   "      | 0 -> true                                  ";
   "      | 1 -> false                                 ";
   "      | n -> ((f)[@tailcall ]) (Stdlib.Int.(-) n 2)";
   "  end                                              "]
val via_sexp_of : string list =
  ["(((pstr_desc                                                                  ";
   "   (Pstr_module                                                               ";
   "    ((pmb_name                                                                ";
   "      ((txt (M))                                                              ";
   "       (loc                                                                   ";
   "        ((loc_start                                                           ";
   "          ((pos_fname file.ml) (pos_lnum 2) (pos_bol 1) (pos_cnum 2)))        ";
   "         (loc_end                                                             ";
   "          ((pos_fname file.ml) (pos_lnum 4) (pos_bol 6) (pos_cnum 9)))        ";
   "         (loc_ghost true)))))                                                 ";
   "     (pmb_expr                                                                ";
   "      ((pmod_desc                                                             ";
   "        (Pmod_structure                                                       ";
   "         (((pstr_desc                                                         ";
   "            (Pstr_value Recursive                                             ";
   "             (((pvb_pat                                                       ";
   "                ((ppat_desc                                                   ";
   "                  (Ppat_var                                                   ";
   "                   ((txt f)                                                   ";
   "                    (loc                                                      ";
   "                     ((loc_start                                              ";
   "                       ((pos_fname file.ml) (pos_lnum 2) (pos_bol 1)          ";
   "                        (pos_cnum 2)))                                        ";
   "                      (loc_end                                                ";
   "                       ((pos_fname file.ml) (pos_lnum 4) (pos_bol 6)          ";
   "                        (pos_cnum 9)))                                        ";
   "                      (loc_ghost true))))))                                   ";
   "                 (ppat_loc                                                    ";
   "                  ((loc_start                                                 ";
   "                    ((pos_fname file.ml) (pos_lnum 2) (pos_bol 1)             ";
   "                     (pos_cnum 2)))                                           ";
   "                   (loc_end                                                   ";
   "                    ((pos_fname file.ml) (pos_lnum 4) (pos_bol 6)             ";
   "                     (pos_cnum 9)))                                           ";
   "                   (loc_ghost true)))                                         ";
   "                 (ppat_loc_stack ()) (ppat_attributes ())))                   ";
   "               (pvb_expr                                                      ";
   "                ((pexp_desc                                                   ";
   "                  (Pexp_function                                              ";
   "                   (((pc_lhs                                                  ";
   "                      ((ppat_desc (Ppat_constant (Pconst_integer 0 ())))      ";
   "                       (ppat_loc                                              ";
   "                        ((loc_start                                           ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_end                                             ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_ghost true)))                                   ";
   "                       (ppat_loc_stack ()) (ppat_attributes ())))             ";
   "                     (pc_guard ())                                            ";
   "                     (pc_rhs                                                  ";
   "                      ((pexp_desc                                             ";
   "                        (Pexp_construct                                       ";
   "                         ((txt (Lident true))                                 ";
   "                          (loc                                                ";
   "                           ((loc_start                                        ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_end                                          ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_ghost true))))                               ";
   "                         ()))                                                 ";
   "                       (pexp_loc                                              ";
   "                        ((loc_start                                           ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_end                                             ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_ghost true)))                                   ";
   "                       (pexp_loc_stack ()) (pexp_attributes ()))))            ";
   "                    ((pc_lhs                                                  ";
   "                      ((ppat_desc (Ppat_constant (Pconst_integer 1 ())))      ";
   "                       (ppat_loc                                              ";
   "                        ((loc_start                                           ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_end                                             ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_ghost true)))                                   ";
   "                       (ppat_loc_stack ()) (ppat_attributes ())))             ";
   "                     (pc_guard ())                                            ";
   "                     (pc_rhs                                                  ";
   "                      ((pexp_desc                                             ";
   "                        (Pexp_construct                                       ";
   "                         ((txt (Lident false))                                ";
   "                          (loc                                                ";
   "                           ((loc_start                                        ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_end                                          ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_ghost true))))                               ";
   "                         ()))                                                 ";
   "                       (pexp_loc                                              ";
   "                        ((loc_start                                           ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_end                                             ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_ghost true)))                                   ";
   "                       (pexp_loc_stack ()) (pexp_attributes ()))))            ";
   "                    ((pc_lhs                                                  ";
   "                      ((ppat_desc                                             ";
   "                        (Ppat_var                                             ";
   "                         ((txt n)                                             ";
   "                          (loc                                                ";
   "                           ((loc_start                                        ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_end                                          ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_ghost true))))))                             ";
   "                       (ppat_loc                                              ";
   "                        ((loc_start                                           ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_end                                             ";
   "                          ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)        ";
   "                           (pos_cnum -1)))                                    ";
   "                         (loc_ghost true)))                                   ";
   "                       (ppat_loc_stack ()) (ppat_attributes ())))             ";
   "                     (pc_guard ())                                            ";
   "                     (pc_rhs                                                  ";
   "                      ((pexp_desc                                             ";
   "                        (Pexp_apply                                           ";
   "                         ((pexp_desc                                          ";
   "                           (Pexp_ident                                        ";
   "                            ((txt (Lident f))                                 ";
   "                             (loc                                             ";
   "                              ((loc_start                                     ";
   "                                ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)  ";
   "                                 (pos_cnum -1)))                              ";
   "                               (loc_end                                       ";
   "                                ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)  ";
   "                                 (pos_cnum -1)))                              ";
   "                               (loc_ghost true))))))                          ";
   "                          (pexp_loc                                           ";
   "                           ((loc_start                                        ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_end                                          ";
   "                             ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)     ";
   "                              (pos_cnum -1)))                                 ";
   "                            (loc_ghost true)))                                ";
   "                          (pexp_loc_stack                                     ";
   "                           (((loc_start                                       ";
   "                              ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)    ";
   "                               (pos_cnum -1)))                                ";
   "                             (loc_end                                         ";
   "                              ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)    ";
   "                               (pos_cnum -1)))                                ";
   "                             (loc_ghost true))))                              ";
   "                          (pexp_attributes                                    ";
   "                           (((attr_name                                       ";
   "                              ((txt tailcall)                                 ";
   "                               (loc                                           ";
   "                                ((loc_start                                   ";
   "                                  ((pos_fname _none_) (pos_lnum 1)            ";
   "                                   (pos_bol 0) (pos_cnum -1)))                ";
   "                                 (loc_end                                     ";
   "                                  ((pos_fname _none_) (pos_lnum 1)            ";
   "                                   (pos_bol 0) (pos_cnum -1)))                ";
   "                                 (loc_ghost true)))))                         ";
   "                             (attr_payload (PStr ()))                         ";
   "                             (attr_loc                                        ";
   "                              ((loc_start                                     ";
   "                                ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)  ";
   "                                 (pos_cnum -1)))                              ";
   "                               (loc_end                                       ";
   "                                ((pos_fname _none_) (pos_lnum 1) (pos_bol 0)  ";
   "                                 (pos_cnum -1)))                              ";
   "                               (loc_ghost true)))))))                         ";
   "                         ((Nolabel                                            ";
   "                           ((pexp_desc                                        ";
   "                             (Pexp_apply                                      ";
   "                              ((pexp_desc                                     ";
   "                                (Pexp_ident                                   ";
   "                                 ((txt (Ldot (Ldot (Lident Stdlib) Int) -))   ";
   "                                  (loc                                        ";
   "                                   ((loc_start                                ";
   "                                     ((pos_fname _none_) (pos_lnum 1)         ";
   "                                      (pos_bol 0) (pos_cnum -1)))             ";
   "                                    (loc_end                                  ";
   "                                     ((pos_fname _none_) (pos_lnum 1)         ";
   "                                      (pos_bol 0) (pos_cnum -1)))             ";
   "                                    (loc_ghost true))))))                     ";
   "                               (pexp_loc                                      ";
   "                                ((loc_start                                   ";
   "                                  ((pos_fname _none_) (pos_lnum 1)            ";
   "                                   (pos_bol 0) (pos_cnum -1)))                ";
   "                                 (loc_end                                     ";
   "                                  ((pos_fname _none_) (pos_lnum 1)            ";
   "                                   (pos_bol 0) (pos_cnum -1)))                ";
   "                                 (loc_ghost true)))                           ";
   "                               (pexp_loc_stack ()) (pexp_attributes ()))      ";
   "                              ((Nolabel                                       ";
   "                                ((pexp_desc                                   ";
   "                                  (Pexp_ident                                 ";
   "                                   ((txt (Lident n))                          ";
   "                                    (loc                                      ";
   "                                     ((loc_start                              ";
   "                                       ((pos_fname _none_) (pos_lnum 1)       ";
   "                                        (pos_bol 0) (pos_cnum -1)))           ";
   "                                      (loc_end                                ";
   "                                       ((pos_fname _none_) (pos_lnum 1)       ";
   "                                        (pos_bol 0) (pos_cnum -1)))           ";
   "                                      (loc_ghost true))))))                   ";
   "                                 (pexp_loc                                    ";
   "                                  ((loc_start                                 ";
   "                                    ((pos_fname _none_) (pos_lnum 1)          ";
   "                                     (pos_bol 0) (pos_cnum -1)))              ";
   "                                   (loc_end                                   ";
   "                                    ((pos_fname _none_) (pos_lnum 1)          ";
   "                                     (pos_bol 0) (pos_cnum -1)))              ";
   "                                   (loc_ghost true)))                        "... (* string length 78; truncated *);
   "                                 (pexp_loc_stack ()) (pexp_attributes ()))) "... (* string length 78; truncated *);
   "                               (Nolabel                                    "... (* string length 78; truncated *);
   "                                ((pexp_desc                               "... (* string length 78; truncated *);
   "                                  (Pexp_constant (Pconst_integer 2 ()))) "... (* string length 78; truncated *);
   "                                 (pexp_loc                              "... (* string length 78; truncated *);
   "                                  ((loc_start                          "... (* string length 78; truncated *);
   "                                    ((pos_fname _none_) (pos_lnum 1)  "... (* string length 78; truncated *);
   "                                     (pos_bol 0) (pos_cnum -1)))     "... (* string length 78; truncated *);
   "                                   (loc_end                         "... (* string length 78; truncated *);
   "                                    ((pos_fname _none_) (pos_lnum 1"... (* string length 78; truncated *);
   "                                     (pos_bol 0) (pos_cnum -1)))  "... (* string length 78; truncated *);
   "                                   (loc_ghost true)))            "... (* string length 78; truncated *);
   "                                 (pexp_loc_stack ()) (pexp_attri"... (* string length 78; truncated *);
   "                            (pexp_loc                          "... (* string length 78; truncated *);
   "                             ((loc_start                      "... (* string length 78; truncated *);
   "                               ((pos_fname _none_) (pos_lnum "... (* string length 78; truncated *);
   "                                (pos_cnum -1)))             "... (* string length 78; truncated *);
   "                              (loc_end                     "... (* string length 78; truncated *);
   "                               ((pos_fname _none_) (pos_ln"... (* string length 78; truncated *);
   "                                (pos_cnum -1)))          "... (* string length 78; truncated *);
   "                              (loc_ghost true)))        "... (* string length 78; truncated *);
   "                            (pexp_loc_stack            "... (* string length 78; truncated *);
   "                             (((loc_start             "... (* string length 78; truncated *);
   "                                ((pos_fname _none_) ("... (* string length 78; truncated *);
   "                                 (pos_cnum -1)))    "... (* string length 78; truncated *);
   "                               (loc_end            "... (* string length 78; truncated *);
   "                                ((pos_fname _none_"... (* string length 78; truncated *);
   "                                 (pos_cnum -1))) "... (* string length 78; truncated *);
   "                               (loc_ghost true))"... (* string length 78; truncated *);
   "                            (pexp_attributes ()"... (* string length 78; truncated *);
   "                       (pexp_loc              "... (* string length 78; truncated *);
   "                        ((loc_start          "... (* string length 78; truncated *);
   "                          ((pos_fname _none_"... (* string length 78; truncated *);
   "                           (pos_cnum -1))) "... (* string length 78; truncated *);
   "                         (loc_end         "... (* string length 78; truncated *);
   "                          ((pos_fname _no"... (* string length 78; truncated *);
   "                           (pos_cnum -1)"... (* string length 78; truncated *);
   "                         (loc_ghost tru"... (* string length 78; truncated *);
   "                       (pexp_loc_stack"... (* string length 78; truncated *);
   "                 (pexp_loc           "... (* string length 78; truncated *);
   "                  ((loc_start       "... (* string length 78; truncated *);
   "                    ((pos_fname _no"... (* string length 78; truncated *);
   "                     (pos_cnum -1)"... (* string length 78; truncated *);
   "                   (loc_end      "... (* string length 78; truncated *);
   "                    ((pos_fname "... (* string length 78; truncated *);
   "                     (pos_cnum "... (* string length 78; truncated *);
   "                   (loc_ghost "... (* string length 78; truncated *);
   "                 (pexp_loc_st"... (* string length 78; truncated *);
   "               (pvb_attribut"... (* string length 78; truncated *);
   "               (pvb_loc    "... (* string length 78; truncated *);
   "                ((loc_star"... (* string length 78; truncated *);
   "                  ((pos_f"... (* string length 78; truncated *);
   "                 (loc_en"... (* string length 78; truncated *);
   "                  ((pos"... (* string length 78; truncated *);
   "                 (loc_"... (* string length 78; truncated *);
   "           (pstr_loc "... (* string length 78; truncated *);
   "            ((loc_st"... (* string length 78; truncated *);
   "              ((pos"... (* string length 78; truncated *);
   "             (loc_"... (* string length 78; truncated *);
   "              ((p"... (* string length 78; truncated *);
   "             (lo"... (* string length 78; truncated *);
   "       (pmod_lo"... (* string length 78; truncated *);
   "        ((loc_"... (* string length 78; truncated *);
   "          ((p"... (* string length 78; truncated *);
   "         (lo"... (* string length 78; truncated *);
   "          ("... (* string length 78; truncated *);
   "         ("... (* string length 78; truncated *);
   "       (p"... (* string length 78; truncated *);
   "     (pm"... (* string length 78; truncated *);
   "     (pm"... (* string length 78; truncated *);
   "      (("... (* string length 78; truncated *);
   "        "... (* string length 78; truncated *);
   "       ("... (* string length 78; truncated *);
   "       ("... (* string length 78; truncated *);
   "  (pstr_"... (* string length 78; truncated *);
   "   ((loc"... (* string length 78; truncated *);
   "    (loc"... (* string length 78; truncated *); ...]
|}]
