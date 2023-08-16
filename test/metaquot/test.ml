let loc = Ppxlib.Location.none
[%%expect{|
val loc : Warnings.loc =
  {Ppxlib.Location.loc_start =
    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
   loc_end =
    {Lexing.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0; pos_cnum = -1};
   loc_ghost = true}
|}]

(* unannotated quotations *)

let _ = [%expr ()]
[%%expect{|
- : Ppxlib.expression =
{Ppxlib_ast.Ast.pexp_desc =
  Ppxlib_ast.Ast.Pexp_construct
   ({Ppxlib_ast.Ast.txt = Ppxlib_ast.Ast.Lident "()";
     loc =
      {Ppxlib_ast.Ast.loc_start =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_end =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_ghost = true}},
   None);
 pexp_loc =
  {Ppxlib_ast.Ast.loc_start =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_end =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_ghost = true};
 pexp_loc_stack = []; pexp_attributes = []}
|}]

let _ = [%pat? ()]
[%%expect{|
- : Ppxlib.pattern =
{Ppxlib_ast.Ast.ppat_desc =
  Ppxlib_ast.Ast.Ppat_construct
   ({Ppxlib_ast.Ast.txt = Ppxlib_ast.Ast.Lident "()";
     loc =
      {Ppxlib_ast.Ast.loc_start =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_end =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_ghost = true}},
   None);
 ppat_loc =
  {Ppxlib_ast.Ast.loc_start =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_end =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_ghost = true};
 ppat_loc_stack = []; ppat_attributes = []}
|}]

let _ = [%type: unit]
[%%expect{|
- : Ppxlib.core_type =
{Ppxlib_ast.Ast.ptyp_desc =
  Ppxlib_ast.Ast.Ptyp_constr
   ({Ppxlib_ast.Ast.txt = Ppxlib_ast.Ast.Lident "unit";
     loc =
      {Ppxlib_ast.Ast.loc_start =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_end =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_ghost = true}},
   []);
 ptyp_loc =
  {Ppxlib_ast.Ast.loc_start =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_end =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_ghost = true};
 ptyp_loc_stack = []; ptyp_attributes = []}
|}]

let _ = [%stri let _ = ()]
[%%expect{|
- : Ppxlib.structure_item =
{Ppxlib_ast.Ast.pstr_desc =
  Ppxlib_ast.Ast.Pstr_value (Ppxlib_ast.Ast.Nonrecursive,
   [{Ppxlib_ast.Ast.pvb_pat =
      {Ppxlib_ast.Ast.ppat_desc = Ppxlib_ast.Ast.Ppat_any;
       ppat_loc =
        {Ppxlib_ast.Ast.loc_start =
          {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true};
       ppat_loc_stack = []; ppat_attributes = []};
     pvb_expr =
      {Ppxlib_ast.Ast.pexp_desc =
        Ppxlib_ast.Ast.Pexp_construct
         ({Ppxlib_ast.Ast.txt = Ppxlib_ast.Ast.Lident "()";
           loc =
            {Ppxlib_ast.Ast.loc_start =
              {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1;
               pos_bol = 0; pos_cnum = -1};
             loc_end =
              {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1;
               pos_bol = 0; pos_cnum = -1};
             loc_ghost = true}},
         None);
       pexp_loc =
        {Ppxlib_ast.Ast.loc_start =
          {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true};
       pexp_loc_stack = []; pexp_attributes = []};
     pvb_attributes = [];
     pvb_loc =
      {Ppxlib_ast.Ast.loc_start =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_end =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_ghost = true}}]);
 pstr_loc =
  {Ppxlib_ast.Ast.loc_start =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_end =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_ghost = true}}
|}]

let _ = [%sigi: include S]
[%%expect{|
- : Ppxlib.signature_item =
{Ppxlib_ast.Ast.psig_desc =
  Ppxlib_ast.Ast.Psig_include
   {Ppxlib_ast.Ast.pincl_mod =
     {Ppxlib_ast.Ast.pmty_desc =
       Ppxlib_ast.Ast.Pmty_ident
        {Ppxlib_ast.Ast.txt = Ppxlib_ast.Ast.Lident "S";
         loc =
          {Ppxlib_ast.Ast.loc_start =
            {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true}};
      pmty_loc =
       {Ppxlib_ast.Ast.loc_start =
         {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true};
      pmty_attributes = []};
    pincl_loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true};
    pincl_attributes = []};
 psig_loc =
  {Ppxlib_ast.Ast.loc_start =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_end =
    {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
     pos_cnum = -1};
   loc_ghost = true}}
|}]

let _ = [%str let _ = ()]
[%%expect{|
- : Ppxlib_ast.Ast.structure =
[{Ppxlib_ast.Ast.pstr_desc =
   Ppxlib_ast.Ast.Pstr_value (Ppxlib_ast.Ast.Nonrecursive,
    [{Ppxlib_ast.Ast.pvb_pat =
       {Ppxlib_ast.Ast.ppat_desc = Ppxlib_ast.Ast.Ppat_any;
        ppat_loc =
         {Ppxlib_ast.Ast.loc_start =
           {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true};
        ppat_loc_stack = []; ppat_attributes = []};
      pvb_expr =
       {Ppxlib_ast.Ast.pexp_desc =
         Ppxlib_ast.Ast.Pexp_construct
          ({Ppxlib_ast.Ast.txt = Ppxlib_ast.Ast.Lident "()";
            loc =
             {Ppxlib_ast.Ast.loc_start =
               {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}},
          None);
        pexp_loc =
         {Ppxlib_ast.Ast.loc_start =
           {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true};
        pexp_loc_stack = []; pexp_attributes = []};
      pvb_attributes = [];
      pvb_loc =
       {Ppxlib_ast.Ast.loc_start =
         {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}}]);
  pstr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}}]
|}]

let _ = [%sig: include S]
[%%expect{|
- : Ppxlib_ast.Ast.signature =
[{Ppxlib_ast.Ast.psig_desc =
   Ppxlib_ast.Ast.Psig_include
    {Ppxlib_ast.Ast.pincl_mod =
      {Ppxlib_ast.Ast.pmty_desc =
        Ppxlib_ast.Ast.Pmty_ident
         {Ppxlib_ast.Ast.txt = Ppxlib_ast.Ast.Lident "S";
          loc =
           {Ppxlib_ast.Ast.loc_start =
             {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
              pos_cnum = -1};
            loc_end =
             {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
              pos_cnum = -1};
            loc_ghost = true}};
       pmty_loc =
        {Ppxlib_ast.Ast.loc_start =
          {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true};
       pmty_attributes = []};
     pincl_loc =
      {Ppxlib_ast.Ast.loc_start =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_end =
        {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
         pos_cnum = -1};
       loc_ghost = true};
     pincl_attributes = []};
  psig_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}}]
|}]

(* attributes *)

let _ =
  let e = [%expr (() [@attr1])] in
  [%expr [%e e] [@attr2]].pexp_attributes
[%%expect{|
- : Ppxlib_ast.Ast.attributes =
[{Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr1";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}};
 {Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr2";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}}]
|}]

let _ =
  let p = [%pat? (() [@attr1])] in
  [%pat? [%p p] [@attr2]].ppat_attributes
[%%expect{|
- : Ppxlib_ast.Ast.attributes =
[{Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr1";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}};
 {Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr2";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}}]
|}]

let _ =
  let t = [%type: (unit [@attr1])] in
  [%type: [%t t] [@attr2]].ptyp_attributes
[%%expect{|
- : Ppxlib_ast.Ast.attributes =
[{Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr1";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}};
 {Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr2";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}}]
|}]

let _ =
  let extract_module_M m =
    match m with
    | [%stri module M = [%m? m]] -> m
    | _ -> assert false
  in
  let m = extract_module_M [%stri module M = (struct end [@attr1])] in
  (extract_module_M [%stri module M = [%m m] [@attr2]]).pmod_attributes
[%%expect{|
- : Ppxlib_ast.Ast.attributes =
[{Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr1";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}};
 {Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr2";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}}]
|}]

let _ =
  let extract_module_ty_S s =
    match s with
    | [%stri module type S = [%m? s]] -> s
    | _ -> assert false
  in
  let s = extract_module_ty_S [%stri module type S = (sig end [@attr1])] in
  (extract_module_ty_S [%stri module type S = [%m s] [@attr2]]).pmty_attributes
[%%expect{|
- : Ppxlib_ast.Ast.attributes =
[{Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr1";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}};
 {Ppxlib_ast.Ast.attr_name =
   {Ppxlib_ast.Ast.txt = "attr2";
    loc =
     {Ppxlib_ast.Ast.loc_start =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_end =
       {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
        pos_cnum = -1};
      loc_ghost = true}};
  attr_payload = Ppxlib_ast.Ast.PStr [];
  attr_loc =
   {Ppxlib_ast.Ast.loc_start =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_end =
     {Ppxlib_ast.Ast.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
      pos_cnum = -1};
    loc_ghost = true}}]
|}]

(* mistyped escapes (not producing ASTs at all) *)

let _ = [%expr [%e ()]]
[%%expect{|
Line _, characters 19-21:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.expression
|}]

let _ = [%pat? [%p ()]]
[%%expect{|
Line _, characters 19-21:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.pattern
|}]

let _ = [%type: [%t ()]]
[%%expect{|
Line _, characters 20-22:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.core_type
|}]

let _ = [%stri [%%i ()]]
[%%expect{|
Line _, characters 20-22:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.structure_item
|}]

let _ = [%sigi: [%%i ()]]
[%%expect{|
Line _, characters 21-23:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.signature_item
|}]

(* mistyped escapes (not producing ASTs at all) with attributes *)

let _ = [%expr [%e ()] [@attr]]
[%%expect{|
Line _, characters 19-21:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.expression
|}]

let _ = [%pat? [%p ()] [@attr]]
[%%expect{|
Line _, characters 19-21:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.pattern
|}]

let _ = [%type: [%t ()] [@attr]]
[%%expect{|
Line _, characters 20-22:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.core_type
|}]

let _ = [%stri module M = [%m ()] [@attr]]
[%%expect{|
Line _, characters 30-32:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.module_expr
|}]

let _ = [%sigi: module type M = [%m ()] [@attr]]
[%%expect{|
Line _, characters 36-38:
Error: This expression should not be a unit literal, the expected type is
       Ppxlib_ast.Ast.module_type
|}]
