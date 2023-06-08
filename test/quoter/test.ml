open Ppxlib
open Expansion_helpers

module Ast = Ast_builder.Default
[%%expect{|
module Ast = Ppxlib.Ast_builder.Default
|}]

let quoter = Quoter.create ();;
[%%expect{|
val quoter : Quoter.t = <abstr>
|}]

let expr1 =
  Ast.evar "foo" ~loc:Location.none
  |> Quoter.quote quoter
[%%expect{|
val expr1 : expression =
  {Ppxlib__.Import.pexp_desc =
    Ppxlib__.Import.Pexp_ident
     {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "__0";
      loc =
       {Ppxlib__.Import.loc_start =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Ppxlib__.Import.loc_start =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_end =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_ghost = true};
   pexp_loc_stack = []; pexp_attributes = []}
|}]

Pprintast.string_of_expression expr1;;
[%%expect{|
- : string = "__0"
|}]

let expr2 =
  Ast_builder.Default.evar ~loc:Location.none "bar"
  |> Quoter.quote quoter
[%%expect{|
val expr2 : expression =
  {Ppxlib__.Import.pexp_desc =
    Ppxlib__.Import.Pexp_ident
     {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "__1";
      loc =
       {Ppxlib__.Import.loc_start =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_end =
         {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
          pos_cnum = -1};
        loc_ghost = true}};
   pexp_loc =
    {Ppxlib__.Import.loc_start =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_end =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_ghost = true};
   pexp_loc_stack = []; pexp_attributes = []}
|}]

let expr3 =
  Ast.eapply ~loc:Location.none (Ast.evar "foo" ~loc:Location.none) [Ast.eunit ~loc:Location.none]
  |> Quoter.quote quoter
[%%expect{|
val expr3 : expression =
  {Ppxlib__.Import.pexp_desc =
    Ppxlib__.Import.Pexp_apply
     ({Ppxlib__.Import.pexp_desc =
        Ppxlib__.Import.Pexp_ident
         {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "__2";
          loc =
           {Ppxlib__.Import.loc_start =
             {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
              pos_bol = 0; pos_cnum = -1};
            loc_end =
             {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
              pos_bol = 0; pos_cnum = -1};
            loc_ghost = true}};
       pexp_loc =
        {Ppxlib__.Import.loc_start =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true};
       pexp_loc_stack = []; pexp_attributes = []},
     [(Ppxlib__.Import.Nolabel,
       {Ppxlib__.Import.pexp_desc =
         Ppxlib__.Import.Pexp_construct
          ({Ppxlib__.Import.txt = Ppxlib__.Import.Lident "()";
            loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}},
          None);
        pexp_loc =
         {Ppxlib__.Import.loc_start =
           {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
            pos_cnum = -1};
          loc_end =
           {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
            pos_cnum = -1};
          loc_ghost = true};
        pexp_loc_stack = []; pexp_attributes = []})]);
   pexp_loc =
    {Ppxlib__.Import.loc_start =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_end =
      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
       pos_cnum = -1};
     loc_ghost = true};
   pexp_loc_stack = []; pexp_attributes = []}
|}]

Pprintast.string_of_expression expr3;;
[%%expect{|
- : string = "__2 ()"
|}]

let quoted =
  let expr = Ast.elist ~loc:Location.none [expr1; expr2; expr3] in
  Quoter.sanitize quoter expr
[%%expect{|
val quoted : expression =
  {Ppxlib__.Import.pexp_desc =
    Ppxlib__.Import.Pexp_let (Ppxlib__.Import.Nonrecursive,
     [{Ppxlib__.Import.pvb_pat =
        {Ppxlib__.Import.ppat_desc =
          Ppxlib__.Import.Ppat_var
           {Ppxlib__.Import.txt = "__2";
            loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_loc_stack = []; ppat_attributes = []};
       pvb_expr =
        {Ppxlib__.Import.pexp_desc =
          Ppxlib__.Import.Pexp_fun (Ppxlib__.Import.Nolabel, None,
           {Ppxlib__.Import.ppat_desc =
             Ppxlib__.Import.Ppat_construct
              ({Ppxlib__.Import.txt = Ppxlib__.Import.Lident "()";
                loc =
                 {Ppxlib__.Import.loc_start =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_end =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_ghost = true}},
              None);
            ppat_loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true};
            ppat_loc_stack = []; ppat_attributes = []},
           {Ppxlib__.Import.pexp_desc =
             Ppxlib__.Import.Pexp_apply
              ({Ppxlib__.Import.pexp_desc =
                 Ppxlib__.Import.Pexp_ident
                  {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "foo";
                   loc =
                    {Ppxlib__.Import.loc_start =
                      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                       pos_bol = 0; pos_cnum = -1};
                     loc_end =
                      {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                       pos_bol = 0; pos_cnum = -1};
                     loc_ghost = true}};
                pexp_loc =
                 {Ppxlib__.Import.loc_start =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_end =
                   {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                    pos_bol = 0; pos_cnum = -1};
                  loc_ghost = true};
                pexp_loc_stack = []; pexp_attributes = []},
              [(Ppxlib__.Import.Nolabel,
                {Ppxlib__.Import.pexp_desc =
                  Ppxlib__.Import.Pexp_construct
                   ({Ppxlib__.Import.txt = Ppxlib__.Import.Lident "()";
                     loc =
                      {Ppxlib__.Import.loc_start =
                        {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                         pos_bol = 0; pos_cnum = -1};
                       loc_end =
                        {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                         pos_bol = 0; pos_cnum = -1};
                       loc_ghost = true}},
                   None);
                 pexp_loc =
                  {Ppxlib__.Import.loc_start =
                    {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                     pos_bol = 0; pos_cnum = -1};
                   loc_end =
                    {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                     pos_bol = 0; pos_cnum = -1};
                   loc_ghost = true};
                 pexp_loc_stack = []; pexp_attributes = []})]);
            pexp_loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true};
            pexp_loc_stack = []; pexp_attributes = []});
         pexp_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_loc_stack = []; pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Ppxlib__.Import.loc_start =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}};
      {Ppxlib__.Import.pvb_pat =
        {Ppxlib__.Import.ppat_desc =
          Ppxlib__.Import.Ppat_var
           {Ppxlib__.Import.txt = "__1";
            loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         ppat_loc_stack = []; ppat_attributes = []};
       pvb_expr =
        {Ppxlib__.Import.pexp_desc =
          Ppxlib__.Import.Pexp_ident
           {Ppxlib__.Import.txt = Ppxlib__.Import.Lident "bar";
            loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}};
         pexp_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_ghost = true};
         pexp_loc_stack = []; pexp_attributes = []};
       pvb_attributes = [];
       pvb_loc =
        {Ppxlib__.Import.loc_start =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_end =
          {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
           pos_cnum = -1};
         loc_ghost = true}};
      {Ppxlib__.Import.pvb_pat =
        {Ppxlib__.Import.ppat_desc =
          Ppxlib__.Import.Ppat_var
           {Ppxlib__.Import.txt = "__0";
            loc =
             {Ppxlib__.Import.loc_start =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_end =
               {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1;
                pos_bol = 0; pos_cnum = -1};
              loc_ghost = true}};
         ppat_loc =
          {Ppxlib__.Import.loc_start =
            {Ppxlib__.Import.pos_fname = "_none_"; pos_lnum = 1; pos_bol = 0;
             pos_cnum = -1};
           loc_end = ...; loc_ghost = ...};
         ppat_loc_stack = ...; ppat_attributes = ...};
       pvb_expr = ...; pvb_attributes = ...; pvb_loc = ...};
      ...],
     ...);
   pexp_loc = ...; pexp_loc_stack = ...; pexp_attributes = ...}
|}]

Pprintast.string_of_expression quoted;;
[%%expect{|
- : string =
"let __2 () = foo ()\nand __1 = bar\nand __0 = foo in [__0; __1; __2 ()]"
|}]
