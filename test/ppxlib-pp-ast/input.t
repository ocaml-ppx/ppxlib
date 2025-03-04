ppxlib-pp-ast can be used on files but it can also read from stdin:

  $ cat > test.ml << EOF
  > let x = x + 2
  > EOF
  $ cat test.ml | ppxlib-pp-ast --str -
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr =
              Pexp_apply
                ( Pexp_ident (Lident "+")
                , [ ( Nolabel, Pexp_ident (Lident "x"))
                  ; ( Nolabel
                    , Pexp_constant
                        { pconst_desc = Pconst_integer ( "2", None)
                        ; pconst_loc = __loc
                        }
                    )
                  ]
                )
          ; pvb_constraint = None
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ]

It can also read the input directly from the command line:

  $ ppxlib-pp-ast --exp "x + 2"
  Pexp_apply
    ( Pexp_ident (Lident "+")
    , [ ( Nolabel, Pexp_ident (Lident "x"))
      ; ( Nolabel
        , Pexp_constant
            { pconst_desc = Pconst_integer ( "2", None); pconst_loc = __loc}
        )
      ]
    )

Note that the kind must be specified when the input is not a file:

  $ ppxlib-pp-ast "x + 2"
  ppxlib-pp-ast: Could not guess kind from input "x + 2". Please use relevant CLI flag.
  [123]
