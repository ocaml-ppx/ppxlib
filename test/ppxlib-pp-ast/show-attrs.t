ppxlib-pp-ast as a --show-attrs flag that controls whether attributes are shown

Consider the following .ml file:

  $ cat > test.ml << EOF
  > let x = 2 + (2[@foo 1])
  > [@@bar: int * string]
  > EOF

And how it's printed without the flag:

  $ ppxlib-pp-ast test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr =
              Pexp_apply
                ( Pexp_ident (Lident "+")
                , [ ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
                  ; ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
                  ]
                )
          ; pvb_attributes = __attrs
          ; pvb_loc = __loc
          }
        ]
      )
  ]

And with the flag:

  $ ppxlib-pp-ast --show-attrs test.ml
  [ Pstr_value
      ( Nonrecursive
      , [ { pvb_pat = Ppat_var "x"
          ; pvb_expr =
              Pexp_apply
                ( Pexp_ident (Lident "+")
                , [ ( Nolabel, Pexp_constant (Pconst_integer ( "2", None)))
                  ; ( Nolabel
                    , { pexp_desc = Pexp_constant (Pconst_integer ( "2", None))
                      ; pexp_loc = __loc
                      ; pexp_loc_stack = __lstack
                      ; pexp_attributes =
                          [ { attr_name = "foo"
                            ; attr_payload =
                                PStr
                                  [ Pstr_eval
                                      ( Pexp_constant
                                          (Pconst_integer ( "1", None))
                                      , []
                                      )
                                  ]
                            ; attr_loc = __loc
                            }
                          ]
                      }
                    )
                  ]
                )
          ; pvb_attributes =
              [ { attr_name = "bar"
                ; attr_payload =
                    PTyp
                      (Ptyp_tuple
                         [ Ptyp_constr ( Lident "int", [])
                         ; Ptyp_constr ( Lident "string", [])
                         ])
                ; attr_loc = __loc
                }
              ]
          ; pvb_loc = __loc
          }
        ]
      )
  ]
