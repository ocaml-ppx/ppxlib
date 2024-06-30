This test is enabled both on 5.0.0 and 5.1.0. The test makes sense for as long
as the ppxlib AST is either 5.0.0 or 5.1.0. While the ppxlib AST is on 5.0.0, the
test checks whether parsing on 5.0.0 (result of test running on 5.0.0) is the same as
parsing on 5.1.0 and then migrating down to 5.0.0 (result of test running on 5.1.0).

The test is mostly useful for debuggung problems in a full round-trip. Since Ppxlib's
`dparsetree` option doesn't compactify or strip locations, its output is very long.
So let's only keep one example.

  $ echo "let make ~foo ~bar = foo ^ bar" > file.ml
  $ ./identity_driver.exe -dparsetree file.ml
  (((pstr_desc
     (Pstr_attribute
      ((attr_name
        ((txt ocaml.ppx.context)
         (loc
          ((loc_start
            ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
           (loc_end
            ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
           (loc_ghost true)))))
       (attr_payload
        (PStr
         (((pstr_desc
            (Pstr_eval
             ((pexp_desc
               (Pexp_record
                ((((txt (Lident tool_name))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_constant
                     (Pconst_string ppxlib_driver
                      ((loc_start
                        ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                         (pos_cnum -1)))
                       (loc_end
                        ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                         (pos_cnum -1)))
                       (loc_ghost true))
                      ())))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident include_dirs))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident hidden_include_dirs))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident load_path))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ())
                   (pexp_attributes
                    (((attr_name
                       ((txt ppxlib.migration.load_path)
                        (loc
                         ((loc_start
                           ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                            (pos_cnum -1)))
                          (loc_end
                           ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                            (pos_cnum -1)))
                          (loc_ghost true)))))
                      (attr_payload
                       (PStr
                        (((pstr_desc
                           (Pstr_eval
                            ((pexp_desc
                              (Pexp_tuple
                               (((pexp_desc
                                  (Pexp_construct
                                   ((txt (Lident []))
                                    (loc
                                     ((loc_start
                                       ((pos_fname _none_) (pos_lnum 0)
                                        (pos_bol 0) (pos_cnum -1)))
                                      (loc_end
                                       ((pos_fname _none_) (pos_lnum 0)
                                        (pos_bol 0) (pos_cnum -1)))
                                      (loc_ghost true))))
                                   ()))
                                 (pexp_loc
                                  ((loc_start
                                    ((pos_fname _none_) (pos_lnum 0)
                                     (pos_bol 0) (pos_cnum -1)))
                                   (loc_end
                                    ((pos_fname _none_) (pos_lnum 0)
                                     (pos_bol 0) (pos_cnum -1)))
                                   (loc_ghost true)))
                                 (pexp_loc_stack ()) (pexp_attributes ()))
                                ((pexp_desc
                                  (Pexp_construct
                                   ((txt (Lident []))
                                    (loc
                                     ((loc_start
                                       ((pos_fname _none_) (pos_lnum 0)
                                        (pos_bol 0) (pos_cnum -1)))
                                      (loc_end
                                       ((pos_fname _none_) (pos_lnum 0)
                                        (pos_bol 0) (pos_cnum -1)))
                                      (loc_ghost true))))
                                   ()))
                                 (pexp_loc
                                  ((loc_start
                                    ((pos_fname _none_) (pos_lnum 0)
                                     (pos_bol 0) (pos_cnum -1)))
                                   (loc_end
                                    ((pos_fname _none_) (pos_lnum 0)
                                     (pos_bol 0) (pos_cnum -1)))
                                   (loc_ghost true)))
                                 (pexp_loc_stack ()) (pexp_attributes ())))))
                             (pexp_loc
                              ((loc_start
                                ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                                 (pos_cnum -1)))
                               (loc_end
                                ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                                 (pos_cnum -1)))
                               (loc_ghost true)))
                             (pexp_loc_stack ()) (pexp_attributes ()))
                            ()))
                          (pstr_loc
                           ((loc_start
                             ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                              (pos_cnum -1)))
                            (loc_end
                             ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                              (pos_cnum -1)))
                            (loc_ghost true)))))))
                      (attr_loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))))))
                 (((txt (Lident open_modules))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident for_package))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident None))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident debug))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident use_threads))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident use_vmthreads))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident recursive_types))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident principal))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident transparent_modules))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident unboxed_types))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident unsafe_string))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ())))
                 (((txt (Lident cookies))
                   (loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true))))
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident []))
                      (loc
                       ((loc_start
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_end
                         ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                          (pos_cnum -1)))
                        (loc_ghost true))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_end
                      ((pos_fname _none_) (pos_lnum 0) (pos_bol 0)
                       (pos_cnum -1)))
                     (loc_ghost true)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                ()))
              (pexp_loc
               ((loc_start
                 ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
                (loc_end
                 ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
                (loc_ghost true)))
              (pexp_loc_stack ()) (pexp_attributes ()))
             ()))
           (pstr_loc
            ((loc_start
              ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
             (loc_end
              ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
             (loc_ghost true)))))))
       (attr_loc
        ((loc_start
          ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
         (loc_end ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
         (loc_ghost true))))))
    (pstr_loc
     ((loc_start ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
      (loc_end ((pos_fname _none_) (pos_lnum 0) (pos_bol 0) (pos_cnum -1)))
      (loc_ghost true))))
   ((pstr_desc
     (Pstr_value Nonrecursive
      (((pvb_pat
         ((ppat_desc
           (Ppat_var
            ((txt make)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_ghost false))))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
            (loc_ghost false)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_fun (Labelled foo) ()
            ((ppat_desc
              (Ppat_var
               ((txt foo)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((pexp_desc
              (Pexp_fun (Labelled bar) ()
               ((ppat_desc
                 (Ppat_var
                  ((txt bar)
                   (loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 15)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 18)))
                     (loc_ghost false))))))
                (ppat_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
                  (loc_ghost false)))
                (ppat_loc_stack ()) (ppat_attributes ()))
               ((pexp_desc
                 (Pexp_apply
                  ((pexp_desc
                    (Pexp_ident
                     ((txt (Lident ^))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 25)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 26)))
                        (loc_ghost false))))))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 25)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 26)))
                     (loc_ghost false)))
                   (pexp_loc_stack ()) (pexp_attributes ()))
                  ((Nolabel
                    ((pexp_desc
                      (Pexp_ident
                       ((txt (Lident foo))
                        (loc
                         ((loc_start
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 21)))
                          (loc_end
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 24)))
                          (loc_ghost false))))))
                     (pexp_loc
                      ((loc_start
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 21)))
                       (loc_end
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 24)))
                       (loc_ghost false)))
                     (pexp_loc_stack ()) (pexp_attributes ())))
                   (Nolabel
                    ((pexp_desc
                      (Pexp_ident
                       ((txt (Lident bar))
                        (loc
                         ((loc_start
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 27)))
                          (loc_end
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 30)))
                          (loc_ghost false))))))
                     (pexp_loc
                      ((loc_start
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 27)))
                       (loc_end
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 30)))
                       (loc_ghost false)))
                     (pexp_loc_stack ()) (pexp_attributes ()))))))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 21)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 30)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 30)))
            (loc_ghost true)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 30)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 30)))
      (loc_ghost false)))))

