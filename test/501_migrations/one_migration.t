This test is enabled both on 5.0.0 and 5.1.0. The test makes sense for as long
as the ppxlib AST is either 5.0.0 or 5.1.0. While the ppxlib AST is on 5.0.0, the
test checks whether parsing on 5.0.0 (result of test running on 5.0.0) is the same as
parsing on 5.1.0 and then migrating down to 5.0.0 (result of test running on 5.1.0).

  $ echo "let x : int = 5" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt x)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly ()
               ((ptyp_desc
                 (Ptyp_constr
                  ((txt (Lident int))
                   (loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 8)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 11)))
                     (loc_ghost false))))
                  ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_constraint
            ((pexp_desc (Pexp_constant (Pconst_integer 5 ())))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))
            ((ptyp_desc
              (Ptyp_constr
               ((txt (Lident int))
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
                  (loc_ghost false))))
               ()))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 11)))
               (loc_ghost false)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
      (loc_ghost false)))))

  $ echo "let f : type a b c. a -> b -> c = fun x y -> assert false" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt f)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly
               (((txt a)
                 (loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 13)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 14)))
                   (loc_ghost false))))
                ((txt b)
                 (loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 15)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 16)))
                   (loc_ghost false))))
                ((txt c)
                 (loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 17)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 18)))
                   (loc_ghost false)))))
               ((ptyp_desc
                 (Ptyp_arrow Nolabel
                  ((ptyp_desc (Ptyp_var a))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 20)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 21)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))
                  ((ptyp_desc
                    (Ptyp_arrow Nolabel
                     ((ptyp_desc (Ptyp_var b))
                      (ptyp_loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 25)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 26)))
                        (loc_ghost false)))
                      (ptyp_loc_stack ()) (ptyp_attributes ()))
                     ((ptyp_desc (Ptyp_var c))
                      (ptyp_loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 30)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 31)))
                        (loc_ghost false)))
                      (ptyp_loc_stack ()) (ptyp_attributes ()))))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 25)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 31)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 20)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 31)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 57)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 31)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_newtype
            ((txt a)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
               (loc_ghost false))))
            ((pexp_desc
              (Pexp_newtype
               ((txt b)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
                  (loc_ghost false))))
               ((pexp_desc
                 (Pexp_newtype
                  ((txt c)
                   (loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 17)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 18)))
                     (loc_ghost false))))
                  ((pexp_desc
                    (Pexp_constraint
                     ((pexp_desc
                       (Pexp_fun Nolabel ()
                        ((ppat_desc
                          (Ppat_var
                           ((txt x)
                            (loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 38)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 39)))
                              (loc_ghost false))))))
                         (ppat_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 38)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 39)))
                           (loc_ghost false)))
                         (ppat_loc_stack ()) (ppat_attributes ()))
                        ((pexp_desc
                          (Pexp_fun Nolabel ()
                           ((ppat_desc
                             (Ppat_var
                              ((txt y)
                               (loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 40)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 41)))
                                 (loc_ghost false))))))
                            (ppat_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 40)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 41)))
                              (loc_ghost false)))
                            (ppat_loc_stack ()) (ppat_attributes ()))
                           ((pexp_desc
                             (Pexp_assert
                              ((pexp_desc
                                (Pexp_construct
                                 ((txt (Lident false))
                                  (loc
                                   ((loc_start
                                     ((pos_fname file.ml) (pos_lnum 1)
                                      (pos_bol 0) (pos_cnum 52)))
                                    (loc_end
                                     ((pos_fname file.ml) (pos_lnum 1)
                                      (pos_bol 0) (pos_cnum 57)))
                                    (loc_ghost false))))
                                 ()))
                               (pexp_loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 52)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 57)))
                                 (loc_ghost false)))
                               (pexp_loc_stack ()) (pexp_attributes ()))))
                            (pexp_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 45)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 57)))
                              (loc_ghost false)))
                            (pexp_loc_stack ()) (pexp_attributes ()))))
                         (pexp_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 40)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 57)))
                           (loc_ghost true)))
                         (pexp_loc_stack ()) (pexp_attributes ()))))
                      (pexp_loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 34)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 57)))
                        (loc_ghost false)))
                      (pexp_loc_stack ()) (pexp_attributes ()))
                     ((ptyp_desc
                       (Ptyp_arrow Nolabel
                        ((ptyp_desc
                          (Ptyp_constr
                           ((txt (Lident a))
                            (loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 20)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 21)))
                              (loc_ghost false))))
                           ()))
                         (ptyp_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 20)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 21)))
                           (loc_ghost false)))
                         (ptyp_loc_stack ()) (ptyp_attributes ()))
                        ((ptyp_desc
                          (Ptyp_arrow Nolabel
                           ((ptyp_desc
                             (Ptyp_constr
                              ((txt (Lident b))
                               (loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 25)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 26)))
                                 (loc_ghost false))))
                              ()))
                            (ptyp_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 25)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 26)))
                              (loc_ghost false)))
                            (ptyp_loc_stack ()) (ptyp_attributes ()))
                           ((ptyp_desc
                             (Ptyp_constr
                              ((txt (Lident c))
                               (loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 30)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 31)))
                                 (loc_ghost false))))
                              ()))
                            (ptyp_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 30)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 31)))
                              (loc_ghost false)))
                            (ptyp_loc_stack ()) (ptyp_attributes ()))))
                         (ptyp_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 25)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 31)))
                           (loc_ghost false)))
                         (ptyp_loc_stack ()) (ptyp_attributes ()))))
                      (ptyp_loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 20)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 31)))
                        (loc_ghost false)))
                      (ptyp_loc_stack ()) (ptyp_attributes ()))))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 4)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 57)))
                     (loc_ghost false)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 57)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 57)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 57)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 57)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 57)))
      (loc_ghost false)))))

  $ echo "let f = (fun (type a) (type b) (type c) -> (fun x y -> assert false : a -> b -> c))" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
            ((txt f)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false))))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
            (loc_ghost false)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_newtype
            ((txt a)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 19)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 20)))
               (loc_ghost false))))
            ((pexp_desc
              (Pexp_newtype
               ((txt b)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 29)))
                  (loc_ghost false))))
               ((pexp_desc
                 (Pexp_newtype
                  ((txt c)
                   (loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 37)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 38)))
                     (loc_ghost false))))
                  ((pexp_desc
                    (Pexp_constraint
                     ((pexp_desc
                       (Pexp_fun Nolabel ()
                        ((ppat_desc
                          (Ppat_var
                           ((txt x)
                            (loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 48)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 49)))
                              (loc_ghost false))))))
                         (ppat_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 48)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 49)))
                           (loc_ghost false)))
                         (ppat_loc_stack ()) (ppat_attributes ()))
                        ((pexp_desc
                          (Pexp_fun Nolabel ()
                           ((ppat_desc
                             (Ppat_var
                              ((txt y)
                               (loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 50)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 51)))
                                 (loc_ghost false))))))
                            (ppat_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 50)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 51)))
                              (loc_ghost false)))
                            (ppat_loc_stack ()) (ppat_attributes ()))
                           ((pexp_desc
                             (Pexp_assert
                              ((pexp_desc
                                (Pexp_construct
                                 ((txt (Lident false))
                                  (loc
                                   ((loc_start
                                     ((pos_fname file.ml) (pos_lnum 1)
                                      (pos_bol 0) (pos_cnum 62)))
                                    (loc_end
                                     ((pos_fname file.ml) (pos_lnum 1)
                                      (pos_bol 0) (pos_cnum 67)))
                                    (loc_ghost false))))
                                 ()))
                               (pexp_loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 62)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 67)))
                                 (loc_ghost false)))
                               (pexp_loc_stack ()) (pexp_attributes ()))))
                            (pexp_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 55)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 67)))
                              (loc_ghost false)))
                            (pexp_loc_stack ()) (pexp_attributes ()))))
                         (pexp_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 50)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 67)))
                           (loc_ghost true)))
                         (pexp_loc_stack ()) (pexp_attributes ()))))
                      (pexp_loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 44)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 67)))
                        (loc_ghost false)))
                      (pexp_loc_stack ()) (pexp_attributes ()))
                     ((ptyp_desc
                       (Ptyp_arrow Nolabel
                        ((ptyp_desc
                          (Ptyp_constr
                           ((txt (Lident a))
                            (loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 70)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 71)))
                              (loc_ghost false))))
                           ()))
                         (ptyp_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 70)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 71)))
                           (loc_ghost false)))
                         (ptyp_loc_stack ()) (ptyp_attributes ()))
                        ((ptyp_desc
                          (Ptyp_arrow Nolabel
                           ((ptyp_desc
                             (Ptyp_constr
                              ((txt (Lident b))
                               (loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 75)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 76)))
                                 (loc_ghost false))))
                              ()))
                            (ptyp_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 75)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 76)))
                              (loc_ghost false)))
                            (ptyp_loc_stack ()) (ptyp_attributes ()))
                           ((ptyp_desc
                             (Ptyp_constr
                              ((txt (Lident c))
                               (loc
                                ((loc_start
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 80)))
                                 (loc_end
                                  ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                   (pos_cnum 81)))
                                 (loc_ghost false))))
                              ()))
                            (ptyp_loc
                             ((loc_start
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 80)))
                              (loc_end
                               ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                                (pos_cnum 81)))
                              (loc_ghost false)))
                            (ptyp_loc_stack ()) (ptyp_attributes ()))))
                         (ptyp_loc
                          ((loc_start
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 75)))
                           (loc_end
                            ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                             (pos_cnum 81)))
                           (loc_ghost false)))
                         (ptyp_loc_stack ()) (ptyp_attributes ()))))
                      (ptyp_loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 70)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 81)))
                        (loc_ghost false)))
                      (ptyp_loc_stack ()) (ptyp_attributes ()))))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 43)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 82)))
                     (loc_ghost false)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 31)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 82)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 22)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 82)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 83)))
            (loc_ghost false)))
          (pexp_loc_stack
           (((loc_start
              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
             (loc_end
              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 82)))
             (loc_ghost false))))
          (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 83)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 83)))
      (loc_ghost false)))))

  $ echo "let f : type a . a -> a = fun x -> x" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt f)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly
               (((txt a)
                 (loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 13)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 14)))
                   (loc_ghost false)))))
               ((ptyp_desc
                 (Ptyp_arrow Nolabel
                  ((ptyp_desc (Ptyp_var a))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 17)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 18)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))
                  ((ptyp_desc (Ptyp_var a))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 22)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 23)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 36)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_newtype
            ((txt a)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
               (loc_ghost false))))
            ((pexp_desc
              (Pexp_constraint
               ((pexp_desc
                 (Pexp_fun Nolabel ()
                  ((ppat_desc
                    (Ppat_var
                     ((txt x)
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 30)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 31)))
                        (loc_ghost false))))))
                   (ppat_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 30)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 31)))
                     (loc_ghost false)))
                   (ppat_loc_stack ()) (ppat_attributes ()))
                  ((pexp_desc
                    (Pexp_ident
                     ((txt (Lident x))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 35)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 36)))
                        (loc_ghost false))))))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 35)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 36)))
                     (loc_ghost false)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 26)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 36)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))
               ((ptyp_desc
                 (Ptyp_arrow Nolabel
                  ((ptyp_desc
                    (Ptyp_constr
                     ((txt (Lident a))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 17)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 18)))
                        (loc_ghost false))))
                     ()))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 17)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 18)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))
                  ((ptyp_desc
                    (Ptyp_constr
                     ((txt (Lident a))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 22)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 23)))
                        (loc_ghost false))))
                     ()))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 22)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 23)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 36)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 36)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 36)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 36)))
      (loc_ghost false)))))

  $ echo "let (x, y) : (int * int) = assert false" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_tuple
               (((ppat_desc
                  (Ppat_var
                   ((txt x)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 5)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 6)))
                      (loc_ghost false))))))
                 (ppat_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 6)))
                   (loc_ghost false)))
                 (ppat_loc_stack ()) (ppat_attributes ()))
                ((ppat_desc
                  (Ppat_var
                   ((txt y)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 8)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 9)))
                      (loc_ghost false))))))
                 (ppat_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                   (loc_ghost false)))
                 (ppat_loc_stack ()) (ppat_attributes ())))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 10)))
               (loc_ghost false)))
             (ppat_loc_stack
              (((loc_start
                 ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                (loc_end
                 ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                (loc_ghost false))))
             (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_tuple
               (((ptyp_desc
                  (Ptyp_constr
                   ((txt (Lident int))
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 14)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 17)))
                      (loc_ghost false))))
                   ()))
                 (ptyp_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 14)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 17)))
                   (loc_ghost false)))
                 (ptyp_loc_stack ()) (ptyp_attributes ()))
                ((ptyp_desc
                  (Ptyp_constr
                   ((txt (Lident int))
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 20)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 23)))
                      (loc_ghost false))))
                   ()))
                 (ptyp_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 20)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 23)))
                   (loc_ghost false)))
                 (ptyp_loc_stack ()) (ptyp_attributes ())))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
               (loc_ghost false)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 24)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_assert
            ((pexp_desc
              (Pexp_construct
               ((txt (Lident false))
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 34)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 39)))
                  (loc_ghost false))))
               ()))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 34)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 39)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 27)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 39)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 39)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 39)))
      (loc_ghost false)))))

  $ echo "let f: type a. a option -> _ = assert false" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt f)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly
               (((txt a)
                 (loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 12)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 13)))
                   (loc_ghost false)))))
               ((ptyp_desc
                 (Ptyp_arrow Nolabel
                  ((ptyp_desc
                    (Ptyp_constr
                     ((txt (Lident option))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 17)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 23)))
                        (loc_ghost false))))
                     (((ptyp_desc (Ptyp_var a))
                       (ptyp_loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 15)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 16)))
                         (loc_ghost false)))
                       (ptyp_loc_stack ()) (ptyp_attributes ())))))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 15)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 23)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))
                  ((ptyp_desc Ptyp_any)
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 27)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 28)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 43)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_newtype
            ((txt a)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 12)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
               (loc_ghost false))))
            ((pexp_desc
              (Pexp_constraint
               ((pexp_desc
                 (Pexp_assert
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 38)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 43)))
                        (loc_ghost false))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 38)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 43)))
                     (loc_ghost false)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 31)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 43)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))
               ((ptyp_desc
                 (Ptyp_arrow Nolabel
                  ((ptyp_desc
                    (Ptyp_constr
                     ((txt (Lident option))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 17)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 23)))
                        (loc_ghost false))))
                     (((ptyp_desc
                        (Ptyp_constr
                         ((txt (Lident a))
                          (loc
                           ((loc_start
                             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                              (pos_cnum 15)))
                            (loc_end
                             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                              (pos_cnum 16)))
                            (loc_ghost false))))
                         ()))
                       (ptyp_loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 15)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 16)))
                         (loc_ghost false)))
                       (ptyp_loc_stack ()) (ptyp_attributes ())))))
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 15)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 23)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))
                  ((ptyp_desc Ptyp_any)
                   (ptyp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 27)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 28)))
                     (loc_ghost false)))
                   (ptyp_loc_stack ()) (ptyp_attributes ()))))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 43)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 43)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 43)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 43)))
      (loc_ghost false)))))

  $ echo "let f : 'a . 'a = (fun (type a) -> (assert false : a))" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt f)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly
               (((txt a)
                 (loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 10)))
                   (loc_ghost false)))))
               ((ptyp_desc (Ptyp_var a))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 15)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_newtype
            ((txt a)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 29)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 30)))
               (loc_ghost false))))
            ((pexp_desc
              (Pexp_constraint
               ((pexp_desc
                 (Pexp_assert
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 43)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 48)))
                        (loc_ghost false))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 43)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 48)))
                     (loc_ghost false)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 36)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 48)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))
               ((ptyp_desc
                 (Ptyp_constr
                  ((txt (Lident a))
                   (loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 51)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 52)))
                     (loc_ghost false))))
                  ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 51)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 52)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 35)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 53)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 54)))
            (loc_ghost false)))
          (pexp_loc_stack
           (((loc_start
              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 19)))
             (loc_end
              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 53)))
             (loc_ghost false))))
          (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 54)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 54)))
      (loc_ghost false)))))

  $ echo "let f : type a . a = assert false" > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt f)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly
               (((txt a)
                 (loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 13)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 14)))
                   (loc_ghost false)))))
               ((ptyp_desc (Ptyp_var a))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 33)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_newtype
            ((txt a)
             (loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 13)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 14)))
               (loc_ghost false))))
            ((pexp_desc
              (Pexp_constraint
               ((pexp_desc
                 (Pexp_assert
                  ((pexp_desc
                    (Pexp_construct
                     ((txt (Lident false))
                      (loc
                       ((loc_start
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 28)))
                        (loc_end
                         ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                          (pos_cnum 33)))
                        (loc_ghost false))))
                     ()))
                   (pexp_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 28)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 33)))
                     (loc_ghost false)))
                   (pexp_loc_stack ()) (pexp_attributes ()))))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 21)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 33)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))
               ((ptyp_desc
                 (Ptyp_constr
                  ((txt (Lident a))
                   (loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 17)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 18)))
                     (loc_ghost false))))
                  ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 33)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 33)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 33)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 33)))
      (loc_ghost false)))))

  $ echo 'let x :> [`A | `B] = `A' > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt x)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly ()
               ((ptyp_desc
                 (Ptyp_variant
                  (((prf_desc
                     (Rtag
                      ((txt A)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 10)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 12)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 10)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 12)))
                      (loc_ghost false)))
                    (prf_attributes ()))
                   ((prf_desc
                     (Rtag
                      ((txt B)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 15)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 17)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 15)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 17)))
                      (loc_ghost false)))
                    (prf_attributes ())))
                  Closed ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_coerce
            ((pexp_desc (Pexp_variant A ()))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 21)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))
            ()
            ((ptyp_desc
              (Ptyp_variant
               (((prf_desc
                  (Rtag
                   ((txt A)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 10)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 12)))
                      (loc_ghost false))))
                   true ()))
                 (prf_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 10)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 12)))
                   (loc_ghost false)))
                 (prf_attributes ()))
                ((prf_desc
                  (Rtag
                   ((txt B)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 15)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 17)))
                      (loc_ghost false))))
                   true ()))
                 (prf_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 15)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 17)))
                   (loc_ghost false)))
                 (prf_attributes ())))
               Closed ()))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 18)))
               (loc_ghost false)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
      (loc_ghost false)))))

  $ echo 'let x : [`A] :> [`A | `B] = `A' > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt x)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly ()
               ((ptyp_desc
                 (Ptyp_variant
                  (((prf_desc
                     (Rtag
                      ((txt A)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 17)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 19)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 17)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 19)))
                      (loc_ghost false)))
                    (prf_attributes ()))
                   ((prf_desc
                     (Rtag
                      ((txt B)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 22)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 24)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 22)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 24)))
                      (loc_ghost false)))
                    (prf_attributes ())))
                  Closed ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 25)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 25)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 25)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_coerce
            ((pexp_desc (Pexp_variant A ()))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 28)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 30)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))
            (((ptyp_desc
               (Ptyp_variant
                (((prf_desc
                   (Rtag
                    ((txt A)
                     (loc
                      ((loc_start
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 9)))
                       (loc_end
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 11)))
                       (loc_ghost false))))
                    true ()))
                  (prf_loc
                   ((loc_start
                     ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                      (pos_cnum 9)))
                    (loc_end
                     ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                      (pos_cnum 11)))
                    (loc_ghost false)))
                  (prf_attributes ())))
                Closed ()))
              (ptyp_loc
               ((loc_start
                 ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                (loc_end
                 ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 12)))
                (loc_ghost false)))
              (ptyp_loc_stack ()) (ptyp_attributes ())))
            ((ptyp_desc
              (Ptyp_variant
               (((prf_desc
                  (Rtag
                   ((txt A)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 17)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 19)))
                      (loc_ghost false))))
                   true ()))
                 (prf_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 17)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 19)))
                   (loc_ghost false)))
                 (prf_attributes ()))
                ((prf_desc
                  (Rtag
                   ((txt B)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 22)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 24)))
                      (loc_ghost false))))
                   true ()))
                 (prf_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 22)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 24)))
                   (loc_ghost false)))
                 (prf_attributes ())))
               Closed ()))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 25)))
               (loc_ghost false)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 30)))
            (loc_ghost false)))
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

  $ echo 'let x : [`A | `B] = (`A : [`A] :> [`A | `B])' > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt x)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly ()
               ((ptyp_desc
                 (Ptyp_variant
                  (((prf_desc
                     (Rtag
                      ((txt A)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 9)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 11)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 9)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 11)))
                      (loc_ghost false)))
                    (prf_attributes ()))
                   ((prf_desc
                     (Rtag
                      ((txt B)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 14)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 16)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 14)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 16)))
                      (loc_ghost false)))
                    (prf_attributes ())))
                  Closed ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_constraint
            ((pexp_desc
              (Pexp_coerce
               ((pexp_desc (Pexp_variant A ()))
                (pexp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 21)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 23)))
                  (loc_ghost false)))
                (pexp_loc_stack ()) (pexp_attributes ()))
               (((ptyp_desc
                  (Ptyp_variant
                   (((prf_desc
                      (Rtag
                       ((txt A)
                        (loc
                         ((loc_start
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 27)))
                          (loc_end
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 29)))
                          (loc_ghost false))))
                       true ()))
                     (prf_loc
                      ((loc_start
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 27)))
                       (loc_end
                        ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                         (pos_cnum 29)))
                       (loc_ghost false)))
                     (prf_attributes ())))
                   Closed ()))
                 (ptyp_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 26)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 30)))
                   (loc_ghost false)))
                 (ptyp_loc_stack ()) (ptyp_attributes ())))
               ((ptyp_desc
                 (Ptyp_variant
                  (((prf_desc
                     (Rtag
                      ((txt A)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 35)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 37)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 35)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 37)))
                      (loc_ghost false)))
                    (prf_attributes ()))
                   ((prf_desc
                     (Rtag
                      ((txt B)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 40)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 42)))
                         (loc_ghost false))))
                      true ()))
                    (prf_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 40)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 42)))
                      (loc_ghost false)))
                    (prf_attributes ())))
                  Closed ()))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 34)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 43)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 20)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 44)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))
            ((ptyp_desc
              (Ptyp_variant
               (((prf_desc
                  (Rtag
                   ((txt A)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 9)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 11)))
                      (loc_ghost false))))
                   true ()))
                 (prf_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 11)))
                   (loc_ghost false)))
                 (prf_attributes ()))
                ((prf_desc
                  (Rtag
                   ((txt B)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 14)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 16)))
                      (loc_ghost false))))
                   true ()))
                 (prf_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 14)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 16)))
                   (loc_ghost false)))
                 (prf_attributes ())))
               Closed ()))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 8)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 17)))
               (loc_ghost false)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 44)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 44)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 44)))
      (loc_ghost false)))))

  $ echo 'let x :> <m:int> = object method m = 0 method n = 1 end' > file.ml
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
                   (pexp_loc_stack ()) (pexp_attributes ())))
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
           (Ppat_constraint
            ((ppat_desc
              (Ppat_var
               ((txt x)
                (loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
                  (loc_ghost false))))))
             (ppat_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 5)))
               (loc_ghost false)))
             (ppat_loc_stack ()) (ppat_attributes ()))
            ((ptyp_desc
              (Ptyp_poly ()
               ((ptyp_desc
                 (Ptyp_object
                  (((pof_desc
                     (Otag
                      ((txt m)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 10)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 11)))
                         (loc_ghost false))))
                      ((ptyp_desc
                        (Ptyp_constr
                         ((txt (Lident int))
                          (loc
                           ((loc_start
                             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                              (pos_cnum 12)))
                            (loc_end
                             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                              (pos_cnum 15)))
                            (loc_ghost false))))
                         ()))
                       (ptyp_loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 12)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 15)))
                         (loc_ghost false)))
                       (ptyp_loc_stack ()) (ptyp_attributes ()))))
                    (pof_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 10)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 15)))
                      (loc_ghost false)))
                    (pof_attributes ())))
                  Closed))
                (ptyp_loc
                 ((loc_start
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
                  (loc_end
                   ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
                  (loc_ghost false)))
                (ptyp_loc_stack ()) (ptyp_attributes ()))))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
               (loc_ghost true)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (ppat_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
            (loc_ghost true)))
          (ppat_loc_stack ()) (ppat_attributes ())))
        (pvb_expr
         ((pexp_desc
           (Pexp_coerce
            ((pexp_desc
              (Pexp_object
               ((pcstr_self
                 ((ppat_desc Ppat_any)
                  (ppat_loc
                   ((loc_start
                     ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                      (pos_cnum 25)))
                    (loc_end
                     ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                      (pos_cnum 25)))
                    (loc_ghost true)))
                  (ppat_loc_stack ()) (ppat_attributes ())))
                (pcstr_fields
                 (((pcf_desc
                    (Pcf_method
                     (((txt m)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 33)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 34)))
                         (loc_ghost false))))
                      Public
                      (Cfk_concrete Fresh
                       ((pexp_desc
                         (Pexp_poly
                          ((pexp_desc (Pexp_constant (Pconst_integer 0 ())))
                           (pexp_loc
                            ((loc_start
                              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                               (pos_cnum 37)))
                             (loc_end
                              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                               (pos_cnum 38)))
                             (loc_ghost false)))
                           (pexp_loc_stack ()) (pexp_attributes ()))
                          ()))
                        (pexp_loc
                         ((loc_start
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 37)))
                          (loc_end
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 38)))
                          (loc_ghost true)))
                        (pexp_loc_stack ()) (pexp_attributes ()))))))
                   (pcf_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 26)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 38)))
                     (loc_ghost false)))
                   (pcf_attributes ()))
                  ((pcf_desc
                    (Pcf_method
                     (((txt n)
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 46)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 47)))
                         (loc_ghost false))))
                      Public
                      (Cfk_concrete Fresh
                       ((pexp_desc
                         (Pexp_poly
                          ((pexp_desc (Pexp_constant (Pconst_integer 1 ())))
                           (pexp_loc
                            ((loc_start
                              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                               (pos_cnum 50)))
                             (loc_end
                              ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                               (pos_cnum 51)))
                             (loc_ghost false)))
                           (pexp_loc_stack ()) (pexp_attributes ()))
                          ()))
                        (pexp_loc
                         ((loc_start
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 50)))
                          (loc_end
                           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                            (pos_cnum 51)))
                          (loc_ghost true)))
                        (pexp_loc_stack ()) (pexp_attributes ()))))))
                   (pcf_loc
                    ((loc_start
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 39)))
                     (loc_end
                      ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                       (pos_cnum 51)))
                     (loc_ghost false)))
                   (pcf_attributes ())))))))
             (pexp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 19)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 55)))
               (loc_ghost false)))
             (pexp_loc_stack ()) (pexp_attributes ()))
            ()
            ((ptyp_desc
              (Ptyp_object
               (((pof_desc
                  (Otag
                   ((txt m)
                    (loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 10)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 11)))
                      (loc_ghost false))))
                   ((ptyp_desc
                     (Ptyp_constr
                      ((txt (Lident int))
                       (loc
                        ((loc_start
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 12)))
                         (loc_end
                          ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                           (pos_cnum 15)))
                         (loc_ghost false))))
                      ()))
                    (ptyp_loc
                     ((loc_start
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 12)))
                      (loc_end
                       ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                        (pos_cnum 15)))
                      (loc_ghost false)))
                    (ptyp_loc_stack ()) (ptyp_attributes ()))))
                 (pof_loc
                  ((loc_start
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 10)))
                   (loc_end
                    ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0)
                     (pos_cnum 15)))
                   (loc_ghost false)))
                 (pof_attributes ())))
               Closed))
             (ptyp_loc
              ((loc_start
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 9)))
               (loc_end
                ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 16)))
               (loc_ghost false)))
             (ptyp_loc_stack ()) (ptyp_attributes ()))))
          (pexp_loc
           ((loc_start
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 4)))
            (loc_end
             ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 55)))
            (loc_ghost false)))
          (pexp_loc_stack ()) (pexp_attributes ())))
        (pvb_attributes ())
        (pvb_loc
         ((loc_start
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
          (loc_end
           ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 55)))
          (loc_ghost false)))))))
    (pstr_loc
     ((loc_start ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 0)))
      (loc_end ((pos_fname file.ml) (pos_lnum 1) (pos_bol 0) (pos_cnum 55)))
      (loc_ghost false)))))
