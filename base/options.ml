let perform_checks = false
(* The checks on extensions are only to get better error messages
   since the compiler will choke on unknown extensions. We disable
   them externally to make it easier to use non ppxlib_base based
   rewriters with ppxlib_base *)
let perform_checks_on_extensions = false
let diff_command = None
