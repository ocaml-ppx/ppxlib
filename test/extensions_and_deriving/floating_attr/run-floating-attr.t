Test `attr_str_floating_expect` via `@@@identity_inline`.

  $ cat << 'EOF' > program.ml
  > [@@@identity_inline
  >   module T : sig
  >     val foo : [%str]
  >   end = struct
  >     let foo = [%suffix "apples"]
  >   end]
  > [@@@end]
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 7, characters 0-1:
   [@@@identity_inline
     module T : sig
       val foo : [%str]
     end = struct
       let foo = [%suffix "apples"]
     end]
  +module T : sig val foo : [%str ] end =
  +  struct let foo = [%suffix "apples"] end
   [@@@end]
  [1]

Test `attr_sig_floating_expect` via `@@@identity_inline`.

  $ cat << 'EOF' > program.ml
  > module type S = sig
  >   [@@@identity_inline:
  >     val foo : [%str]
  >     include module type of struct
  >       let foo = [%suffix "apples"]
  >     end]
  >   [@@@end]
  > end 
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 7, characters 0-1:
   module type S = sig
     [@@@identity_inline:
       val foo : [%str]
       include module type of struct
         let foo = [%suffix "apples"]
       end]
  -  [@@@end]
  +  
  +val foo : [%str ]
  +include module type of struct let foo = [%suffix "apples"] end
  +[@@@end]
   end 
  [1]

Test `attr_str_floating_expect_and_expand` via `@@@identity_inline_expanded`.

  $ cat << 'EOF' > program.ml
  > [@@@identity_inline_expanded
  >   module T : sig
  >     val foo : [%str]
  >   end = struct
  >     let foo = [%suffix "apples"]
  >   end]
  > [@@@end]
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 7, characters 0-1:
   [@@@identity_inline_expanded
     module T : sig
       val foo : [%str]
     end = struct
       let foo = [%suffix "apples"]
     end]
  +module T : sig val foo : string end = struct let foo = "apples_suffix" end
   [@@@end]
  [1]

Test `attr_sig_floating_expect_and_expand` via `@@@identity_inline_expanded`.

  $ cat << 'EOF' > program.ml
  > module type S = sig
  >   [@@@identity_inline_expanded:
  >     val foo : [%str]
  >     include module type of struct
  >       let foo = [%suffix "apples"]
  >     end]
  >   [@@@end]
  > end 
  > EOF
  $ ./ppx.exe -no-color -null program.ml
  -program.ml
  +program.ml.ppx-corrected
  File "program.ml", line 7, characters 0-1:
   module type S = sig
     [@@@identity_inline_expanded:
       val foo : [%str]
       include module type of struct
         let foo = [%suffix "apples"]
       end]
  -  [@@@end]
  +  
  +val foo : string
  +include module type of struct let foo = "apples_suffix" end
  +[@@@end]
   end 
  [1]
