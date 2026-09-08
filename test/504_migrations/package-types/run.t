The signature for package types, as well as packed expressions
changed inbetween.

Originally we had a bug in the migrations that would silently drop some
attributes.

  $ cat > test.ml << EOF
  > module type S = sig type t end
  > let f (module K : S with type t = int [@foo]) = ()
  > let f (module K : S with type t = (int [@foo])) = ()
  > let f (module K : S with type t = (int [@foo])[@bar]) = ()
  > type t = (module S [@inner])[@outer]
  > 
  > EOF

  $ ./id_driver.exe test.ml
  module type S  = sig type t end
  let f ((module K)  : (((module S with type t = int))[@foo ])) = ()
  let f ((module K)  : (module S with type t = ((int)[@foo ]))) = ()
  let f ((module K)  : (((module S with type t = ((int)[@foo ])))[@bar ])) = ()
  type t = (((module S))[@outer ][@inner ])
  $ ./id_driver.exe --use-compiler-pp test.ml
  module type S  = sig type t end
  let f ((module K)  : (((module S with type t = int))[@foo ])) = ()
  let f ((module K)  : (module S with type t = ((int)[@foo ]))) = ()
  let f ((module K)  : (((module S with type t = ((int)[@foo ])))[@bar ])) = ()
  type t = (((module S))[@outer ][@inner ])

