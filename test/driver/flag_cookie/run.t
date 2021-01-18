The cookie flag is taken into account by the main standalone

  $ echo "[@@@print_cookie_x]" > impl.ml
  $ print_cookie_driver -cookie x=1 impl.ml
  Value of cookie x: 1

[To be fixed]
And should also be taken into account by the `-as-ppx` standalone,
but isn't at the moment

  $ ocaml -ppx 'print_cookie_driver --as-ppx -cookie x=1' impl.ml
  Cookie x isn't set.
