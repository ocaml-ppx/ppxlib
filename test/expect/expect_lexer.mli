type version = int * int

type version_range =
  | Up_to of version
  | From of version
  | Between of version * version

type versioned_expect = version_range * string

val split_file :
  file_contents:string ->
  Lexing.lexbuf ->
  (Lexing.position * string * versioned_expect list) list
