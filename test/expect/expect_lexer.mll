{
open StdLabels

type version = int * int

type version_range =
  | Up_to of version
  | From of version
  | Between of version * version

type versioned_expect = version_range * string

let make_version major minor =
  let major = int_of_string major in
  let minor = int_of_string minor in
  (major, minor)

let extract_chunk txt start lexbuf =
  let pos = start.Lexing.pos_cnum in
  let len = Lexing.lexeme_start lexbuf - pos in
  String.sub txt ~pos ~len
}

let digit = ['0'-'9']

(* Entrypoint
   Parses blocks of code to execute seperated by [%%expect{|...|}] statement.
   Code blocks can be separated by a single [%%expect{|...|}] statement or by a
   series of [%%expect_in <version-range> {|...|}]. *)
rule code txt start = parse
  | "[%%expect{|\n" {
    let s = extract_chunk txt start lexbuf in
    Lexing.new_line lexbuf;
    (start, s, []) :: expectation txt lexbuf
  }
  | "[%%expect_in " (digit+ as major) '.' (digit+ as minor) " + {|\n" {
      let s = extract_chunk txt start lexbuf in
      Lexing.new_line lexbuf;
      let version = make_version major minor in
      let range = From version in
      let cstart = lexbuf.lex_curr_p in
      versioned_expectation_content txt (start, s) [] (range, cstart) lexbuf
  }
  | "[%%expect_in " (digit+ as major) '.' (digit+ as minor) " - {|\n" {
      let s = extract_chunk txt start lexbuf in
      Lexing.new_line lexbuf;
      let version = make_version major minor in
      let range = Up_to version in
      let cstart = lexbuf.lex_curr_p in
      versioned_expectation_content txt (start, s) [] (range, cstart) lexbuf
  }
  | "[%%expect_in "
    (digit+ as major1) '.' (digit+ as minor1)
    " >> "
    (digit+ as major2) '.' (digit+ as minor2)
    " {|\n" {
      let s = extract_chunk txt start lexbuf in
      Lexing.new_line lexbuf;
      let v1 = make_version major1 minor1 in
      let v2 = make_version major2 minor2 in
      let range = Between (v1, v2) in
      let cstart = lexbuf.lex_curr_p in
      versioned_expectation_content txt (start, s) [] (range, cstart) lexbuf
  }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    code txt start lexbuf
  }
  | eof {
    let pos = start.Lexing.pos_cnum in
    let len = String.length txt - pos in
    if pos > 0 then begin
      let s = String.sub txt ~pos ~len in
      if String.trim s = "" then
        []
      else
        [(start, s, [])]
    end else
      []
  }

and expectation txt = parse
  | "|}]\n" {
      Lexing.new_line lexbuf;
      code txt lexbuf.lex_curr_p lexbuf
    }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    expectation txt lexbuf
  }

(* Parses the content of a [%%expect_in .. {| ... |}] block along with following
   blocks in the same group *)
and versioned_expectation_content txt code_chunk vexpects curr = parse
  | "|}]\n[%%expect_in " (digit+ as major) '.' (digit+ as minor) " + {|\n" {
    let range, start = curr in
    let s = extract_chunk txt start lexbuf in
    Lexing.new_line lexbuf;
    Lexing.new_line lexbuf;
    let block = range, s in
    let version = make_version major minor in
    let next_range = From version in
    let cstart = lexbuf.lex_curr_p in
    versioned_expectation_content txt code_chunk
      (block::vexpects) (next_range, cstart) lexbuf
  }
  | "|}]\n[%%expect_in " (digit+ as major) '.' (digit+ as minor) " - {|\n" {
    let range, start = curr in
    let s = extract_chunk txt start lexbuf in
    Lexing.new_line lexbuf;
    Lexing.new_line lexbuf;
    let block = range, s in
    let version = make_version major minor in
    let next_range = Up_to version in
    let cstart = lexbuf.lex_curr_p in
    versioned_expectation_content txt code_chunk
      (block::vexpects) (next_range, cstart) lexbuf
  }
  | "|}]\n[%%expect_in "
    (digit+ as major1) '.' (digit+ as minor1)
    " >> "
    (digit+ as major2) '.' (digit+ as minor2)
    " {|\n" {
    let range, start = curr in
    let s = extract_chunk txt start lexbuf in
    Lexing.new_line lexbuf;
    Lexing.new_line lexbuf;
    let block = range, s in
    let v1 = make_version major1 minor1 in
    let v2 = make_version major2 minor2 in
    let next_range = Between (v1, v2) in
    let cstart = lexbuf.lex_curr_p in
    versioned_expectation_content txt code_chunk
      (block::vexpects) (next_range, cstart) lexbuf
  }
  | "|}]\n" {
    let range, start = curr in
    let pos = start.Lexing.pos_cnum in
    let len = Lexing.lexeme_start lexbuf - pos in
    let s = String.sub txt ~pos ~len in
    Lexing.new_line lexbuf;
    let vexpects = List.rev ((range, s)::vexpects) in
    let start, s = code_chunk in
    (start, s, vexpects) :: code txt lexbuf.lex_curr_p lexbuf
  }
  | [^'\n']*'\n' {
    Lexing.new_line lexbuf;
    versioned_expectation_content txt code_chunk vexpects curr lexbuf
  }

{
  let split_file ~file_contents lexbuf =
    code file_contents lexbuf.Lexing.lex_curr_p lexbuf
}
