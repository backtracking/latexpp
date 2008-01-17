
(* main scanner *)

{
  open Lexing 
  open Format

  let out = ref std_formatter
  let print_string s = pp_print_string !out s
  let print_char c = pp_print_char !out c
}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule pp = parse
  | "\\begin{" (ident as s) "}"
      { pp lexbuf }
  | eof 
      { () }
  | _ as c
      { print_char c; pp lexbuf }

{
}
