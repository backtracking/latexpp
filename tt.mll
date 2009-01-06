(* Ocaml preprocessor *)

{
  open Lexing 
  open Format
  open Options
  open Util
}

let space = [' ' '\t']
let latex_symbol = '\\' | '#' | '$'

rule raw fmt = parse
  | '{'  { pp_print_string fmt "\\{"; raw fmt lexbuf }
  | '}'  { pp_print_string fmt "\\}"; raw fmt lexbuf }
  | latex_symbol as c 
      { fprintf fmt "\\symbol{%d}" (Char.code c); raw fmt lexbuf }
  | "\\pause" | "\\tab"
         { pp_print_string fmt (lexeme lexbuf); raw fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; raw fmt lexbuf }
  | ":"  { pp_print_string fmt "\\symbol{58}"; raw fmt lexbuf }
  | '_'  { pp_print_string fmt "\\_{}"; raw fmt lexbuf }
  | '%'  { pp_print_string fmt "\\%{}"; raw fmt lexbuf }
  | '~'  { pp_print_string fmt "\\~{}"; raw fmt lexbuf }
  | ";;"  { pp_print_string fmt ";\\hspace*{-0.5ex};"; raw fmt lexbuf }
  | '&'  { pp_print_string fmt "\\&{}"; raw fmt lexbuf }
  | '^'  { pp_print_string fmt "\\^{}"; raw fmt lexbuf }
  | '%'  { pp_print_string fmt "\\%{}"; raw fmt lexbuf }
  | '\n' { pp_print_string fmt "\\\\\n"; raw fmt lexbuf }
  | "\n" space* eof { pp_print_string fmt "\n" }
  | eof  { () }
  | _    { pp_print_string fmt (lexeme lexbuf); raw fmt lexbuf }

{
  let raw fmt s = raw fmt (from_string s)

  let () = 
    Pp.add_pp_environment "lightgreen-tt" (lightgreen_box_tt raw);
    Pp.add_pp_environment "lightblue-tt" (lightblue_box_tt raw);
    Pp.add_pp_environment "lightred-tt" (lightred_box_tt raw)

}
