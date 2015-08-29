(* Ocaml preprocessor *)

{
  open Lexing
  open Format
  open Options
  open Util
}

let space = [' ' '\t']
let latex_symbol =
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^' | '{' | '}'

rule raw fmt = parse
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c); raw fmt lexbuf }
  | "\\pause" | "\\tab" as s
         { pp_print_string fmt s; raw fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; raw fmt lexbuf }
  | '\n' { pp_print_string fmt "\\\\\n"; raw fmt lexbuf }
  | "\n" space* eof
         { pp_print_string fmt "\n" }
  | eof  { () }
  | _ as c { pp_print_char fmt c; raw fmt lexbuf }

{
  let raw fmt s = raw fmt (from_string s)

  let () =
    Pp.add_pp_environment "tt" (noindent_tt raw);
    Pp.add_pp_environment "lightgreen-tt" (lightgreen_box_tt raw);
    Pp.add_pp_environment "lightblue-tt" (lightblue_box_tt raw);
    Pp.add_pp_environment "lightgray-tt" (lightgray_box_tt raw);
    Pp.add_pp_environment "lightred-tt" (lightred_box_tt raw)
}
