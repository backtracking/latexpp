(* Ocaml preprocessor *)

{
  open Lexing 
  open Format
  open Options
  open Util
}

let space = [' ' '\t']

rule raw fmt = parse
  | '{'  { pp_print_string fmt "\\{"; raw fmt lexbuf }
  | '}'  { pp_print_string fmt "\\}"; raw fmt lexbuf }
  | '\\' { pp_print_string fmt "\\ensuremath{\\backslash}"; raw fmt lexbuf }
  | "\\pause" | "\\tab"
         { pp_print_string fmt (lexeme lexbuf); raw fmt lexbuf }
  | '#' { pp_print_string fmt "\\#{}";
	  raw fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1ex}"; raw fmt lexbuf }
  | " :"  { pp_print_string fmt "\\hspace*{0ex}:"; raw fmt lexbuf }
  | "::"  { pp_print_string fmt ":\\hspace{-1ex}:"; raw fmt lexbuf }
  | " ::" { pp_print_string fmt "\\hspace*{0ex}:\\hspace*{-1ex}:"; raw fmt lexbuf }
  | '_'  { pp_print_string fmt "\\_{}"; raw fmt lexbuf }
  | '%'  { pp_print_string fmt "\\%{}"; raw fmt lexbuf }
  | ";;"  { pp_print_string fmt ";\\hspace*{-0.5ex};"; raw fmt lexbuf }
  | '&'  { pp_print_string fmt "\\&{}"; raw fmt lexbuf }
  | '%'  { pp_print_string fmt "\\%{}"; raw fmt lexbuf }
  | '\n' { pp_print_string fmt "\\\\\n"; raw fmt lexbuf }
  | "\n" space* eof { pp_print_string fmt "\n" }
  | eof  { () }
  | _    { pp_print_string fmt (lexeme lexbuf); raw fmt lexbuf }

and start_of_line_raw fmt = parse
  | space* as s
      { indentation fmt (count_spaces s); raw fmt lexbuf }
  | eof 
      { () }

{
  let color_box_tt color fmt s =
    fprintf fmt "\\colorbox{%s}{\\begin{minipage}{\\textwidth}\\tt\\parindent 0pt\n" color;
    start_of_line_raw fmt (from_string s);
    fprintf fmt "\\end{minipage}}\n"

  let () = 
    Pp.add_pp_environment "lightgreen-tt" (color_box_tt "lightgreen");
    Pp.add_pp_environment "lightblue-tt" (color_box_tt "lightblue");
    Pp.add_pp_environment "lightred-tt" (color_box_tt "lightred");

}
