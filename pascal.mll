
(* Pascal preprocessor *)

{
  open Lexing
  open Format
  open Options
  open Util

  let is_keyword = make_table
    [
      "if"; "then"; "else"; "while"; "do"; "program"; "procedure";
      "begin"; "end"; "record"; "forward";
    ]

  let is_type = make_table
    [
      "var"; "integer"; "array"; "of";
    ]

  let color () = is_set "color"
  let tt = ref false

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let latex_symbol =
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^'

rule pp fmt = parse
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c); pp fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1ex}"; pp fmt lexbuf }
(*
  | ">" { if !tt then fprintf fmt ">" else fprintf fmt "\\ensuremath{>}";
	  pp fmt lexbuf }
  | "<" { if !tt then fprintf fmt "<" else fprintf fmt "\\ensuremath{<}";
	  pp fmt lexbuf }
  | ">=" { if !tt then fprintf fmt ">=" else fprintf fmt "\\ensuremath{\\ge}";
	   pp fmt lexbuf }
  | "<=" { if !tt then fprintf fmt "<=" else fprintf fmt "\\ensuremath{\\le}";
	   pp fmt lexbuf }
  | "=="
      { if !tt then fprintf fmt "==" else fprintf fmt "\\ensuremath{\\equiv}";
	pp fmt lexbuf }
  | "!="
  { if !tt then fprintf fmt "!=" else fprintf fmt "\\ensuremath{\\not\\equiv}";
    pp fmt lexbuf }
*)
  | "(*"
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{pascalcomment}";
	pp_print_string fmt "(*"; comment fmt lexbuf;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | "{"
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{pascalcomment}";
	pp_print_string fmt "\\symbol{123}"; comment fmt lexbuf;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | ident as s
      {
	if is_keyword s then begin
	  if color () then fprintf fmt "{\\color{pascalkeyword}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else if is_type s then begin
	  if color () then fprintf fmt "{\\color{pascaltype}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else
          print_ident fmt s;
	pp fmt lexbuf
      }
  | '\n' { pp_print_string fmt "\\\\\n"; pp fmt lexbuf }
  | "\n" space* eof { pp_print_string fmt "\n" }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; pp fmt lexbuf }

and comment fmt = parse
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak"; indentation fmt (count_spaces s);
	comment fmt lexbuf }
  | "*)" { pp_print_string fmt "*)" }
  | '}'  { fprintf fmt "\\symbol{125}" }
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c);
	   comment fmt lexbuf }
  | ">" { fprintf fmt "\\ensuremath{>}"; comment fmt lexbuf }
  | "<" { fprintf fmt "\\ensuremath{<}"; comment fmt lexbuf }
  | " " { fprintf fmt "~"; comment fmt lexbuf }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; comment fmt lexbuf }

and one_line_comment fmt = parse
  | "\n" { () }
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c);
	   one_line_comment fmt lexbuf }
  | " " { fprintf fmt "~"; one_line_comment fmt lexbuf }
  | eof  { () }
  | _ as c { pp_print_char fmt c; one_line_comment fmt lexbuf }

and start_of_line fmt = parse
  | space* as s
      { indentation fmt (count_spaces s) }
  | eof
      { () }


{
  let pascal fmt s =
    let lb = from_string s in start_of_line fmt lb; pp fmt lb

  let pascal_tt = noindent_tt pascal
  let () = Pp.add_pp_environment "pascal-tt" pascal_tt

  let pascal_lightblue_tt = lightblue_box_tt pascal
  let () =
    Pp.add_pp_environment "pascal-lightblue-tt" pascal_lightblue_tt;
    Pp.add_pp_environment "pascal" pascal_lightblue_tt;
    Pp.add_pp_environment "pascal-lightgray-tt" (lightgray_box_tt pascal_tt)

}

