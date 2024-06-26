
(* C preprocessor *)

{
  open Lexing
  open Format
  open Options
  open Util

  let c_keywords =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [ "union"; "sizeof";
        "if"; "else"; "for"; "while"; "do";
        "return";
        "register"; "break"; "continue"; "goto";
        "static"; "const";
        "typedef";
        "switch"; "case"; "default";
      ];
    h

  let is_keyword = Hashtbl.mem c_keywords

  let c_types =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [
	"int"; "long"; "char"; "void"; "struct"; "enum";
      ];
    h

  let is_type = Hashtbl.mem c_types

  let framac_keywords =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [
	"inductive"; "case"; "null"; "valid";"assert";
        "assume"; (* assume is not a keyword of framac but it's handy *)
      ];
    h

  let is_framac_keyword = Hashtbl.mem framac_keywords

  let tab_size = 8

  let count_spaces s =
    let c = ref 0 in
    for i = 0 to String.length s - 1 do
      c := !c + (
        if s.[i] = '\t' then
	  tab_size - (!c mod tab_size)
	else
	  1
      )
    done;
    !c

  let indentation fmt n =
    let space = 0.5 *. (float n) in
    fprintf fmt "\n\\noindent\\hspace*{%2.2fem}" space

  let print_ident fmt =
    let char = function
      | '_' -> pp_print_string fmt "\\_{}"
      | c -> pp_print_char fmt c
    in
    String.iter char

  let color () = is_set "color"
  let tt = ref true

}

let space = [' ' '\t']
let latex_symbol =
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^'
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule pp fmt = parse
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; pp fmt lexbuf }
  | '{'  { fprintf fmt "\\symbol{123}"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\symbol{125}"; pp fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  (* | ':'  { fprintf fmt "\\ensuremath{\\colon}"; pp fmt lexbuf } *)
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '$'  { fprintf fmt "\\${}"; pp fmt lexbuf }
  | '~'  { fprintf fmt "\\symbol{126}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\symbol{92}"; pp fmt lexbuf }
  | "--" { fprintf fmt "{-{}-}"; pp fmt lexbuf }
  (* | "--" { if !tt then fprintf fmt "{-{}-}" else fprintf fmt "\\ensuremath{-{}-}"; *)
  (*          pp fmt lexbuf } *)
  | '<'  { fprintf fmt "\\symbol{60}"; pp fmt lexbuf }
  | '>'  { fprintf fmt "\\symbol{62}"; pp fmt lexbuf }
(*
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
  | "|" { fprintf fmt "\\ensuremath{|}"; pp fmt lexbuf }
  | ">" { fprintf fmt "\\ensuremath{>}"; pp fmt lexbuf }
  | "<" { fprintf fmt "\\ensuremath{<}"; pp fmt lexbuf }
  | '*'  { fprintf fmt "\\ensuremath{\\star}"; pp fmt lexbuf }
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "<-" { fprintf fmt "\\ensuremath{\\leftarrow}"; pp fmt lexbuf }
  | "&&" { fprintf fmt "\\ensuremath{\\land}"; pp fmt lexbuf }
  | "||" { fprintf fmt "\\ensuremath{\\lor}"; pp fmt lexbuf }
*)
  | "/*@"
      {
	pp_print_string fmt "/*@"; framac fmt lexbuf;
	pp fmt lexbuf
      }
  | "/*"
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{ccomment}";
	pp_print_string fmt "/*"; comment fmt lexbuf;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | "//"
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{ccomment}";
	pp_print_string fmt "//"; one_line_comment fmt lexbuf;
	fprintf fmt "}\\linebreak"; start_of_line fmt lexbuf;
	pp fmt lexbuf
      }
  (* strings *)
  | '"'
      { pp_print_string fmt "\\symbol{34}"; string fmt lexbuf; pp fmt lexbuf }
  | ident as s
      {
	if is_keyword s then begin
	  if color () then fprintf fmt "{\\color{ckeyword}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else if is_type s then begin
	  if color () then fprintf fmt "{\\color{ctype}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else
          print_ident fmt s;
	pp fmt lexbuf
      }
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak";
	indentation fmt (count_spaces s);
	pp fmt lexbuf }
  | "\n" space* eof
      { pp_print_string fmt "\n" }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; pp fmt lexbuf }

and string fmt = parse
  | '"' { pp_print_string fmt "\\symbol{34}" }
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c); string fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; string fmt lexbuf }
  | "\n"
      { fprintf fmt "~\\linebreak"; string fmt lexbuf }
  | '\\' '"'
      { fprintf fmt "\\symbol{92}\""; string fmt lexbuf }
  | "\\\\"
      { fprintf fmt "\\symbol{92}\\symbol{92}"; string fmt lexbuf }
  | '{'  { fprintf fmt "\\symbol{123}"; string fmt lexbuf }
  | '}'  { fprintf fmt "\\symbol{125}"; string fmt lexbuf }
  | "\n" space* eof { }
  | eof  { }
  | _ as c { pp_print_char fmt c; string fmt lexbuf }

and comment fmt = parse
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak"; indentation fmt (count_spaces s);
	comment fmt lexbuf }
  | "*/" as s
      { pp_print_string fmt s }
  | '\\'  { fprintf fmt "\\symbol{92}"; comment fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; comment fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; comment fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; comment fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; comment fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; comment fmt lexbuf }
  | "&" { fprintf fmt "\\&{}"; comment fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; comment fmt lexbuf }
  | ">" { fprintf fmt ">"; comment fmt lexbuf }
  | "<" { fprintf fmt "<"; comment fmt lexbuf }
(*
  | "|" { fprintf fmt "\\ensuremath{|}"; comment fmt lexbuf }
  | ">" { fprintf fmt "\\ensuremath{>}"; comment fmt lexbuf }
  | "<" { fprintf fmt "\\ensuremath{<}"; comment fmt lexbuf }
  | ">=" { fprintf fmt "\\ensuremath{\\ge}"; comment fmt lexbuf }
  | "<=" { fprintf fmt "\\ensuremath{\\le}"; comment fmt lexbuf }
  | "=>" { fprintf fmt "\\ensuremath{\\Rightarrow}"; comment fmt lexbuf }
  | "&&" { fprintf fmt "\\ensuremath{\\land}"; comment fmt lexbuf }
  | "||" { fprintf fmt "\\ensuremath{\\lor}"; comment fmt lexbuf }
  | "==" { fprintf fmt "\\ensuremath{\\equiv}"; comment fmt lexbuf }
  | "!=" { fprintf fmt "\\ensuremath{\\not\\equiv}"; comment fmt lexbuf }
*)
  | " " { fprintf fmt "~"; comment fmt lexbuf }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; comment fmt lexbuf }

and framac fmt = parse
  | '{'  { fprintf fmt "\\symbol{123}"; framac fmt lexbuf }
  | '}'  { fprintf fmt "\\symbol{125}"; framac fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; framac fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; framac fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; framac fmt lexbuf }
  | ':'  { fprintf fmt "\\ensuremath{\\colon}"; framac fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; framac fmt lexbuf }
  | '~'  { fprintf fmt "\\symbol{126}"; framac fmt lexbuf }
  | '\\'  { fprintf fmt "\\symbol{92}"; framac fmt lexbuf }
  | "--" { if !tt then fprintf fmt "--" else fprintf fmt "\\ensuremath{-{}-}";
	   framac fmt lexbuf }
  | ">" { if !tt then fprintf fmt ">" else fprintf fmt "\\ensuremath{>}";
	  framac fmt lexbuf }
  | "<" { if !tt then fprintf fmt "<" else fprintf fmt "\\ensuremath{<}";
	  framac fmt lexbuf }
  | ">=" { if !tt then fprintf fmt ">=" else fprintf fmt "\\ensuremath{\\ge}";
	   framac fmt lexbuf }
  | "<=" { if !tt then fprintf fmt "<=" else fprintf fmt "\\ensuremath{\\le}";
	   framac fmt lexbuf }
  | "=="
      { if !tt then fprintf fmt "==" else fprintf fmt "\\ensuremath{\\equiv}";
	framac fmt lexbuf }
  | "!="
  { if !tt then fprintf fmt "!=" else fprintf fmt "\\ensuremath{\\not\\equiv}";
    framac fmt lexbuf }
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; framac fmt lexbuf }
  | "<-" { fprintf fmt "\\ensuremath{\\leftarrow}"; framac fmt lexbuf }
  | ">" { fprintf fmt "\\ensuremath{>}"; framac fmt lexbuf }
  | "<" { fprintf fmt "\\ensuremath{<}"; framac fmt lexbuf }
  | ">=" { fprintf fmt "\\ensuremath{\\ge}"; framac fmt lexbuf }
  | "<=" { fprintf fmt "\\ensuremath{\\le}"; framac fmt lexbuf }
  | "==>" { fprintf fmt "\\ensuremath{\\Rightarrow}"; framac fmt lexbuf }
  | "&&" { fprintf fmt "\\ensuremath{\\land}"; framac fmt lexbuf }
  | "||" { fprintf fmt "\\ensuremath{\\lor}"; framac fmt lexbuf }
  | "==" { fprintf fmt "\\ensuremath{\\equiv}"; framac fmt lexbuf }
  | "!=" { fprintf fmt "\\ensuremath{\\not\\equiv}"; framac fmt lexbuf }
  | "\\forall" { fprintf fmt "\\ensuremath{\\forall}"; framac fmt lexbuf }
  | "@*/" as s { pp_print_string fmt s }
  | ('\\'? as b) (ident as s)
      {
        let print_backslach ()=
          if b = "\\" then pp_print_string fmt "\\symbol{92}" in
	if is_framac_keyword s then begin
	  if color () then fprintf fmt "{\\color{ckeyword}"
	  else fprintf fmt "\\textbf{";
          print_backslach ();
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else if is_type s then begin
	  if color () then fprintf fmt "{\\color{ctype}"
	  else fprintf fmt "\\textbf{";
          print_backslach ();
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else begin
          print_backslach ();
          print_ident fmt s;
        end;
	framac fmt lexbuf
      }
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak";
	indentation fmt (count_spaces s);
	framac fmt lexbuf }
  | "\n" space* eof
      { pp_print_string fmt "\n" }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; framac fmt lexbuf }

and one_line_comment fmt = parse
  | "\n" { () }
  | '\\'
      { fprintf fmt "\\symbol{92}"; one_line_comment fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; one_line_comment fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; one_line_comment fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; one_line_comment fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; one_line_comment fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; one_line_comment fmt lexbuf }
  | "&" { fprintf fmt "\\&{}"; one_line_comment fmt lexbuf }
  | " " { fprintf fmt "~"; one_line_comment fmt lexbuf }
  | eof  { () }
  | _ as c { pp_print_char fmt c; one_line_comment fmt lexbuf }

and start_of_line fmt = parse
  | space* as s
      { indentation fmt (count_spaces s) }
  | eof
      { () }


{
  let c_alltt fmt s =
    fprintf fmt "\\begin{alltt}";
    let lb = from_string s in
    tt := true; start_of_line fmt lb; pp fmt lb; tt := false;
    fprintf fmt "\\end{alltt}%%\n"

  let () = Pp.add_pp_environment "c-alltt" c_alltt

  let c fmt s =
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb

  let c_tt =
    noindent_tt (fun fmt s -> tt := true; c fmt s; tt := false)

  let () = Pp.add_pp_environment "c-tt" c_tt
  let () = Pp.add_pp_environment "c" c_tt

  let lightblue_c_tt = lightblue_box_tt c
  let () = Pp.add_pp_environment "c-lightblue-tt" lightblue_c_tt
  let lightgray_c_tt = lightgray_box_tt c
  let () = Pp.add_pp_environment "c-lightgray-tt" lightgray_c_tt

  let c_sf =
    noindent_sf (fun fmt s -> tt := false; c fmt s; tt := true)

  let () = Pp.add_pp_environment "c-sf" c_sf

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    tt := true; pp fmt (from_string s); tt := false;
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    tt := false; pp fmt (from_string s); tt := true;
    fprintf fmt "}"

  let () = Pp.add_pp_macro "c-tt" texttt
  let () = Pp.add_pp_macro "c-sf" textsf
  let () = Pp.add_pp_macro "c" texttt
}

