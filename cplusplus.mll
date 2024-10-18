
(* C++ preprocessor *)

{
  open Lexing
  open Format
  open Options
  open Util

  let c_keywords =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [ "union"; "sizeof";
        "if"; "else"; "for"; "while";
        "return";
        "register"; "break"; "continue"; "goto";
        "static"; "const";

        "class"; "public"; "protected"; "virtual"; "using"; "namespace";
      ];
    h

  let is_keyword = Hashtbl.mem c_keywords

  let c_types =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      [
	"int"; "char"; "void"; "struct";
      ];
    h

  let is_type = Hashtbl.mem c_types

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

  let print_green_ident fmt s =
    fprintf fmt "{\\color{cpptype}";
    print_ident fmt s;
    fprintf fmt "}"

  let ident fmt s =
    if is_keyword s then begin
      if color () then fprintf fmt "{\\color{cppkeyword}"
      else fprintf fmt "\\textbf{";
      pp_print_string fmt s;
      fprintf fmt "}"
    end else if is_type s && color () then
      print_green_ident fmt s
    else
      print_ident fmt s

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let latex_symbol =
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^'

rule pp fmt = parse
  | '{'  { fprintf fmt "\\symbol{123}"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\symbol{125}"; pp fmt lexbuf }
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c); pp fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; pp fmt lexbuf }
  (* | ':'  { fprintf fmt "\\ensuremath{\\colon}"; pp fmt lexbuf } *)
  | "--" { if !tt then fprintf fmt "--" else fprintf fmt "\\ensuremath{-{}-}";
	   pp fmt lexbuf }
  | '<'  { fprintf fmt "\\symbol{60}"; pp fmt lexbuf }
  | '>'  { fprintf fmt "\\symbol{62}"; pp fmt lexbuf }
  (* | ">=" { if !tt then fprintf fmt ">=" else fprintf fmt "\\ensuremath{\\ge}"; *)
  (*          pp fmt lexbuf } *)
  (* | "<=" { if !tt then fprintf fmt "<=" else fprintf fmt "\\ensuremath{\\le}"; *)
  (*          pp fmt lexbuf } *)
  (* | "==" *)
  (*     { if !tt then fprintf fmt "==" else fprintf fmt "\\ensuremath{\\equiv}"; *)
  (*       pp fmt lexbuf } *)
  (* | "!=" *)
  (* { if !tt then fprintf fmt "!=" else fprintf fmt "\\ensuremath{\\not\\equiv}"; *)
  (*   pp fmt lexbuf } *)
  | '"' as c { pp_print_char fmt c; string fmt lexbuf; pp fmt lexbuf }
  | "/*"
      {
	fprintf fmt "\\emph{";
	if color () then fprintf fmt "\\color{cppcomment}";
	pp_print_string fmt "/*"; comment fmt lexbuf;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | "//"
      {
	fprintf fmt "\\emph{";
	if color () then fprintf fmt "\\color{cppcomment}";
	pp_print_string fmt "//"; one_line_comment fmt lexbuf;
	fprintf fmt "}\\linebreak"; start_of_line fmt lexbuf;
	pp fmt lexbuf
      }
  | ident as s
      { ident fmt s; pp fmt lexbuf }
  | (ident as id1) (space+ as s) (('*'* ident) as id2)
      { begin
	  if is_keyword id1 || is_keyword id2 || not (color ()) then
	    ident fmt id1
	  else
	    print_green_ident fmt id1
	end;
	pp_print_string fmt s; ident fmt id2; pp fmt lexbuf }
  | (ident as id1) (space+ as s) '&' (ident as id2)
      { begin
	  if is_keyword id1 || is_keyword id2 || not (color ()) then
	    ident fmt id1
	  else
	    print_green_ident fmt id1
	end;
	pp_print_string fmt s; fprintf fmt "\\&{}"; ident fmt id2;
        pp fmt lexbuf }
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
  | " " { fprintf fmt "~"; comment fmt lexbuf }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; comment fmt lexbuf }

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

and string fmt = parse
  | '"' as c { pp_print_char fmt c }
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak";
	indentation fmt (count_spaces s);
	string fmt lexbuf }
  | '\\' '"'
      { fprintf fmt "\\symbol{92}\""; string fmt lexbuf }
  | '\\'
      { fprintf fmt "\\symbol{92}"; string fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; string fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; string fmt lexbuf }
  | '$' { fprintf fmt "\\${}"; string fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; string fmt lexbuf }
  | '^' { fprintf fmt "\\^{}"; string fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; string fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; string fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; string fmt lexbuf }
  | "&" { fprintf fmt "\\&{}"; string fmt lexbuf }
  | " " { fprintf fmt "~"; string fmt lexbuf }
  | "\n" space* eof { }
  | eof  { }
  | _ as c { pp_print_char fmt c; string fmt lexbuf }

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

  let () = Pp.add_pp_environment "c++-alltt" c_alltt

  let c fmt s =
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb

  let c_tt =
    noindent_tt (fun fmt s -> tt := true; c fmt s; tt := false)

  let c_tt fmt s =
    fprintf fmt "\\begin{flushleft}\\ttfamily\\parindent 0pt\n";
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{flushleft}%%\n"

  let () = Pp.add_pp_environment "c++-tt" c_tt
  let () = Pp.add_pp_environment "c++" c_tt

  let lightblue_c_tt = lightblue_box_tt c
  let () = Pp.add_pp_environment "lightblue-c++" lightblue_c_tt
  let () = Pp.add_pp_environment "c++-lightblue-tt" lightblue_c_tt
  let () = Pp.add_pp_environment "c++-lightgray-tt" (lightgray_box_tt c_tt)

  let c_sf =
    noindent_sf (fun fmt s -> tt := false; c fmt s; tt := true)

  let () = Pp.add_pp_environment "c++-sf" c_sf

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    tt := true; pp fmt (from_string s); tt := false;
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    tt := false; pp fmt (from_string s); tt := true;
    fprintf fmt "}"

  let () = Pp.add_pp_macro "c++-tt" texttt
  let () = Pp.add_pp_macro "c++-sf" textsf
  let () = Pp.add_pp_macro "c++" texttt
}

