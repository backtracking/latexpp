
(* Ocaml preprocessor *)

{
  open Lexing
  open Format
  open Options
  open Util

  let is_keyword = make_table
    [ "and"; "as"; "assert"; "asr"; "begin";
      "class"; "constraint"; "do"; "done"; "downto"; "else"; "end";
      "exception"; "external"; "false"; "for"; "fun"; "function";
      "functor"; "if"; "in"; "include"; "inherit"; "initializer";
      "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match";
      "method"; "mod"; "module"; "mutable"; "new"; "object"; "of";
      "open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to";
      "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
      "raise";
    ]

  let is_keyword s =
    is_keyword s ||
    is_set "ocamllex" && (s = "rule" || s = "parse" || s = "shortest")

  let color () = is_set "color"
  let is_tt = ref true
}

let space = [' ' '\t']
let latex_symbol =
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^'
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule pp fmt = parse
  | '_'  { if !is_tt then fprintf fmt "\\symbol{95}"
    else fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '|'  { if !is_tt then fprintf fmt "\\symbol{124}"
    else fprintf fmt "\\ensuremath{|}"; pp fmt lexbuf }
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c); pp fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; pp fmt lexbuf }
  | '{'  { if !is_tt then fprintf fmt "\\symbol{123}" else fprintf fmt "\\{";
	   pp fmt lexbuf }
  | '}'  { if !is_tt then fprintf fmt "\\symbol{125}" else fprintf fmt "\\}";
	   pp fmt lexbuf }
(*
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "<-" { fprintf fmt "\\ensuremath{\\leftarrow}"; pp fmt lexbuf }
  | ">"  { if !is_tt then fprintf fmt ">" else fprintf fmt "\\ensuremath{>}";
           pp fmt lexbuf }
  | "<"  { if !is_tt then fprintf fmt "<" else fprintf fmt "\\ensuremath{<}";
           pp fmt lexbuf }
  | ">=" { if !is_tt then fprintf fmt ">="
           else fprintf fmt "\\ensuremath{\\ge}";
           pp fmt lexbuf }
  | "<=" { if !is_tt then fprintf fmt "<="
           else fprintf fmt "\\ensuremath{\\le}";
           pp fmt lexbuf }
*)

(*   | "&&" { fprintf fmt "\\ensuremath{\\land}"; pp fmt lexbuf } *)
(*   | "||" { fprintf fmt "\\ensuremath{\\lor}"; pp fmt lexbuf } *)

(*
  | "==" { fprintf fmt "\\ensuremath{\\equiv}"; pp fmt lexbuf }
  | ":=" { if !is_tt then fprintf fmt ":=" else fprintf fmt "\\ensuremath{:=}";
           pp fmt lexbuf }
  | "!=" { fprintf fmt "\\ensuremath{\\not\\equiv}"; pp fmt lexbuf }
  | "<>" { fprintf fmt "\\ensuremath{\\not=}"; pp fmt lexbuf }
  | "'a" { fprintf fmt "\\ensuremath{\\alpha}"; pp fmt lexbuf }
  | "'b" { fprintf fmt "\\ensuremath{\\beta}"; pp fmt lexbuf }
  | "'c" { fprintf fmt "\\ensuremath{\\gamma}"; pp fmt lexbuf }
  | "*" as c
      { if is_set "ocamllex" then pp_print_char fmt c
	else fprintf fmt "\\ensuremath{\\times}";
	pp fmt lexbuf }
*)
  | "(*"
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt "(*"; comment fmt lexbuf;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  (* strings *)
  | '"' as c
      { pp_print_char fmt c; string fmt lexbuf; pp fmt lexbuf }
  (* characters *)
  | "'" (latex_symbol as c) "'"
         { fprintf fmt "'\\symbol{%d}'" (Char.code c); pp fmt lexbuf }
  | "'" _ "'" as s
      { pp_print_string fmt s; pp fmt lexbuf }
  | "'\\\\'"
      { pp_print_string fmt "'\\symbol{92}\\symbol{92}'"; pp fmt lexbuf }
  | "'\\" ([^'\'']* as s) "'"
      { pp_print_string fmt "'\\symbol{92}"; pp_print_string fmt s;
	pp_print_string fmt "'"; pp fmt lexbuf }
  | ident as s
      {
	if is_set "keywords" && is_keyword s then begin
	  if color () then fprintf fmt "{\\color{blue}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else
          print_ident ~tt:!is_tt fmt s;
	pp fmt lexbuf
      }
  | "\n"
      { fprintf fmt "~\\linebreak"; start_of_line fmt lexbuf }
  | "\n" space* eof
      { pp_print_string fmt "\n" }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; pp fmt lexbuf }

and comment fmt = parse
  | "(*" as s
      { pp_print_string fmt s; comment fmt lexbuf; comment fmt lexbuf }
  | "*)" as s
      { pp_print_string fmt s }
  | latex_symbol as c
      { fprintf fmt "\\symbol{%d}" (Char.code c); comment fmt lexbuf }
  | ' '
      { pp_print_string fmt "\\hspace*{1.22ex}"; comment fmt lexbuf }
  | '{'  { if !is_tt then fprintf fmt "\\symbol{123}" else fprintf fmt "\\{";
	   comment fmt lexbuf }
  | '}'  { if !is_tt then fprintf fmt "\\symbol{125}" else fprintf fmt "\\}";
	   comment fmt lexbuf }
  | ">"
      { fprintf fmt "\\ensuremath{>}"; comment fmt lexbuf }
  | "<"
      { fprintf fmt "\\ensuremath{<}"; comment fmt lexbuf }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; comment fmt lexbuf }

and string fmt = parse
  | '"' as c { pp_print_char fmt c }
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

and start_of_line fmt = parse
  | space* as s
      { indentation fmt (count_spaces s); pp fmt lexbuf }
  | eof
      { () }

{
  let ocaml fmt s =
    let lb = from_string s in start_of_line fmt lb

  let ocaml_tt = noindent_tt ocaml
  let () = Pp.add_pp_environment "ocaml-tt" ocaml_tt

  let ocaml_colorbox_tt = colorname_box_tt "ocaml-bg" ocaml
  let () = Pp.add_pp_environment "ocaml-colorbox-tt" ocaml_colorbox_tt

  let ocaml_lightblue_tt = lightblue_box_tt ocaml
  let () = Pp.add_pp_environment "ocaml-lightblue-tt" ocaml_lightblue_tt
  let () = Pp.add_pp_environment "ocaml" ocaml_lightblue_tt

  let ocaml_sf =
    noindent_sf (fun fmt s -> is_tt := false; ocaml fmt s; is_tt := true)
  let () = Pp.add_pp_environment "ocaml-sf" ocaml_sf

  let ocamllex_lightblue_tt =
    lightblue_box_tt (fun fmt -> with_options ["ocamllex","yes"] (ocaml fmt))
  let () = Pp.add_pp_environment "ocamllex" ocamllex_lightblue_tt

  let pseudocode_lightblue_tt =
    lightblue_box_tt (fun fmt -> with_options ["keywords","no"] (ocaml fmt))
  let () = Pp.add_pp_environment "pseudocode" pseudocode_lightblue_tt

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "ocaml-tt" texttt
  let () = Pp.add_pp_macro "ocaml-sf" textsf
  let () = Pp.add_pp_macro "ocaml" textsf
}

