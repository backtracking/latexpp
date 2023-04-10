
(* Python *)

{
  open Lexing
  open Format
  open Options
  open Util

  let python_keywords =
    let h = Hashtbl.create 97 in
    List.iter (fun s -> Hashtbl.add h s ())
      ["def"; "if"; "else"; "elif";
       "return";
       "for"; "in";
       "and"; "or"; "not"; "while";
       "True";
       "False";
       "None";
       "from"; "import";
       "assert";
      ];
    h

  let is_keyword = Hashtbl.mem python_keywords

  let color () = is_set "color"

  let print_green_ident fmt s =
    fprintf fmt "{\\color{pythontype}";
    print_ident fmt s;
    fprintf fmt "}"

  let ident fmt s =
    if is_keyword s then begin
      if color () then fprintf fmt "{\\color{pythonkeyword}"
      else fprintf fmt "\\textbf{";
      pp_print_string fmt s;
      fprintf fmt "}"
    end else
      print_ident ~tt:true fmt s

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule pp fmt = parse
  | '{'  { fprintf fmt "\\symbol{123}"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\symbol{125}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\symbol{95}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '$'  { fprintf fmt "\\${}"; pp fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; pp fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\symbol{92}"; pp fmt lexbuf }
  | '^'  { fprintf fmt "\\symbol{94}"; pp fmt lexbuf }
  | '<'  { fprintf fmt "\\symbol{60}"; pp fmt lexbuf }
  | '>'  { fprintf fmt "\\symbol{62}"; pp fmt lexbuf }
  | "--" { fprintf fmt "{-{}-}"; pp fmt lexbuf }
  | '"' as c { pp_print_char fmt c; string fmt lexbuf; pp fmt lexbuf }
  | ("@" ident) as p
      { fprintf fmt "{%s%s}"
          (if color () then "\\color{pythoncomment}" else "") p;
	pp fmt lexbuf
      }
  | "#"
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{pythoncomment}";
	pp_print_string fmt "\\#{}";
	one_line_comment fmt lexbuf;
	start_of_line fmt lexbuf;
	pp fmt lexbuf
      }
  | ident as s
      { ident fmt s; pp fmt lexbuf }
(*
  | (ident as id1) (space+ as s) (ident as id2)
      { begin
	  if is_keyword id1 || is_keyword id2 || not (color ()) then begin
	    ident fmt id1; pp_print_string fmt s; ident fmt id2
	  end else begin
	    print_green_ident fmt id1; pp_print_string fmt s; ident fmt id2;
	  end
	end;
	pp fmt lexbuf }
*)
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

and one_line_comment fmt = parse
  | "\n" { fprintf fmt "}~\\linebreak" }
  | '\\'
      { fprintf fmt "\\symbol{92}"; one_line_comment fmt lexbuf }
  | '{'  { fprintf fmt "\\symbol{123}"; one_line_comment fmt lexbuf }
  | '}'  { fprintf fmt "\\symbol{125}"; one_line_comment fmt lexbuf }
  | '$' { fprintf fmt "\\${}"; one_line_comment fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; one_line_comment fmt lexbuf }
  | '^' { fprintf fmt "\\^{}"; one_line_comment fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; one_line_comment fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; one_line_comment fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; one_line_comment fmt lexbuf }
  | "&" { fprintf fmt "\\&{}"; one_line_comment fmt lexbuf }
  | " " { fprintf fmt "~"; one_line_comment fmt lexbuf }
  | "\n" space* eof { fprintf fmt "}" }
  | eof  { fprintf fmt "}" }
  | _ as c { pp_print_char fmt c; one_line_comment fmt lexbuf }

and string fmt = parse
  | '"' as c { pp_print_char fmt c }
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak";
	indentation fmt (count_spaces s);
	string fmt lexbuf }
  | '\\' '"'
      { fprintf fmt "\\ensuremath{\\backslash}\""; string fmt lexbuf }
  | '\\'
      { fprintf fmt "\\ensuremath{\\backslash}"; string fmt lexbuf }
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
  let python_tt fmt s =
    fprintf fmt "\\begin{flushleft}\\ttfamily\\parindent 0pt\n";
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{flushleft}%%\n"

  let python_lightblue_tt fmt s =
    fprintf fmt "\\colorbox{lightblue}{\\begin{minipage}{\\textwidth}\\ttfamily\\parindent 0pt\n";
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{minipage}}\n"

  let () = Pp.add_pp_environment "python-lightblue-tt" python_lightblue_tt
  let () = Pp.add_pp_environment "python-tt" python_tt
  let () = Pp.add_pp_environment "python" python_lightblue_tt
  let () = Pp.add_pp_environment "python-lightgray-tt" (lightgray_box_tt python_tt)

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "python-tt" texttt
  let () = Pp.add_pp_macro "python-sf" textsf
  let () = Pp.add_pp_macro "python" textsf
}

