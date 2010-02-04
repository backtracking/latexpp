
(* Java *)

{
  open Lexing 
  open Format
  open Options
  open Util

  let java_keywords = 
    let h = Hashtbl.create 97 in 
    List.iter (fun s -> Hashtbl.add h s ()) 
      [ 
	"class"; "extends"; "this"; "super";
	"public"; "static"; "final"; "abstract"; "private";
	"throw"; "new"; "for"; "while"; "return"; "instanceof"; "if"; "else";
      ]; 
    h

  let is_keyword = Hashtbl.mem java_keywords  

  let java_types = 
    let h = Hashtbl.create 97 in 
    List.iter (fun s -> Hashtbl.add h s ()) 
      [ 
	"int"; "boolean"; "double"; "String"; "void";
      ]; 
    h

  let is_type = Hashtbl.mem java_types  

  let color () = is_set "color"

  let print_green_ident fmt s =
    fprintf fmt "{\\color{darkgreen}";
    print_ident fmt s;
    fprintf fmt "}"

  let ident fmt s =
    if is_keyword s then begin
      if color () then fprintf fmt "{\\color{blue}"
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

rule pp fmt = parse
  | '{'  { fprintf fmt "\\symbol{123}"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\symbol{125}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '$'  { fprintf fmt "\\${}"; pp fmt lexbuf }
  | ' '  { fprintf fmt "~"; pp fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\symbol{92}"; pp fmt lexbuf }
  | "--" { fprintf fmt "\\ensuremath{-{}-}"; pp fmt lexbuf }
  | '"' as c { pp_print_char fmt c; string fmt lexbuf; pp fmt lexbuf }
  | "/*" 
      { 
	fprintf fmt "{"; 
	if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt "/*"; 
	comment fmt lexbuf;
	fprintf fmt "}";
	pp fmt lexbuf 
      }
  | "//" 
      { 
	fprintf fmt "{"; 
	if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt "//"; 
	one_line_comment fmt lexbuf;
	start_of_line fmt lexbuf;
	pp fmt lexbuf 
      }
  | ident as s
      { ident fmt s; pp fmt lexbuf }
  | (ident as id1) (space+ as s) (ident as id2)
      { begin
	  if is_keyword id1 || is_keyword id2 || not (color ()) then begin
	    ident fmt id1; pp_print_string fmt s; ident fmt id2
	  end else begin
	    print_green_ident fmt id1; pp_print_string fmt s; ident fmt id2;
	  end
	end;
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

and one_line_comment fmt = parse
  | "\n" { fprintf fmt "}~\\linebreak" }
  | '\\' 
      { fprintf fmt "\\symbol{92}"; one_line_comment fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; one_line_comment fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; one_line_comment fmt lexbuf }
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

and comment fmt = parse
  | "*/" { fprintf fmt "*/" }
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak"; 
	indentation fmt (count_spaces s); 
	comment fmt lexbuf }
  | '\\' 
      { fprintf fmt "\\symbol{92}"; comment fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; comment fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; comment fmt lexbuf }
  | '$' { fprintf fmt "\\${}"; comment fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; comment fmt lexbuf }
  | '^' { fprintf fmt "\\^{}"; comment fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; comment fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; comment fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; comment fmt lexbuf }
  | "&" { fprintf fmt "\\&{}"; comment fmt lexbuf }
  | " " { fprintf fmt "~"; comment fmt lexbuf }
  | "\n" space* eof { fprintf fmt "}" }
  | eof  { fprintf fmt "}" }
  | _ as c { pp_print_char fmt c; comment fmt lexbuf }

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
  let java_tt fmt s =
    fprintf fmt "\\begin{flushleft}\\ttfamily\\parindent 0pt\n";
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{flushleft}%%\n"

  let java_lightblue_tt fmt s =
    fprintf fmt "\\colorbox{lightblue}{\\begin{minipage}{\\textwidth}\\tt\\parindent 0pt\n";
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{minipage}}\n"
 
  let () = Pp.add_pp_environment "java-lightblue-tt" java_lightblue_tt
  let () = Pp.add_pp_environment "java-tt" java_tt
  let () = Pp.add_pp_environment "java" java_lightblue_tt

}

