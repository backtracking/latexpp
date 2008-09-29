
(* MIPS preprocessor *)

{
  open Lexing 
  open Format
  open Options
  open Util

  let mips_keywords = 
    let h = Hashtbl.create 97 in 
    List.iter (fun s -> Hashtbl.add h s ()) 
      [ 
	"addi"; "addiu"; "sw"; "lw"; "move"; "not"; "and"; "li";
	"beqz"; "bnez"; "j"; "sub"; "sll"; "srl"; "jal"; "add"; "neg";
	"syscall"; "la"; "jr";
      ]; 
    h

  let is_keyword = Hashtbl.mem mips_keywords  

  let color () = is_set "color"

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule pp fmt = parse
  | '{'  { fprintf fmt "\\{"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  | ident as s ':' 
      { fprintf fmt "{\\color{red}"; print_ident fmt s;
	fprintf fmt "\\ensuremath{:\\,}}"; 
	pp fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '$'  { fprintf fmt "\\${}"; pp fmt lexbuf }
  | ' '  { fprintf fmt "~"; pp fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\ensuremath{\\backslash}"; pp fmt lexbuf }
  | "--" { fprintf fmt "\\ensuremath{-{}-}"; pp fmt lexbuf }
  | "#" 
      { 
	fprintf fmt "{"; 
	if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt "\\#{}"; 
	one_line_comment fmt lexbuf;
	start_of_line fmt lexbuf;
	pp fmt lexbuf 
      }
  | ('.' ident) as s
      { if color () then fprintf fmt "{\\color{violet}"
	else fprintf fmt "\\textbf{";
	print_ident fmt s;
	fprintf fmt "}";
	pp fmt lexbuf 
      }
  | ident as s
      { if is_keyword s then begin
	  if color () then fprintf fmt "{\\color{blue}"
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

and one_line_comment fmt = parse
  | "\n" { fprintf fmt "}~\\linebreak" }
  | '\\' 
      { fprintf fmt "\\ensuremath{\\backslash}"; one_line_comment fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; one_line_comment fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; one_line_comment fmt lexbuf }
  | '$' { fprintf fmt "\\${}"; one_line_comment fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; one_line_comment fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; one_line_comment fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; one_line_comment fmt lexbuf }
  | '~'  { fprintf fmt "\\~{}"; one_line_comment fmt lexbuf }
  | "&" { fprintf fmt "\\&{}"; one_line_comment fmt lexbuf }
  | " " { fprintf fmt "~"; one_line_comment fmt lexbuf }
  | "\n" space* eof { fprintf fmt "}" }
  | eof  { fprintf fmt "}" }
  | _ as c { pp_print_char fmt c; one_line_comment fmt lexbuf }

and start_of_line fmt = parse
  | space* as s
      { indentation fmt (count_spaces s) }
  | eof 
      { () }

{
  let mips_tt fmt s =
    fprintf fmt "\\begin{flushleft}\\ttfamily\\parindent 0pt\n";
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{flushleft}%%\n"

  let mips_lightblue_tt fmt s =
    fprintf fmt "\\colorbox{lightblue}{\\begin{minipage}{\\textwidth}\\tt\\parindent 0pt\n";
    let lb = from_string s in
    start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{minipage}}\n"
 
  let () = Pp.add_pp_environment "mips-lightblue-tt" mips_lightblue_tt
  let () = Pp.add_pp_environment "mips-tt" mips_tt
  let () = Pp.add_pp_environment "mips" mips_lightblue_tt

}

