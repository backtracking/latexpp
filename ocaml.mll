
(* Ocaml preprocessor *)

{
  open Lexing 
  open Format
  open Options

  let ocaml_keywords = 
    let h = Hashtbl.create 97 in 
    List.iter (fun s -> Hashtbl.add h s ()) 
      [ 
	"and"; "as"; "assert"; "asr"; "begin";
	"class"; "constraint"; "do"; "done"; "downto"; "else"; "end";
	"exception"; "external"; "false"; "for"; "fun"; "function";
	"functor"; "if"; "in"; "include"; "inherit"; "initializer";
	"land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match";
	"method"; "mod"; "module"; "mutable"; "new"; "object"; "of";
	"open"; "or"; "private"; "rec"; "sig"; "struct"; "then"; "to";
	"true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with";
      ]; 
    h

  let is_keyword = Hashtbl.mem ocaml_keywords  

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule pp fmt = parse
  | '{'  { fprintf fmt "\\{"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; pp fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\ensuremath{\\backslash}"; pp fmt lexbuf }
  | '\n' { fprintf fmt "\n"; pp fmt lexbuf }
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "<-" { fprintf fmt "\\ensuremath{\\leftarrow}"; pp fmt lexbuf }
  | "=>" { fprintf fmt "\\ensuremath{\\Rightarrow}"; pp fmt lexbuf }
  | "<->" { fprintf fmt "\\ensuremath{\\leftrightarrow}"; pp fmt lexbuf }
  | "\\emph{" [^'}']* '}' 
      { pp_print_string fmt (lexeme lexbuf); pp fmt lexbuf }
  | "(*" [^'\n']* "*)" as s
      { fprintf fmt "\\emph{"; pp_print_string fmt s; fprintf fmt "}"; 
	pp fmt lexbuf }
  | eof  { () }
  | "'a" { fprintf fmt "\\ensuremath{\\alpha}"; pp fmt lexbuf }
  | "*"  { fprintf fmt "\\ensuremath{\\times}"; pp fmt lexbuf }
  | ident as s
	{ if is_keyword s then begin
	    if color then fprintf fmt "{\\color{blue}"
	    else fprintf fmt "\\textbf{";
	    pp_print_string fmt s;
	    fprintf fmt "}"
	  end else 
            pp_print_string fmt s;
	  pp fmt lexbuf 
	}
  | _ as c  { pp_print_char fmt c; pp fmt lexbuf }

{
  let ocaml_alltt fmt s =
    fprintf fmt "\\begin{alltt}\n";
    pp fmt (from_string s);
    fprintf fmt "\\end{alltt}\n"
 
  let () = Pp.add_pp_environment "ocaml" ocaml_alltt
}

