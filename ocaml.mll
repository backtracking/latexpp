
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
    fprintf fmt "\n\\noindent\\kern%2.2fem" space

  let print_ident fmt =
    let char = function
      | '_' -> pp_print_string fmt "\\_{}"
      | c -> pp_print_char fmt c
    in
    String.iter char

  let color () = is_set "color"

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
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "<-" { fprintf fmt "\\ensuremath{\\leftarrow}"; pp fmt lexbuf }
  | ">=" { fprintf fmt "\\ensuremath{\\ge}"; pp fmt lexbuf }
  | "<=" { fprintf fmt "\\ensuremath{\\le}"; pp fmt lexbuf }
  | "&&" { fprintf fmt "\\ensuremath{\\land}"; pp fmt lexbuf }
  | "||" { fprintf fmt "\\ensuremath{\\lor}"; pp fmt lexbuf }
  | "==" { fprintf fmt "\\ensuremath{\\equiv}"; pp fmt lexbuf }
  | "!=" { fprintf fmt "\\ensuremath{\\not\\equiv}"; pp fmt lexbuf }
  | "<>" { fprintf fmt "\\ensuremath{\\not=}"; pp fmt lexbuf }
  | "'a" { fprintf fmt "\\ensuremath{\\alpha}"; pp fmt lexbuf }
  | "*"  { fprintf fmt "\\ensuremath{\\times}"; pp fmt lexbuf }
  | "(*" [^'\n']* "*)" as s
      { fprintf fmt "\\emph{"; if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt s; fprintf fmt "}"; 
	pp fmt lexbuf }
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
      { indentation fmt (count_spaces s);
	pp fmt lexbuf 
      }
  | "\n" space* eof
      { pp_print_string fmt "\n" }
  | eof  
      { () }
  | _ as c  
      { pp_print_char fmt c; pp fmt lexbuf }

{
  let ocaml_alltt fmt s =
    fprintf fmt "\\begin{alltt}";
    pp fmt (from_string s);
    fprintf fmt "\\end{alltt}%%\n"
 
  let ocaml_sf fmt s =
    fprintf fmt "\\bgroup\\begin{obeylines}\\sf\\medskip\n";
    pp fmt (from_string s);
    fprintf fmt "\\end{obeylines}\\medskip\\egroup\\noindent\n"
 
  let () = Pp.add_pp_environment "ocaml-tt" ocaml_alltt
  let () = Pp.add_pp_environment "ocaml-sf" ocaml_sf
  let () = Pp.add_pp_environment "ocaml" ocaml_sf

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

