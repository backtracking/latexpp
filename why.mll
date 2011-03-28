
(* Why preprocessor *)

{
  open Lexing 
  open Format
  open Options
  open Util

  let why_keywords = 
    let h = Hashtbl.create 97 in 
    List.iter (fun s -> Hashtbl.add h s ()) 
      [ 
	"logic"; "axiom"; "parameter"; "predicate"; "type"; "exception";
	"use"; "import"; "clone"; "export"; "namespace"; "as"; "theory";
	"goal";

	"if"; "then"; "else"; "while"; "do"; "done"; "let"; "in"; "rec";
	"assert"; "begin"; "end"; "ref"; "try"; "with"; "raise"; "and";
	"invariant"; "variant"; "match";
      ]; 
    h

  let is_keyword = Hashtbl.mem why_keywords  

  let tab_size = 8

  let substitutions = Hashtbl.create 20
  let is_substituted = Hashtbl.mem substitutions

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
  let is_tt = ref true
  let inside_annotation = ref false
  let reset () = inside_annotation := false

}

let space = [' ' '\t']
let latex_symbol = 
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^' 
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule pp fmt = parse
  | latex_symbol as c 
         { fprintf fmt "\\symbol{%d}" (Char.code c); pp fmt lexbuf }
  | ' '  { pp_print_string fmt "\\hspace*{1.22ex}"; pp fmt lexbuf }
  | '{'  { if color () then fprintf fmt "{\\color{colorspec}";
	   inside_annotation := true;
	   fprintf fmt "\\{"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; 
	   inside_annotation := false;
	   if color () then fprintf fmt "}"; pp fmt lexbuf }
  | "--" { fprintf fmt "\\ensuremath{-{}-}"; pp fmt lexbuf }

  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "<->" { fprintf fmt "\\ensuremath{\\leftrightarrow}"; pp fmt lexbuf }
  | "forall" { fprintf fmt "\\ensuremath{\\forall}"; pp fmt lexbuf }
  | "exists" { fprintf fmt "\\ensuremath{\\exists}"; pp fmt lexbuf }
  | "and" { fprintf fmt "\\ensuremath{\\land}"; pp fmt lexbuf }
  | "or" { fprintf fmt "\\ensuremath{\\lor}"; pp fmt lexbuf }
  | "(*" 
      { 
	fprintf fmt "\\emph{"; 
	if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt "(*"; comment fmt lexbuf; 
	fprintf fmt "}"; 
	pp fmt lexbuf 
      }
  | ident as s
      { 
	if is_keyword s && not !inside_annotation then begin
	  if color () then fprintf fmt "{\\color{blue}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
            
	end else if is_substituted s then begin
          pp_print_string fmt (Hashtbl.find substitutions s)
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

and comment fmt = parse
  | "(*" as s 
      { pp_print_string fmt s; comment fmt lexbuf; comment fmt lexbuf }
  | "*)" as s
      { pp_print_string fmt s }
  | latex_symbol as c 
      { fprintf fmt "\\symbol{%d}" (Char.code c); comment fmt lexbuf }
  | ' '  
      { pp_print_string fmt "\\hspace*{1.22ex}"; comment fmt lexbuf }
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak"; indentation fmt (count_spaces s);
	comment fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; comment fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; comment fmt lexbuf }
  | eof  
      { () }
  | _ as c  
      { pp_print_char fmt c; comment fmt lexbuf }

and start_of_line fmt = parse
  | space* as s
      { indentation fmt (count_spaces s) }
  | eof 
      { () }


{
  let why_alltt fmt s =
    fprintf fmt "\\begin{alltt}";
    let lb = from_string s in
    reset (); start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{alltt}%%\n"

  let () = Pp.add_pp_environment "why-alltt" why_alltt
 
  let why fmt s = 
    let lb = from_string s in reset (); start_of_line fmt lb; pp fmt lb
    
  let why_tt = noindent_tt why

  let () = Pp.add_pp_environment "why-tt" why_tt
  let () = Pp.add_pp_environment "why" why_tt
 
  let why_sf =
    noindent_sf (fun fmt s -> is_tt := false; why fmt s; is_tt := true)

  let () = Pp.add_pp_environment "why-sf" why_sf

  let texttt fmt s =
    fprintf fmt "\\texttt{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let textsf fmt s =
    fprintf fmt "\\textsf{";
    pp fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "why-tt" texttt
  let () = Pp.add_pp_macro "why-sf" textsf
  let () = Pp.add_pp_macro "why" texttt

  let () = Pp.add_pp_envsopts "why"
    (fun error opts -> 
       match opts with
         | [] -> assert false
         | ["subst";v1;v2] -> Hashtbl.add substitutions v1 v2
         | s::_ -> error ("envopts why : Unknown option "^s))
}

