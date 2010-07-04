
(* Why preprocessor *)

{
  open Lexing 
  open Format
  open Options

  let why_keywords = 
    let h = Hashtbl.create 97 in 
    List.iter (fun s -> Hashtbl.add h s ()) 
      [ 
	"logic"; "axiom"; "parameter"; "predicate"; "type"; "exception";
	"use"; "import"; "clone"; "export"; "namespace"; "as"; "theory";

	"if"; "then"; "else"; "while"; "do"; "done"; "let"; "in"; "rec";
	"assert"; "begin"; "end"; "ref"; "try"; "with"; "raise"; "and";
	"invariant"; "variant"; 
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
  let inside_annotation = ref false
  let reset () = inside_annotation := false

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule pp fmt = parse
  | '{'  { if color () then fprintf fmt "{\\color{darkgreen}";
	   inside_annotation := true;
	   fprintf fmt "\\{"; pp fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; 
	   inside_annotation := false;
	   if color () then fprintf fmt "}"; pp fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; pp fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; pp fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; pp fmt lexbuf }
  | ':'  { fprintf fmt "\\ensuremath{\\colon}"; pp fmt lexbuf }
  | '&'  { fprintf fmt "\\&{}"; pp fmt lexbuf }
  | '\\'  { fprintf fmt "\\ensuremath{\\backslash}"; pp fmt lexbuf }
  | "--" { fprintf fmt "\\ensuremath{-{}-}"; pp fmt lexbuf }

  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "forall" { fprintf fmt "\\ensuremath{\\forall}"; pp fmt lexbuf }
  | '$' ([^'$']* as s) '$' { fprintf fmt "\\ensuremath{_{%a}}" pp (from_string s); pp fmt lexbuf}

(*
  | "|" { fprintf fmt "\\ensuremath{|}"; pp fmt lexbuf }
  | ">" { fprintf fmt "\\ensuremath{>}"; pp fmt lexbuf }
  | "<" { fprintf fmt "\\ensuremath{<}"; pp fmt lexbuf }
  | '*'  { fprintf fmt "\\ensuremath{\\star}"; pp fmt lexbuf }
  | "->" { fprintf fmt "\\ensuremath{\\rightarrow}"; pp fmt lexbuf }
  | "<-" { fprintf fmt "\\ensuremath{\\leftarrow}"; pp fmt lexbuf }
  | ">=" { fprintf fmt "\\ensuremath{\\ge}"; pp fmt lexbuf }
  | "<=" { fprintf fmt "\\ensuremath{\\le}"; pp fmt lexbuf }
  | "&&" { fprintf fmt "\\ensuremath{\\land}"; pp fmt lexbuf }
  | "||" { fprintf fmt "\\ensuremath{\\lor}"; pp fmt lexbuf }
  | "==" { fprintf fmt "\\ensuremath{\\equiv}"; pp fmt lexbuf }
  | "!=" { fprintf fmt "\\ensuremath{\\not\\equiv}"; pp fmt lexbuf }
*)
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
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak"; indentation fmt (count_spaces s);
	comment fmt lexbuf }
  | '\\'  { fprintf fmt "\\ensuremath{\\backslash}"; comment fmt lexbuf }
  | '{'  { fprintf fmt "\\{"; comment fmt lexbuf }
  | '}'  { fprintf fmt "\\}"; comment fmt lexbuf }
  | '#' { fprintf fmt "\\#{}"; comment fmt lexbuf }
  | '_'  { fprintf fmt "\\_{}"; comment fmt lexbuf }
  | '%'  { fprintf fmt "\\%%{}"; comment fmt lexbuf }
  | "&" { fprintf fmt "\\&{}"; comment fmt lexbuf }
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
 
  let why_tt fmt s =
    fprintf fmt "\\begin{flushleft}\\ttfamily\\parindent 0pt\n";
    let lb = from_string s in
    reset (); start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{flushleft}%%\n"
 
  let why_sf fmt s =
    fprintf fmt "\\bgroup\\sf\\begin{flushleft}\n";
    let lb = from_string s in
    reset (); start_of_line fmt lb; pp fmt lb;
    fprintf fmt "\\end{flushleft}\\egroup\\noindent\n"
 
  let () = Pp.add_pp_environment "why-alltt" why_alltt
  let () = Pp.add_pp_environment "why-tt" why_tt
  let () = Pp.add_pp_environment "why-sf" why_sf
  let () = Pp.add_pp_environment "why" why_tt

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

