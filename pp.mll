
(* main scanner *)

{
  open Lexing 
  open Format
  open Options

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let out = ref std_formatter
  let set_out_formatter = (:=) out
  let print_string s = pp_print_string !out s
  let print_char c = pp_print_char !out c

  let buffer = Buffer.create 1024

  let ppenvs = Hashtbl.create 17
  let add_pp_environment = Hashtbl.add ppenvs
  let remove_environment = Hashtbl.remove ppenvs

  let map_environment e pp = 
    try
      Hashtbl.add ppenvs e (Hashtbl.find ppenvs pp)
    with Not_found ->
      eprintf "latexpp: unknown preprocesseur `%s'@." pp; 
      exit 1

  let ppmacros = Hashtbl.create 17
  let add_pp_macro = Hashtbl.add ppmacros
  let remove_macro = Hashtbl.remove ppmacros

  let map_macro m pp = 
    try
      Hashtbl.add ppmacros m (Hashtbl.find ppmacros pp)
    with Not_found ->
      eprintf "latexpp: unknown preprocesseur `%s'@." pp; 
      exit 1

  let error l msg =
    eprintf "%s:%d: %s@." l.pos_fname l.pos_lnum msg; 
    exit 1 

}

let space = [' ' '\t' '\r']
let newline = '\n'
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '-']* 

rule pp = parse
  | "%latexpp" space+ "-g" space+ (ident as o) space+ (ident as v) space* '\n'
      { 
	Options.add o v; newline lexbuf; pp lexbuf
      }
  | "%latexpp" space+ "-g" space+ (ident as o) space+ 
    '"' ([^ '"' '\n']* as v) '"' space* '\n'
      { 
	Options.add o v; newline lexbuf; pp lexbuf
      }
  | "%latexpp" space+ ("-e" | "-m" as o) 
    space+ (ident as id1) space+ (ident as id2) space* '\n'
      {
	(if o = "-e" then map_environment else map_macro) id1 id2;
	newline lexbuf;
	pp lexbuf
      }
  | "%" [^ '\n']* '\n'
      { 
	print_string (lexeme lexbuf);
	newline lexbuf;
	pp lexbuf 
      }
  | "\\begin{" (ident as id) "}" ('['? as o) space* '\n'?
      { 
	if Hashtbl.mem ppenvs id then begin
	  Buffer.reset buffer;
	  let e = id, lexeme_start_p lexbuf in
	  let ol = match o with "" -> [] | _ -> options (snd e) lexbuf in
	  environment e true lexbuf;
  	  let ppf = Hashtbl.find ppenvs id in
	  with_options ol (ppf !out) (Buffer.contents buffer)
	end else 
	  print_string (lexeme lexbuf);
	pp lexbuf 
      }
  | "\\" (ident as id) "{"
      { 
	if Hashtbl.mem ppmacros id then begin
	  Buffer.reset buffer;
	  let l = lexeme_start_p lexbuf in
	  macro l lexbuf;
	  let ppf = Hashtbl.find ppmacros id in
	  ppf !out (Buffer.contents buffer)
	end else 
	  print_string (lexeme lexbuf);
	pp lexbuf 
      }
  | eof 
      { () }
  | _ as c
      { 
	if c = '\n' then newline lexbuf;
	print_char c; pp lexbuf 
      }

and environment e stop = parse
  | "\\end{" (ident as id) "}" space* (newline? as nl)
      { 
	if id = fst e then begin 
	  if not stop then Buffer.add_string buffer (lexeme lexbuf)
	end else begin
	  let l = lexeme_start_p lexbuf in
	  eprintf "%s:%d: should close environment %s, not %s@." 
	    l.pos_fname l.pos_lnum (fst e) id;
	  exit 1
	end;
	if nl <> "" then newline lexbuf
      }
  | "\\begin{" (ident as id) "}"
      { 
	Buffer.add_string buffer (lexeme lexbuf);
	let e' = id, lexeme_start_p lexbuf in
	environment e' false lexbuf;
	environment e stop lexbuf 
      }
  | _ as c
      { 
	if c = '\n' then newline lexbuf;
	Buffer.add_char buffer c; environment e stop lexbuf 
      }
  | eof
      { error (snd e) "unterminated environment" }

and macro l = parse 
  | eof
      { 
	eprintf "%s:%d: unclosed brace@." l.pos_fname l.pos_lnum; 
	exit 1 
      }
  | "}"
      { () }
  | "{"
      { 
	Buffer.add_char buffer '{'; macro (lexeme_start_p lexbuf) lexbuf;
	Buffer.add_char buffer '}'; macro l lexbuf 
      }
  | _ as c
      { 
	if c = '\n' then newline lexbuf;
	Buffer.add_char buffer c; macro l lexbuf 
      }

and options l = parse
  | ']' space* '\n'?
      { [] }
  | space+
      { options l lexbuf }
  | newline
      { newline lexbuf; options l lexbuf }
  | (ident as id) space* '=' space* 
      { 
	let v = value l id lexbuf in
	(id, v) :: options l lexbuf
      }
  | eof
      { error l "unterminated options" }
  | _
      { error l "syntax error in options" }

and value l o = parse
  | '"' ([^ '"']* as v) '"'
      { v }
  | [^ ']' ' ' '\r' '\t' '\n']+ as v
      { v }
  | eof
      { error l "unterminated options" }
  | _
      { error l ("syntax error in value for option " ^ o) }

{

  (* some predefined environments *)

  let () = 
    add_pp_environment "verbatim" 
      (fun fmt s -> fprintf fmt "\\begin{verbatim}%s\\end{verbatim}%%\n" s)

  let () = 
    add_pp_environment "alltt" 
      (fun fmt s -> fprintf fmt "\\begin{alltt}%s\\end{alltt}%%\n" s)

  let () = 
    add_pp_environment "copy" pp_print_string

  let () = 
    add_pp_environment "ignore" (fun _ _ -> ())

  (* some predefined macros *)

  let () = 
    add_pp_macro "copy" pp_print_string

  let () = 
    add_pp_macro "ignore" (fun _ _ -> ())

}
