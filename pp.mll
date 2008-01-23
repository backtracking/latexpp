
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

  let map_environment (e,pp) = 
    try
      Hashtbl.add ppenvs e (Hashtbl.find ppenvs pp)
    with Not_found ->
      eprintf "latexpp: unknown preprocesseur `%s'@." pp; 
      exit 1

  let ppmacros = Hashtbl.create 17
  let add_pp_macro = Hashtbl.add ppmacros

  let map_macro (m,pp) = 
    try
      Hashtbl.add ppmacros m (Hashtbl.find ppmacros pp)
    with Not_found ->
      eprintf "latexpp: unknown preprocesseur `%s'@." pp; 
      exit 1
}

let space = [' ' '\t' '\r']
let newline = '\n'
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule pp = parse
  | "\\begin{" (ident as id) "}" space* (newline? as nl)
      { 
	if nl <> "" then newline lexbuf;
	if Hashtbl.mem ppenvs id then begin
	  Buffer.reset buffer;
	  let e = id, lexeme_start_p lexbuf in
	  environment e true lexbuf;
  	  let ppf = Hashtbl.find ppenvs id in
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
      { 
	let id, l = e in
	eprintf 
	  "%s:%d: unterminated environment %s@." l.pos_fname l.pos_lnum id; 
	exit 1 
      }

{
}
