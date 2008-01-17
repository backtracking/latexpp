
(* main scanner *)

{
  open Lexing 
  open Format

  let out = ref std_formatter
  let print_string s = pp_print_string !out s
  let print_char c = pp_print_char !out c

  let buffer = Buffer.create 1024

  let ppenvs = Hashtbl.create 17
  let add_pp_environment = Hashtbl.add ppenvs
}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* 

rule pp = parse
  | "\\begin{" (ident as e) "}"
      { 
	if Hashtbl.mem ppenvs e then begin
	  Buffer.reset buffer;
	  cut_env ((=) e) lexbuf;
  	  let ppf = Hashtbl.find ppenvs e in
	  ppf !out (Buffer.contents buffer)
	end else 
	  print_string (lexeme lexbuf);
	pp lexbuf 
      }
  | eof 
      { () }
  | _ as c
      { print_char c; pp lexbuf }

and cut_env p = parse
  | "\\end{" (ident as e') "}"
      { 
	if not (p e') then begin 
	  print_string (lexeme lexbuf); cut_env p lexbuf 
        end 
      }
  | "\\begin{" ident "}"
      { 
	cut_env (fun _ -> true) lexbuf;
	cut_env p lexbuf 
      }
  | _ as c
      { Buffer.add_char buffer c; cut_env p lexbuf }
  | eof
      { eprintf "unterminated environment %s@." e; exit 1 }

{
}
