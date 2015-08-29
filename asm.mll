
(* Assembly preprocessor *)

{
  open Lexing
  open Format
  open Options
  open Util

  let mips_keyword = make_table
    [
      "addi"; "addiu"; "sw"; "lw"; "move"; "not"; "and"; "andi"; "li"; "lui";
      "beq"; "beqz"; "bnez"; "j"; "sub"; "sll"; "sllv"; "srl";
      "sra"; "srlv"; "srav"; "jal"; "jalr"; "add"; "neg"; "mul";
      "syscall"; "la"; "jr"; "abs"; "or"; "ori"; "ble"; "bge"; "b";
      "rol"; "ror"; "seq"; "slt"; "slti"; "sltu"; "sltiu";
      "nop";
    ]

  let x86_keyword = make_table
    [ "mov"; "movl"; "movq"; "call"; "ret"; "syscall"; "rep";
      "xorb"; "andq"; "orq"; "notq"; "shlq"; "shrq";
      "addq"; "subq";
      "testq"; "jz"; "jmp";
      ]

  type asm = Mips | X86
  let asm = ref Mips
  let is_keyword x = match !asm with
    | Mips -> mips_keyword x
    | X86  -> x86_keyword  x

  let color () = is_set "color"
}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let latex_symbol =
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^' | '{' | '}'

rule pp fmt = parse
  | "#"
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{red}";
	pp_print_string fmt "\\symbol{35}";
	one_line_comment fmt lexbuf;
	start_of_line fmt lexbuf;
	pp fmt lexbuf
      }
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c); pp fmt lexbuf }
  | ('.'? ident) as s ':'
      { fprintf fmt "{\\color{red}"; print_ident fmt s;
	fprintf fmt "\\symbol{58}}";
	pp fmt lexbuf }
  | ' '  { fprintf fmt "~"; pp fmt lexbuf }
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
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c);
	   one_line_comment fmt lexbuf }
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

  (* MIPS *)

  let mips fmt s =
    asm := Mips;
    let lb = from_string s in start_of_line fmt lb; pp fmt lb

  let mips_tt = noindent_tt mips
  let () = Pp.add_pp_environment "mips-tt" mips_tt

  let mips_lightblue_tt = lightblue_box_tt mips
  let () = Pp.add_pp_environment "mips-lightblue-tt" mips_lightblue_tt
  let () = Pp.add_pp_environment "mips" mips_lightblue_tt

  (* X86 *)

  let x86 fmt s =
    asm := X86;
    let lb = from_string s in start_of_line fmt lb; pp fmt lb

  let x86_tt = noindent_tt x86
  let () = Pp.add_pp_environment "x86-tt" x86_tt

  let x86_lightblue_tt = lightblue_box_tt x86
  let () = Pp.add_pp_environment "x86-lightblue-tt" x86_lightblue_tt
  let () = Pp.add_pp_environment "x86" x86_lightblue_tt
  let () = Pp.add_pp_environment "x86-lightgray-tt" (lightgray_box_tt x86)
}

