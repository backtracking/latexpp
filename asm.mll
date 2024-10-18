
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
    [ "mov"; "movb"; "movw"; "movl"; "movq";
      "movs"; "movsbw"; "movsbl"; "movsbq"; "movswl"; "movswq"; "movslq";
      "movz"; "movzbw"; "movzbl"; "movzbq"; "movzwl"; "movzwq"; "movzlq";
      "movabsq";
      "pushq"; "popq";
      "xor"; "xorb"; "xorw"; "xorl"; "xorq";
      "or"; "orb"; "orw"; "orl"; "orq";
      "and"; "andb"; "andw"; "andl"; "andq";
      "not"; "notb"; "notw"; "notl"; "notq";
      "sal"; "salb"; "salw"; "sall"; "salq";
      "sar"; "sarb"; "sarw"; "sarl"; "sarq";
      "shr"; "shrb"; "shrw"; "shrl"; "shrq";
      "rol"; "rolb"; "rolw"; "roll"; "rolq";
      "ror"; "rorb"; "rorw"; "rorl"; "rorq";
      "add"; "addb"; "addw"; "addl"; "addq";
      "inc"; "incb"; "incw"; "incl"; "incq";
      "dec"; "decb"; "decw"; "decl"; "decq";
      "sub"; "subb"; "subw"; "subl"; "subq";
      "imul"; "imulb"; "imulw"; "imull"; "imulq";
      "neg"; "negb"; "negw"; "negl"; "negq";
      "lea"; "leab"; "leaw"; "leal"; "leaq";
      "divq"; "idivq"; "cltd"; "cqto";
      "call"; "ret"; "syscall"; "rep"; "leave";
      "cmp"; "cmpb"; "cmpw"; "cmpl"; "cmpq";
      "test"; "testb"; "testw"; "testl"; "testq";
      "je"; "jz"; "jne"; "jnz"; "js"; "jns"; "jg"; "jge";
      "jl"; "jle"; "ja"; "jae"; "jb"; "jbe";
      "jmp";
      "sete"; "setz"; "setne"; "setnz"; "sets"; "setns"; "setg"; "setge";
      "setl"; "setle"; "seta"; "setae"; "setb"; "setbe";
      "cmove"; "cmovz"; "cmovne"; "cmovnz";
      "cmovs"; "cmovns"; "cmovg"; "cmovge";
      "cmovl"; "cmovle"; "cmova"; "cmovae"; "cmovb"; "cmovbe";
      "cvttss2sil";
    ]

  let llvm_keyword = make_table
    [ "define"; "icmp"; "br"; "phi"; "add"; "mul"; "ret";
      "call"; "tail"; ]
  let llvm_type = make_table
    [ "i1"; "i32"; "i64"; "label"]

  type asm = Mips | X86 | LLVM
  let asm = ref Mips
  let is_keyword x = match !asm with
    | Mips -> mips_keyword x
    | X86  -> x86_keyword  x
    | LLVM  -> llvm_keyword  x
  let is_type x = match !asm with
    | Mips | X86 -> false
    | LLVM  -> llvm_type  x

  let color () = is_set "color"

}

let space = [' ' '\t']
let ident = ['a'-'z' 'A'-'Z' '_' '0'-'9']+
let latex_symbol =
  '\\' | '#' | '$' | ':' | '_' | '%' | '~' | ';' | '&' | '^' | '{' | '}'

rule pp fmt = parse
  | "#" | ";" as s
      {
	fprintf fmt "{";
	if color () then fprintf fmt "\\color{asmcomment}";
	pp_print_string fmt (if s = '#' then "\\symbol{35}" else ";");
	one_line_comment fmt lexbuf;
	start_of_line fmt lexbuf;
	pp fmt lexbuf
      }
  | latex_symbol as c
         { fprintf fmt "\\symbol{%d}" (Char.code c); pp fmt lexbuf }
  | ('.'? ident) as s ':'
      { fprintf fmt "{\\color{asmlabel}"; print_ident fmt s;
	fprintf fmt "\\symbol{58}}";
	pp fmt lexbuf }
  | ' '  { fprintf fmt "~"; pp fmt lexbuf }
  | ('.' ident) as s
      { if color () then fprintf fmt "{\\color{asmdirective}"
	else fprintf fmt "\\textbf{";
	print_ident fmt s;
	fprintf fmt "}";
	pp fmt lexbuf
      }
  | ident as s
      { if is_keyword s then begin
	  if color () then fprintf fmt "{\\color{asmkeyword}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else if is_type s then begin
	  if color () then fprintf fmt "{\\color{asmtype}"
	  else fprintf fmt "\\textbf{";
	  pp_print_string fmt s;
	  fprintf fmt "}"
	end else
          print_ident fmt s;
	pp fmt lexbuf
      }
  | "\n" (space* as s)
      { fprintf fmt "~\\linebreak"; newline fmt;
	indentation fmt (count_spaces s);
	pp fmt lexbuf }
  | "\n" space* eof
      { pp_print_string fmt "\n" }
  | eof
      { () }
  | _ as c
      { pp_print_char fmt c; pp fmt lexbuf }

and one_line_comment fmt = parse
  | "\n" { fprintf fmt "}~\\linebreak"; newline fmt }
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

  let start fmt lb =
    newline fmt; start_of_line fmt lb; pp fmt lb; reset_line_number ()

  let mips fmt s =
    asm := Mips;
    start fmt (from_string s)

  let mips_tt = noindent_tt mips
  let () = Pp.add_pp_environment "mips-tt" mips_tt

  let mips_lightblue_tt = lightblue_box_tt mips
  let () = Pp.add_pp_environment "mips-lightblue-tt" mips_lightblue_tt
  let () = Pp.add_pp_environment "mips" mips_lightblue_tt

  (* X86 *)

  let x86 fmt s =
    asm := X86;
    start fmt (from_string s)

  let x86_tt = noindent_tt x86
  let () = Pp.add_pp_environment "x86-tt" x86_tt

  let x86_lightblue_tt = lightblue_box_tt x86
  let () = Pp.add_pp_environment "x86-lightblue-tt" x86_lightblue_tt
  let () = Pp.add_pp_environment "x86" x86_lightblue_tt
  let () = Pp.add_pp_environment "x86-lightgray-tt" (lightgray_box_tt x86)

  (* LLVM *)

  let llvm fmt s =
    asm := LLVM;
    start fmt (from_string s)

  let llvm_tt = noindent_tt llvm
  let () = Pp.add_pp_environment "llvm-tt" llvm_tt

  let llvm_lightblue_tt = lightblue_box_tt llvm
  let () = Pp.add_pp_environment "llvm-lightblue-tt" llvm_lightblue_tt
  let () = Pp.add_pp_environment "llvm" llvm_lightblue_tt
  let () = Pp.add_pp_environment "llvm-lightgray-tt" (lightgray_box_tt llvm)
}

