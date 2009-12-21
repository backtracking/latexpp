
(* Math preprocessor *)

{
  open Lexing 
  open Format
  open Options
  open Util

  type t = 
    | Empty 
    | Rule of int * string 
    | Other of int * string 
    | Frac of int * t * t * string

  let length = function
    | Empty -> 0
    | Rule (n, _) | Other (n, _) | Frac (n, _, _, _) -> n

}

let space = [' ' '\t']
let latex_symbol = 
  '#' | '$' | ':' | '%' | ';' | '&'
let ident = ['a'-'z' 'A'-'Z']+

let greek =
  "alpha" | "beta" | "gamma" | "delta" | "epsilon" | "varepsilon" 
| "zeta" | "eta" | "theta" | "vartheta" | "iota" | "kappa" | "lambda" 
| "mu" | "nu" | "xi" | "pi" | "varpi" 
| "rho" | "varrho" | "sigma" | "varsigma" | "tau" | "upsilon" | "phi" 
| "varphi" | "chi" | "psi" | "omega" 
| "Gamma" | "Delta" | "Theta" | "Lambda" | "Xi" | "Pi" | "Sigma" 
| "Upsilon" | "Phi" | "Psi" | "Omega"

rule math fmt = parse
  | latex_symbol as c 
         { fprintf fmt "\\symbol{%d}" (Char.code c); math fmt lexbuf }
  (* arrows *)
  | "->" { fprintf fmt "\\rightarrow "; math fmt lexbuf }
  | "-->" { fprintf fmt "\\longrightarrow "; math fmt lexbuf }
  | "=>" { fprintf fmt "\\Rightarrow "; math fmt lexbuf }
  | "==>" { fprintf fmt "\\Longrightarrow "; math fmt lexbuf }
  | "<-" { fprintf fmt "\\leftarrow "; math fmt lexbuf }
  | "<--" { fprintf fmt "\\longleftarrow "; math fmt lexbuf }
  | "<=" { fprintf fmt "\\Leftarrow "; math fmt lexbuf }
  | "<=" { fprintf fmt "\\Longleftarrow "; math fmt lexbuf }
  | "<->" { fprintf fmt "\\leftrightarrow "; math fmt lexbuf }
  | "<=>" { fprintf fmt "\\Leftrightarrow "; math fmt lexbuf }
  | "<==>" { fprintf fmt "\\Longleftrightarrow "; math fmt lexbuf }
  | "|->" { fprintf fmt "\\mapsto "; math fmt lexbuf }
  (* arith *)
  | ">=" { fprintf fmt "\\ge "; math fmt lexbuf }
  | "<=" { fprintf fmt "\\le "; math fmt lexbuf }
  (* logic *)
  | "|-" { fprintf fmt "{\\vdash}"; math fmt lexbuf }
  | "|=" { fprintf fmt "\\models "; math fmt lexbuf }
  | "forall" { fprintf fmt "\\forall "; math fmt lexbuf }
  | "exists" { fprintf fmt "\\exists "; math fmt lexbuf }

  | greek as s 
      { fprintf fmt "\\%s " s; math fmt lexbuf }
  | ident as s 
      { pp_print_string fmt s; math fmt lexbuf }
  | eof  
      { () }
  | _ as c  
      { pp_print_char fmt c; math fmt lexbuf }

and cut_lines i = parse
  | eof 
      { [] }
  | ' '* (("--" '-'+) as r) ([^ '\n']* as s) '\n'
      { let n = String.length r in
	Rule (n, s) :: cut_lines (i + n) lexbuf }
  | ' '* ([^ '\n']+ as s) '\n'
      { let n = String.length s in
	Other (n, s) :: cut_lines (i + n) lexbuf }

{
  let m_math fmt s =
    fprintf fmt "\\ensuremath{";
    math fmt (from_string s);
    fprintf fmt "}"

  let () = Pp.add_pp_macro "math" m_math

  let e_math fmt s =
    fprintf fmt "\\begin{displaymath}@\n";
    math fmt (from_string s);
    fprintf fmt "\\end{displaymath}"

  let () = Pp.add_pp_environment "math" e_math

  let e_rules fmt s =
    let ll = cut_lines 0 (from_string s) in
    let rec add t l = match t, l with
      | t, Empty | Empty, t -> 
	  t
      | Other (i1, s1), Other (i2, s2) -> 
	  Other (max i1 i2, s1 ^ "\n" ^ s2)
      | t1, Rule (n2, s2) when n2 >= length t1 -> 
	  Frac (n2, t1, Empty, s2)
      | Frac (n1, t1, b1, s1), l2 when length l2 < n1 -> 
	  Frac (n1, t1, add b1 l2, s1)
      | _ -> 
	  Other (4, "latexpp: TODO")
    in
    let t = List.fold_left add Empty ll in
    let rec draw fmt = function
      | Empty ->
	  ()
      | Other (_, s) ->
	  math fmt (from_string s)
      | Frac (_, t1, t2, s) ->
	  fprintf fmt "\\frac{\\displaystyle %a}{\\displaystyle %a}%a"
	    draw t1 draw t2 math (from_string s)
      | Rule _ ->
	  pp_print_string fmt "\\hrulefill"
    in
    draw fmt t

  let () = Pp.add_pp_environment "rules" e_rules
}

