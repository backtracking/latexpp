
open Format

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
  fprintf fmt "\n\\noindent\\hspace*{%2.2fem}" space

let print_ident fmt =
  let char = function
    | '_' -> pp_print_string fmt "\\_{}"
    | c -> pp_print_char fmt c
  in
  String.iter char

let colorname_box_tt color pp fmt s =
  fprintf fmt "\\colorbox{%s}{\\begin{minipage}{\\textwidth}\\tt\\parindent 0pt\n" color;
  pp fmt s;
  fprintf fmt "\\end{minipage}}\n"

let rgbcolor_box_tt r g b pp fmt s =
  fprintf fmt "{\\definecolor{tmpcolor}{rgb}{%.2f,%.2f,%.2f}\\colorbox{tmpcolor}{\\begin{minipage}{\\textwidth}\\tt\\parindent 0pt\n" r g b;
  pp fmt s;
  fprintf fmt "\\end{minipage}}}\n"

let lightgreen_box_tt pp = rgbcolor_box_tt 0.6 1.0 0.6 pp
let lightblue_box_tt pp = rgbcolor_box_tt 0.8 0.8 1.0 pp
let lightred_box_tt pp = rgbcolor_box_tt 1.0 0.8 0.8 pp

