
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

