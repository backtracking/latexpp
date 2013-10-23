
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

let print_ident ?(tt=false) fmt =
  let char = function
    | '_' when tt -> pp_print_string fmt "\\symbol{95}"
    | '_' -> pp_print_string fmt "\\_{}"
    | c -> pp_print_char fmt c
  in
  String.iter char

let vspacing () = Options.find "vspacing"

let vspacing_before () = match Options.find "vspacing_before" with
  | None -> vspacing ()
  | Some _ as s -> s

let vspacing_after () = match Options.find "vspacing_after" with
  | None -> vspacing ()
  | Some _ as s -> s

let with_vspacing pp fmt s = 
  let print_sep = function
    | None -> ()
    | Some v -> fprintf fmt "\n\n%s\n" v
  in
  print_sep (vspacing_before ());
  pp fmt s;
  print_sep (vspacing_after ())

let colorname_box_tt color pp fmt s =
  fprintf fmt 
    "\\colorbox{%s}{\\begin{minipage}{\\textwidth}\\ttfamily\\parindent 0pt\n" 
    color;
  pp fmt s;
  fprintf fmt "\\end{minipage}}\n"

let rgbcolor_box_tt r g b pp fmt s =
  fprintf fmt "{\\definecolor{tmpcolor}{rgb}{%.2f,%.2f,%.2f}\\colorbox{tmpcolor}{\\begin{minipage}{\\textwidth}\\ttfamily\\parindent 0pt\n" r g b;
  pp fmt s;
  fprintf fmt "\\end{minipage}}}\n" 

let lightgreen_box_tt pp = with_vspacing (rgbcolor_box_tt 0.6 1.0 0.6 pp)
let lightblue_box_tt pp = with_vspacing (rgbcolor_box_tt 0.8 0.8 1.0 pp)
let lightred_box_tt pp = with_vspacing (rgbcolor_box_tt 1.0 0.8 0.8 pp)

let noindent_tt pp = 
  with_vspacing (fun fmt s ->
    fprintf fmt "\n\n\\noindent{\\ttfamily\\parindent 0pt\n";
    pp fmt s;
    fprintf fmt "}\n")

let noindent_sf pp = 
  with_vspacing (fun fmt s ->
    fprintf fmt "\n\n\\noindent{\\sffamily\\parindent 0pt\n";
    pp fmt s;
    fprintf fmt "}\n")

let make_table l =
  let h = Hashtbl.create 97 in 
  List.iter (fun s -> Hashtbl.add h s ()) l;
  Hashtbl.mem h
