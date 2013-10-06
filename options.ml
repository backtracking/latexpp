
open Format
open Arg

(* pp options *)

let opt = Hashtbl.create 97

let add o v = Hashtbl.add opt o v

let remove = Hashtbl.remove opt

let find o = try Some (Hashtbl.find opt o) with Not_found -> None

let is_set o = match find o with
  | Some ("true" | "yes" | "1") -> true
  | None | Some _ -> false

let with_options ol f x =
  List.iter (fun (o,v) -> add o v) ol;
  let unroll () = List.iter (fun (o,_) -> Hashtbl.remove opt o) ol in
  try
    let y = f x in unroll (); y
  with e ->
    unroll (); raise e

let () = add "vspacing" "\\medskip\\noindent"
let () = add "color" "yes"
let () = add "keywords" "yes"

(* command line options *)

let input_file = ref None
let output_file = ref None
let whizzytex = ref false
let auto_spacing = ref true
let list_all = ref false

type command = Add of string * string | Remove of string

let env_mappings = ref []
let add_env_mapping s1 s2 = env_mappings := Add (s1,s2) :: !env_mappings
let remove_env s1 = env_mappings := Remove s1 :: !env_mappings
let macro_mappings = ref []
let add_macro_mapping s1 s2 = macro_mappings := Add (s1,s2) :: !macro_mappings
let remove_macro s1 = macro_mappings := Remove s1 :: !macro_mappings

let set_input_file f = match !input_file with
  | None ->
      if not !whizzytex && not (Sys.file_exists f) then begin
	eprintf "latexpp: %s: no such file@." f;
	exit 1
      end;
      input_file := Some f
  | Some _ ->
      raise (Bad "you must specify at most one input file on the command line")

let set_output_file f = match !output_file with
  | None -> output_file := Some f
  | Some _ -> raise (Bad "option -o cannot be used more than once")

let print_version () =
  printf "This is latexpp version %s, compiled on %s@."
    Version.version Version.date;
  printf "Copyright (c) 2008 Jean-Christophe Filliatre@.";
  exit 0

let spec =
  [
    "-w", Set whizzytex, "behaves as Whizzytex preprocessor";
    "-o", String set_output_file, "<file> sets the output file";
    "--version", Unit print_version, "prints version and exits";
    "-e", Tuple (let s1 = ref "" in
		 [Set_string s1; String (fun s2 -> add_env_mapping !s1 s2)]),
    "<id1> <id2> maps LaTeX environment <id1> to preprocessor <id2>";
    "-m", Tuple (let s1 = ref "" in
		 [Set_string s1; String (fun s2 -> add_macro_mapping !s1 s2)]),
    "<id1> <id2> maps LaTeX macro <id1> to preprocessor <id2>";
    "-re", String remove_env,
    "<id> removes interpretation of environment <id>";
    "-rm", String remove_macro,
    "<id> removes interpretation of macro <id>";
    "-g", Tuple (let s1 = ref "" in
		 [Set_string s1; String (fun s2 -> add !s1 s2)]),
    "<o> <v> set global option <o> to value <v>";
    "-set", String (fun o -> add o "yes"),
    "<o> sets global option <o> to value `yes'";
    "-unset", String (fun o -> add o "no"),
    "<o> sets global option <o> to value `no'";
    "-clear", String remove,
    "<o> removes any value for global option <o>";
    "-no-auto-spacing", Clear auto_spacing,
    "disables auto spacing";
    "-l", Set list_all, "lists all mappings";
  ]

let usage_msg = "latexpp [options] [file]"

let () =
  parse spec set_input_file usage_msg

let () =
  if !whizzytex then begin
    if !output_file <> None then begin
      eprintf "latexpp: options -o and -w are not compatible@.";
      exit 1
    end;
    match !input_file with
      | None ->
	  eprintf
	    "latexpp: option -w requires an input file to be specified@.";
	  exit 1
      | Some f ->
	  let fnew = Filename.chop_extension f ^ ".new" in
	  if not (Sys.file_exists fnew) then begin
	    eprintf "latexpp: %s: no such file@." fnew;
	    exit 1
	  end;
	  input_file := Some fnew;
	  output_file := Some f
  end

let input_file = !input_file
let output_file = !output_file
let whizzytex = !whizzytex
let auto_spacing = !auto_spacing
let list_all = !list_all

let env_mappings = List.rev !env_mappings
let macro_mappings = List.rev !macro_mappings

