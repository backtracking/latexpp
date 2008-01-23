
open Format
open Arg

let input_file = ref None
let output_file = ref None
let whizzytex = ref false
let color = ref false
let env_mappings = ref []
let add_env_mapping s1 s2 = env_mappings := (s1,s2) :: !env_mappings
let macro_mappings = ref []
let add_macro_mapping s1 s2 = macro_mappings := (s1,s2) :: !macro_mappings

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
    "-c", Set color, "uses colors";
    "--version", Unit print_version, "prints version and exits";
    "-e", Tuple (let s1 = ref "" in
		 [Set_string s1; String (fun s2 -> add_env_mapping !s1 s2)]),
    "<id1> <id2> maps LaTeX environment id1 to preprocessor id2";
    "-m", Tuple (let s1 = ref "" in
		 [Set_string s1; String (fun s2 -> add_macro_mapping !s1 s2)]),
    "<id1> <id2> maps LaTeX macro id1 to preprocessor id2";
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
let color = !color

let env_mappings = List.rev !env_mappings
let macro_mappings = List.rev !macro_mappings
