
open Format
open Arg

let input_file = ref None
let output_file = ref None
let whizzytex = ref false
let color = ref false

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

let spec =
  [ 
    "-w", Set whizzytex, "behaves as Whizzytex preprocessor"; 
    "-o", String set_output_file, "sets the output file";
    "-c", Set color, "use colors";
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

