
open Format
open Arg

let input_file = ref None

let set_input_file f = match !input_file with
  | None -> 
      if not (Sys.file_exists f) then begin
	eprintf "latexpp: %s: no such file@." f;
	exit 1
      end;
      input_file := Some f
  | Some _ ->
      raise (Bad "you must specify at most one input file on the command line")

let spec =
  [ ]

let usage_msg = "latexpp [options] [file]"

let () = 
  parse spec set_input_file usage_msg

let input_file = !input_file

