
open Format
open Options

let () = 
  List.iter (function 
	       | Add (s1,s2) -> Pp.map_environment s1 s2
	       | Remove s1 -> Pp.remove_environment s1)
    env_mappings
let () = 
  List.iter (function
	       | Add (s1,s2) -> Pp.map_macro s1 s2
	       | Remove s1 -> Pp.remove_macro s1)
    macro_mappings

let main () =
  begin match output_file with
    | None -> 
	()
    | Some f -> 
	let c = open_out f in
	let fmt = formatter_of_out_channel c in
	Pp.set_out_formatter fmt;
	at_exit (fun () -> pp_print_flush fmt (); close_out c)
  end;
  match input_file with
    | None -> 
	Pp.pp (Lexing.from_channel stdin)
    | Some f ->
	let c = open_in f in
	let lb =  Lexing.from_channel c in
	lb.Lexing.lex_curr_p <- 
	  { lb.Lexing.lex_curr_p with Lexing.pos_fname = f };
	Pp.pp lb;
	close_in c

let () = main ()
