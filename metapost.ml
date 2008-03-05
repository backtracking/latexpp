
open Format

let metapost fmt0 s =
  let f = "fig" in
  let fig = f ^ ".1" in
  (*if not (Sys.file_exists fig) then begin*)
    let fmp = f ^ ".mp" in
    let c = open_out fmp in
    let fmt = formatter_of_out_channel c in
    fprintf fmt "beginfig(1)@\n";
    fprintf fmt "%s@\n" s;
    fprintf fmt "endfig;@.";
    close_out c;
    ignore (Sys.command ("mpost " ^ fmp ^ " end"));
  (* end;*)
  fprintf fmt0 "\\includegraphics{%s}\n" fig
    
let () = Pp.add_pp_environment "metapost" metapost
let () = Pp.add_pp_environment "mpost" metapost
