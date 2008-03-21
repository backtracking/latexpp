
open Format

let mlpost fmt0 s =
  let f = "mlfig" in
  let fig = f ^ ".1" in
  (*if not (Sys.file_exists fig) then begin*)
    let fmp = f ^ ".ml" in
    let c = open_out fmp in
    let fmt = formatter_of_out_channel c in
    fprintf fmt "%s@\n" s;
    fprintf fmt "let () = Metapost.emit \"%s\" fig@." f;
    close_out c;
    let out = Sys.command ("mlpost " ^ fmp) in
    if out <> 0 then exit out;
  (* end;*)
  fprintf fmt0 "\\includegraphics{%s}\n" fig
    
let () = Pp.add_pp_environment "mlpost" mlpost
