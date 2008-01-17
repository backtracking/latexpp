
open Format

val set_out_formatter : formatter -> unit

val add_pp_environment : string -> (formatter -> string -> unit) -> unit

val pp : Lexing.lexbuf -> unit
