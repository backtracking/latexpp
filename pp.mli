
open Format

val add_pp_environment : string -> (formatter -> string -> unit) -> unit

val pp : Lexing.lexbuf -> unit
