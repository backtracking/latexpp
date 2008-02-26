
open Format

val set_out_formatter : formatter -> unit

val add_pp_environment : string -> (formatter -> string -> unit) -> unit
val add_pp_macro : string -> (formatter -> string -> unit) -> unit

val map_environment : string -> string -> unit
val remove_environment : string -> unit

val map_macro : string -> string -> unit
val remove_macro : string -> unit

val pp : Lexing.lexbuf -> unit
