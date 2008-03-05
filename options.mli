
(* command line options *)

val input_file : string option
val output_file : string option

type command = Add of string * string | Remove of string

val env_mappings : command list
val macro_mappings : command list

(* pp options *)

val add : string -> string -> unit

val find : string -> string option

val is_set : string -> bool

val with_options : (string * string) list -> ('a -> 'b) -> 'a -> 'b
