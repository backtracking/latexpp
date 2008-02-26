
val input_file : string option
val output_file : string option
val color : bool

type command = Add of string * string | Remove of string

val env_mappings : command list
val macro_mappings : command list
