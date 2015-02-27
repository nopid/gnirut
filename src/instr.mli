type t =
	| Single of string * (string, string, string list) Rtm.instr
	| Forward of string list * string list * string list
	| Backward of string list * string list * string list
	| Def of string list * string * string list
	| Fun of string list * string * string list
	| Call of bool * bool * string list * string list * string list * string list

val to_str : t -> string
