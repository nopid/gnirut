type cmd =
	| Code of int * Instr.t option
	| Clear
	| Show of int option * string * int * string
	| Info
	| Letters
	| States
	| Load of string
	| Save of string
	| Use of string
	| Run
	| Dump of string option
	| Torus
	| Link of (string list * string * string list) option
