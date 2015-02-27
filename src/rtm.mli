type dir = Left | Right | Here

type ('a, 'b, 'c) instr = 
	| Move of dir * 'a 
	| Match of ('b, 'b * 'a) Matching.t 
	| MatchRall of ('a * 'c * ('b, ('b * 'a)) Matching.t)
	| MatchEall of ('b * 'a * 'c * ('b, ('b * 'a)) Matching.t)

exception Collision
type rtm

val create : unit -> rtm
val create_lshared : rtm -> rtm
val add : rtm -> string -> (string, string, string list) instr -> unit
val get : rtm -> int -> (int, int, Matching.but) instr
val phantom : rtm -> string -> unit
val states : rtm -> int
val letters : rtm -> int
val snames : rtm -> string list
val sname : rtm -> int -> string
val sint : rtm -> string -> int
val lname : rtm -> int -> string
val lint : rtm -> string -> int
val step : rtm -> int -> int -> int * int * int
val dump : rtm -> out_channel -> string -> unit

exception Not_Shared
exception Syntax_Error
exception Bad_Call

type frtm
val inside : frtm -> rtm
val params : frtm -> int*int
val defun : rtm -> string list -> string list -> frtm

val addentry : rtm -> bool * bool * string list * string * string list * string -> unit
val link : rtm -> (string, frtm) Hashtbl.t -> (string list * string * string list) option -> rtm

val copy : rtm -> frtm -> string list -> string list -> unit
val lr : rtm -> frtm -> string list -> string list -> unit
exception Wrong_Number_Of_Parameters
val revert : rtm -> frtm -> string list -> string list -> unit

val show : rtm -> int option -> string -> int -> string -> unit
val showa : rtm -> int option -> string -> int -> string array -> unit

(*
val compose : rtm -> frtm list -> string list -> string list -> unit
val torus : rtm -> frtm -> string list -> string list -> unit
*)

exception Invalid_file of string
val load : string -> rtm
val save : rtm -> string -> unit
