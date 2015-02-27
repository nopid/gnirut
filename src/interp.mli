type t

exception Bad_Indent
exception NIY

val create : unit -> t
val eat : t -> Lexing.lexbuf -> out_channel -> (Rtm.rtm -> unit) -> bool
val use : t -> string -> out_channel -> (Rtm.rtm -> unit) -> unit
val save : t -> string -> unit