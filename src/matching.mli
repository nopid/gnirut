type ('a, 'b) t
type but
val create : unit -> ('a,'b) t
val single : 'a * 'b -> ('a, 'b) t
exception Collision
val add : 'a * 'b -> ('a, 'b) t -> ('a, 'b) t
val find : 'a -> ('a, 'b) t -> 'b
val mem : 'a -> ('a, 'b) t -> bool
val merge : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val iter : ('a -> 'b -> unit) -> ('a,'b) t -> unit
val butlen : but -> int -> int
val bmem : int -> but -> bool
val buter : (int -> unit) -> but -> int -> unit
val map : ('a * 'b -> 'a * 'b) -> ('a,'b) t -> ('a,'b) t
val mapl : ('a -> 'b -> 'c) -> ('a,'b) t -> 'c list
val length : ('a, 'b) t -> int
val of_list : ('a * 'b) list -> ('a, 'b) t
val inside : (int, 'b) t -> but -> bool
val del : but -> (int, 'b) t -> int list
val ldel : string list -> (string, 'b) t -> string list
val newbutl : int list -> (int, 'b) t -> but
val inbut : int -> but -> bool
