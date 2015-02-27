type t
val create : unit -> t
val set_last : t -> int -> unit
val set : t -> int -> string -> unit
val of_int : t -> int -> string
val of_string : t -> string -> int
val all : t -> string list
exception Already_in_use
val add : t -> string -> unit
val anonymous : t -> int
val rename : t -> string -> string -> unit
exception New of int
val get_or_add : t -> string -> int
val copy : t -> t
val copy_subset : t -> string list -> t
