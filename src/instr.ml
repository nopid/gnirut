open Printf
open Rtm

type t =
	| Single of string * (string, string, string list) Rtm.instr
	| Forward of string list * string list * string list
	| Backward of string list * string list * string list
	| Def of string list * string * string list
	| Fun of string list * string * string list
	| Call of bool * bool * string list * string list * string list * string list

let virg st = function
	| [] -> ""
	| x::xs -> List.fold_left (fun s i -> s ^ st ^ i) x xs
	
let block (i,n,o) = (virg "," i) ^ "|" ^ (virg " " n) ^ "|" ^ (virg "," o)

let prmatch t =
	virg " | " (Matching.mapl (fun a (b,s) -> sprintf "%s:%s, %s" a b s) t)

let prbutl = function
	| [] -> ""
	| a::ls ->
		List.fold_left (fun s b -> s ^ ", " ^ b) (" but " ^ a) ls

let single_to_str s = function
	| Move (Left,s') -> sprintf "%s. <-, %s" s s'
	| Move (Right,s') -> sprintf "%s. ->, %s" s s'
	| Move (Here,s') -> sprintf "%s. !, %s" s s'
	| Match t -> sprintf "%s. %s" s (prmatch t)
	| MatchRall (s', butl, t) -> sprintf "%s. %s else %s%s" s (prmatch t) s' (prbutl (Matching.ldel butl t))
	| MatchEall (s', a, butl, t) -> sprintf "%s. %s else %s write %s%s" s (prmatch t) s' a (prbutl (Matching.ldel butl t))

let to_str = function
	| Single (s, i) -> single_to_str s i
	| Forward (i,n,o) -> "[" ^ (block (i,n,o)) ^ ">"
	| Backward (i,n,o) -> "<" ^ (block (i,n,o)) ^ "]"
	| Def (i,n,o) -> "def [" ^ (block (i,[n],o)) ^ ">:"
	| Fun (i,n,o) -> "fun [" ^ (block (i,[n],o)) ^ ">:"
	| Call (false,lr,i,n,o,l) -> "call [" ^ (block (i,(if lr then "lr"::n else n),o)) ^ "> from " ^ (virg "," l)
	| Call (true,lr,i,n,o,l) -> "call <" ^ (block (i,(if lr then "lr"::n else n),o)) ^ "] from " ^ (virg "," l)
