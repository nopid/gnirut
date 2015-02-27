module Int = struct type t = int let compare = compare end
module S = Set.Make(Int)
module String = struct type t = string let compare = compare end
module St = Set.Make(String)

type ('a, 'b) t = ('a * 'b) list
type but = S.t

let create () = []

let single (a,b) = [a,b]

let length = List.length

exception Collision

let rec add (a,b) = function
  | (a',b')::xs when a' < a -> (a',b')::(add (a,b) xs)
  | (a',b')::xs as l when a' > a -> (a,b)::l
  | (a',b')::xs -> raise Collision
  | [] -> [a,b]

let rec mem a = function
  | (a',_)::xs when a = a' -> true
  | _::xs -> mem a xs
  | [] -> false

let rec find a = function
  | (a',b')::xs when a' < a -> find a xs
  | (a',b')::xs when a' > a -> raise Not_found
  | (a',b')::xs -> b'
  | [] -> raise Not_found

let merge t t' =
	let rec aux = function
	| (a,b)::ts, ((a',_)::ts' as t') when a < a' -> (a,b)::(aux (ts,t'))
	| ((a,_)::ts as t), (a',b')::ts' when a' < a -> (a',b')::(aux (t,ts'))
	| t, [] -> t
	| [], t' -> t' 
	| _ -> raise Collision in
	aux (t,t')

let rec iter f = function
	| (a,b)::ts -> (f a b); iter f ts
	| [] -> ()

let buter f b nle =
	let l = ref [] in
	for x = 0 to nle-1 do
		if not (S.mem x b) then l := x::(!l)
	done;
	List.iter f (List.sort compare !l)

let butlen b nle =
	let n = ref 0 in
	for x = 0 to nle-1 do
		if not (S.mem x b) then incr n
	done;
	!n

let bmem a b = S.mem a b

let rec mapl f = function
	| (a,b)::ts -> (f a b)::(mapl f ts)
	| [] -> []
	
let rec map f = function
	| x::xs -> (f x)::(map f xs)
	| [] -> []
	
let of_list l = List.fold_left (fun a b -> add b a) [] l

let inside t butl =
	let rec aux = function
	| (a,_)::ts -> 
		if S.mem a butl then
			aux ts
		else false
	| [] -> true in
	aux t

let del butl t =
	let s = ref butl in
	List.iter (fun (a,_) -> s := S.remove a !s) t;
	let l = ref [] in
	S.iter (fun x -> l := x :: !l) !s;
	List.sort compare !l

let ldel l t =
	let s = ref St.empty in
	List.iter (fun a -> s := St.add a !s) l;
	List.iter (fun (a,_) -> s := St.remove a !s) t;
	let l = ref [] in
	St.iter (fun x -> l := x :: !l) !s;
	List.sort compare !l

let newbutl l t =
	let s = ref S.empty in
	List.iter (fun a -> s := S.add a !s) l;
	List.iter (fun (a,_) -> s := S.add a !s) t;
	!s
	
let inbut = S.mem