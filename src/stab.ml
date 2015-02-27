type t = { mutable last : int; of_int: (int,string) Hashtbl.t;  of_string: (string,int) Hashtbl.t }

let create () = { last= -1; of_int=Hashtbl.create 97; of_string=Hashtbl.create 97 }

let copy t = { last=t.last; of_int=Hashtbl.copy t.of_int;  of_string=Hashtbl.copy t.of_string }

let set_last t v = t.last <- v

let of_int s i = Hashtbl.find s.of_int i

let of_string s i = Hashtbl.find s.of_string i

let all s =
	let l = ref [] in
	Hashtbl.iter (fun a b -> l := a::(!l)) s.of_string;
	!l

exception Already_in_use

let add s name =
	if Hashtbl.mem s.of_string name then raise Already_in_use;
	let n = s.last+1 in
	s.last <- n;
	Hashtbl.add s.of_string name n;
	Hashtbl.add s.of_int n name

let anonymous s =
	let n = s.last+1 in
	s.last <- n;
	n

let set s n name =
	Hashtbl.add s.of_string name n;
	Hashtbl.add s.of_int n name

let rename s name name' =
	if Hashtbl.mem s.of_string name' then raise Already_in_use;
	let i = Hashtbl.find s.of_string name in
	Hashtbl.remove s.of_string name;
	Hashtbl.add s.of_string name' i;
	Hashtbl.replace s.of_int i name'

exception New of int

let get_or_add s name =
	try
		Hashtbl.find s.of_string name
	with Not_found ->
	let n = s.last+1 in
	s.last <- n;
	Hashtbl.add s.of_string name n;
	Hashtbl.add s.of_int n name;
	raise (New n)

let copy_subset t l =
	let t' = create () in
	t'.last <- t.last;
	List.iter (fun s ->
		let i = of_string t s in
		Hashtbl.add t'.of_int i s;
		Hashtbl.add t'.of_string s i) l;
	t'
