open Printf 

type dir = Left | Right | Here

type calls = (bool * bool * string, (int * int list * int list) list) Hashtbl.t

let dz = function
	| Left -> -1
	| Here -> 0
	| Right -> 1

type ('a, 'b, 'c) instr = 
	| Move of dir * 'a 
	| Match of ('b, 'b * 'a) Matching.t 
	| MatchRall of ('a * 'c * ('b, ('b * 'a)) Matching.t)
	| MatchEall of ('b * 'a * 'c * ('b, ('b * 'a)) Matching.t)

let is_phantom = function
  | Match t -> Matching.length t = 0
  | _ -> false

type source = (int, (int, int, Matching.but) instr) Hashtbl.t

exception Collision

exception Funky of (int, int, Matching.but) instr

let sadd src s i =
	match i with
	| Move (d,s') ->
		if Hashtbl.mem src s then 
			if is_phantom (Hashtbl.find src s) then
				Hashtbl.replace src s i
			else raise Collision
		else
			Hashtbl.add src s i
	| Match t ->
		(try
		  match Hashtbl.find src s with
		 	| Move _ -> 
				if not (is_phantom i) then raise Collision
			| Match t' ->
				Hashtbl.replace src s (Match (Matching.merge t t'))
			| MatchRall (sd, butl, t') ->
				if Matching.inside t butl then
					let me = Matching.merge t t' in
					Hashtbl.replace src s (MatchRall (sd, butl, me))
				else raise Collision
			| MatchEall (sd, a, butl, t') ->
				if Matching.inside t butl then
					let me = Matching.merge t t' in
					Hashtbl.replace src s (MatchEall (sd, a, butl, me))
				else raise Collision
		with Not_found ->
			Hashtbl.add src s i)
	| MatchRall (sd, butl, t) ->
		(try
		  match Hashtbl.find src s with
			| Match t' ->
				if Matching.inside t' butl then
					let me = Matching.merge t t' in
					Hashtbl.replace src s (MatchRall (sd, butl, me))
				else raise Collision
			| _ -> raise Collision
		with Not_found ->
			Hashtbl.add src s i)
	| MatchEall (sd, a, butl, t) ->
		(try
		  match Hashtbl.find src s with
			| Match t' ->
				if Matching.inside t' butl then
					let me = Matching.merge t t' in
					Hashtbl.replace src s (MatchEall (sd, a, butl, me))
				else raise Collision
			| _ -> raise Collision
		with Not_found ->
			Hashtbl.add src s i)


type rtm = { mutable states: int; letters: int ref; src: source; ssymb: Stab.t; lsymb: Stab.t; c: calls }

let rtm_copy m = { states=m.states; letters=ref !(m.letters); src=Hashtbl.copy m.src; ssymb=Stab.copy m.ssymb; lsymb=m.lsymb; c=Hashtbl.copy m.c }

let gs ma s =
	try
		Stab.of_int ma.ssymb s
	with Not_found -> sprintf "#%i" s

let gl ma l = 
	try
		Stab.of_int ma.lsymb l
	with Not_found -> failwith "Houston... Letter problem :("

let letters m = !(m.letters)
let states m = m.states
let lint m = Stab.of_string m.lsymb
let sint m = Stab.of_string m.ssymb
let lname m = Stab.of_int m.lsymb
let sname m = Stab.of_int m.ssymb
let snames m = Stab.all m.ssymb
	
let create () = {states=0; letters=ref 0; src=Hashtbl.create 97; ssymb=Stab.create (); lsymb=Stab.create (); c=Hashtbl.create 97}

let create_lshared m = {states=0; letters=m.letters; src=Hashtbl.create 97; ssymb=Stab.create (); lsymb=m.lsymb; c=Hashtbl.create 97}

exception Not_Shared
	
let gas m s = 
	try
		Stab.get_or_add m.ssymb s
	with Stab.New i ->
		m.states <- m.states+1;
		i

let anonymous m =
	let i = Stab.anonymous m.ssymb in
	m.states <- m.states+1;
	sadd m.src i (Match (Matching.create ()));
	i

let gal m s = 
	try
		Stab.get_or_add m.lsymb s
	with Stab.New i ->
		incr m.letters;
		i

let get m s = Hashtbl.find m.src s

let add m ss = function
	| Move (d,ss') ->
		let s = gas m ss in
		let s' = gas m ss' in
		sadd m.src s (Move (d,s'))
	| Match st ->
		let s = gas m ss in
		let t = ref (Matching.create ()) in
		let aux sa (sb,ss) =
			let a = gal m sa in
			let s = gas m ss in
			let b = gal m sb in
			t := Matching.add (a,(b,s)) !t in
		Matching.iter aux st;
		sadd m.src s (Match !t)
	| MatchRall (ss', butl, st) ->
		let s = gas m ss in
		let s' = gas m ss' in
		let t = ref (Matching.create ()) in
		let aux sa (sb,ss) =
			let a = gal m sa in
			let s = gas m ss in
			let b = gal m sb in
			t := Matching.add (a,(b,s)) !t in
		Matching.iter aux st;
		sadd m.src s (MatchRall (s',Matching.newbutl (List.map (gal m) butl) !t,!t))
	| MatchEall (ss', sa, butl, st) ->
		let s = gas m ss in
		let s' = gas m ss'
		and a = gal m sa in
		let t = ref (Matching.create ()) in
		let aux sa (sb,ss) =
			let a = gal m sa in
			let s = gas m ss in
			let b = gal m sb in
			t := Matching.add (a,(b,s)) !t in
		Matching.iter aux st;
		sadd m.src s (MatchEall (s',a,Matching.newbutl (List.map (gal m) butl) !t,!t))


let phantom m ss =
	let s = gas m ss in
	let t = Matching.create () in
	sadd m.src s (Match t)

let addientry m (rev,lr,i,n,o,l) =
	let ll =
		if Hashtbl.mem m.c (rev,lr,n) then
			Hashtbl.find m.c (rev,lr,n)
		else [] in
	Hashtbl.replace m.c (rev,lr,n) ((l,i,o)::ll)

let addentry m (rev,lr,i,n,o,l) =
	let ni = List.map (gas m) i
	and no = List.map (gas m) o in
	List.iter (fun s -> phantom m s) i;
	List.iter (fun s -> phantom m s) o;
	addientry m (rev,lr,ni,n,no,gal m l)

let step m s a =
	match Hashtbl.find m.src s with
	| Move (d,s') -> (s',a,dz d)
	| Match t ->
		let (b,s') = Matching.find a t in
		(s',b,0)
	| MatchRall (sd,butl,t) ->
		(try
			let (b,s') = Matching.find a t in
			(s',b,0)
		with Not_found ->
			if Matching.inbut a butl then
				raise Not_found
			else
				(sd,a,0))
	| MatchEall (sd,c,butl,t) ->
		(try
			let (b,s') = Matching.find a t in
			(s',b,0)
		with Not_found ->
			if Matching.inbut a butl then
				raise Not_found
			else
				(sd,c,0))

type frtm = { m: rtm; i: string list; o: string list }

let inside f = f.m
let params f = List.length f.i, List.length f.o

module Int = struct type t = int let compare = compare end
module S = Set.Make(Int)

let alldiff m l =
	try
		let s = ref S.empty in
		List.iter (fun x -> 
			let n = sint m x in
			if S.mem n !s then raise Not_found;
			s := S.add n !s) l;
		true
	with Not_found -> false

exception Syntax_Error

let defun m i o = 
	if not (alldiff m (i @ o)) then raise Syntax_Error;
	List.iter (fun x -> phantom m x) o;
	{ m=m; i=i; o=o }

exception Wrong_Number_Of_Parameters

let fcopy f fc ma sub inames onames =
	if ma.lsymb != sub.m.lsymb then raise Not_Shared;
	let n = sub.m.states 
	and ni = List.length sub.i
	and no = List.length sub.o in
	if (ni <> List.length inames) || (no <> List.length onames) then raise Wrong_Number_Of_Parameters;
	let oldtonew = Hashtbl.create 97 in
	for s = 0 to n-1 do
		try
			let old = Stab.of_int sub.m.ssymb s in
			let aux o n =
				if o = old then 
					(Hashtbl.add oldtonew s (gas ma n);
					phantom ma n) in
			List.iter2 aux sub.i inames;
			List.iter2 aux sub.o onames;
			if not (Hashtbl.mem oldtonew s) then
				Hashtbl.add oldtonew s (anonymous ma)
		with Not_found -> 
			Hashtbl.add oldtonew s (anonymous ma)
	done;
	let translate = function
		| (s, Move (d,s')) -> (Hashtbl.find oldtonew s, Move (d,Hashtbl.find oldtonew s'))
		| (s, Match t) -> (Hashtbl.find oldtonew s, 
			Match (Matching.map (fun (a,(b,s)) -> (a,(b,Hashtbl.find oldtonew s))) t))
		| (s, MatchRall (sd,butl,t)) -> (Hashtbl.find oldtonew s, 
			MatchRall (Hashtbl.find oldtonew sd, butl, Matching.map (fun (a,(b,s)) -> (a,(b,Hashtbl.find oldtonew s))) t))
		| (s, MatchEall (sd,c,butl,t)) -> (Hashtbl.find oldtonew s, 
			MatchEall (Hashtbl.find oldtonew sd, c, butl, Matching.map (fun (a,(b,s)) -> (a,(b,Hashtbl.find oldtonew s))) t)) in
	Hashtbl.iter (fun s i -> 
		let l = f (translate (s,i)) in
		List.iter (fun (s,i) -> sadd ma.src s i) l) sub.m.src;
	Hashtbl.iter (fun (rev,lr,n) le ->
		let rev',lr' = fc (rev,lr) in
		List.iter (fun (l,i,o) -> 
			let i,o = List.map (fun s -> Hashtbl.find oldtonew s) i, List.map (fun s -> Hashtbl.find oldtonew s) o in
			let i,o = if rev' <> rev then o,i else i,o in
			addientry ma (rev',lr',i,n,o,l)) le) sub.m.c

let copy = fcopy (fun x -> [x]) (fun x -> x)

let lri = function
	| (s,Move (Left,x)) -> [s,Move (Right,x)]
	| (s,Move (Right,x)) -> [s,Move (Left,x)]
	| x -> [x]

let lr = fcopy lri (fun (rev,lr) -> (rev, not lr))

let revi = function
	| (s,Move (Left,s')) -> [s',Move (Right,s)]
	| (s,Move (Right,s')) -> [s',Move (Left,s)]
	| (s,Move (Here,s')) -> [s',Move (Here,s)]
	| (s,Match t) -> Matching.mapl (fun a (b,s') -> (s', Match (Matching.single (b,(a,s))))) t
	| (s,MatchRall (sd, butl, t)) -> 
			 (sd, MatchRall (s, butl, Matching.create ()))::(Matching.mapl (fun a (b,s') -> (s', Match (Matching.single (b,(a,s))))) t)
	| (s,MatchEall (sd, c, butl, t)) -> raise Collision

let revert = fcopy revi (fun (rev, lr) -> (not rev, lr))

let revlri x = List.concat (List.map revi (lri x))

(*
val compose : rtm -> frtm list -> string list -> string list -> unit
val torus : rtm -> frtm -> string list -> string list -> unit
*)

exception Bad_Call

let link m ca st =
	let m = rtm_copy m in
	let cur = ref (states m)
	and linked = Hashtbl.create 97 in
	let fresh pre k =
		let l = ref [] in
		for i = 1 to k do
			let s = pre ^ (string_of_int (!cur + k - i)) in
			ignore (gas m s);
			l := s::(!l)
		done;
		cur := !cur + k;
		!l in
	let addfun (n,i,o) =
		let f = Hashtbl.find ca n in
		copy m f i o;
		let e = ref 0 in
		Hashtbl.add linked (false,false,n) (i,o,e)
	and addcall (rev,lr,n,l,i,o) =
		let (li,lo,e) =
			try
				Hashtbl.find linked (rev,lr,n);
			with Not_found ->
				let f = Hashtbl.find ca n in
				let ni, no = List.length f.i, List.length f.o in
				let li, lo = fresh (if rev then ">" else "<") ni, fresh (if rev then "<" else ">") no in
				fcopy (match (rev,lr) with 
					| false,false -> (fun x -> [x])
					| false,true -> lri
					| true,false -> revi
					| true,true -> revlri)
					 (match (rev,lr) with
					| false,false -> (fun x -> x)
					| false,true -> (fun (rev,lr) -> (rev,not lr))
					| true,false -> (fun (rev,lr) -> (not rev,lr))
					| true,true -> (fun (rev,lr) -> (not rev,not lr)))
					m f li lo;
				let (li,lo) = if rev then (lo,li) else (li,lo) in
				let e = ref 0 in
				Hashtbl.add linked (rev,lr,n) (li,lo,e);
				(li,lo,e) in
		let le = gal m ("@" ^ (string_of_int !e)) in
		incr e;
		let rec auxi = function
		| s::is, ls::lis ->
			sadd m.src s (Match (Matching.single (l,(le,gas m ls))));
			auxi (is, lis)
		| [],[] -> ()
		| _ -> raise Bad_Call in
		auxi (i,li);
		let rec auxo = function
		| s::os, ls::los ->
			sadd m.src (gas m ls) (Match (Matching.single (le,(l,s))));
			auxo (os, los)
		| [],[] -> ()
		| _ -> raise Bad_Call in
		auxo (o,lo) in
	(match st with
	| Some (i,n,o) -> 
		addfun (n,i,o);
		List.iter (phantom m) o
	| None -> ());
	while Hashtbl.length m.c > 0 do
		let c' = Hashtbl.copy m.c in
		Hashtbl.clear m.c;
		Hashtbl.iter (fun (rev,lr,n) ll ->
			List.iter (fun (l,i,o) -> addcall (rev,lr,n,l,i,o)) ll) c'
	done;
	m



exception Invalid_file of string

let icode f x =
	if x mod 2 = 0 then
	  begin
		seek_in f x;
		let gw () = input_binary_int f in
		let t = ref (Matching.create ()) in
		let cur = ref (gw ()) in
		while !cur <> 0 do
			let s = gw () in
			cur := (!cur-1)/2;
			let b = !cur land 0x7fff
			and a = !cur lsr 15 in
			t := Matching.add (a,(b,s)) !t;
			cur := gw ()
		done;
		Match !t
	  end
	else
	  begin
		let s = x/4
		and d = (x/2) mod 2 in
		Move ((match d with 0 -> Left | _ -> Right),s)
	  end
	
let input_charz f =
	let pos = pos_in f in
	while input_char f <> '\000' do () done;
	let l = (pos_in f) - pos - 1 in
	let s = String.create l in
	seek_in f pos;
	really_input f s 0 l;
	ignore (input_char f);
	s

let output_charz f s =
	output_string f s;
	output_char f '\000'

let load name =
	let f1 = open_in_bin name
	and f2 = open_in_bin name
	and ma = create () in
	let gw () = input_binary_int f1 
	and icode = icode f2 in
	try
		if (input_char f1 <> 'r')
		|| (input_char f1 <> 'u') 
		|| (input_char f1 <> 't') 
		|| (input_char f1 <> '\n') then raise (Invalid_file "bad magic");
		let m = gw () in
		let n = gw () in
		ma.letters := m;
		ma.states <- n;
		Stab.set_last ma.lsymb (m-1);
		Stab.set_last ma.ssymb (n-1);
		let olst = gw () in
		for s = 0 to n-1 do
			let i = icode (gw ()) in
			sadd ma.src s i
		done;
		seek_in f1 olst;
		let gs () = input_charz f1 in
		for l = 0 to m-1 do
			Stab.set ma.lsymb l (gs ())
		done;
		(try
			while true do
				let s = gw () in
				let name = gs () in
				Stab.set ma.ssymb s name
			done
		with End_of_file -> ());
		ma
	with e ->
		close_in f1;
		close_in f2;
		raise e

let ocode nl ofs = function
	| Move (d,s) ->
		4*s+2*(match d with Left -> 0 | Right -> 1 | Here -> failwith "Here movement forbidden in rut!")+1
	| Match t ->
		if !ofs mod 2 = 1 then incr ofs;
		let sz = 2*(Matching.length t)+1
		and r = !ofs in
		ofs := !ofs + 4*sz;
		r
	| MatchRall (_,but,t) | MatchEall (_,_,but,t) ->
		if !ofs mod 2 = 1 then incr ofs;
		let sz = 2*((Matching.length t)+(Matching.butlen but nl))+1
		and r = !ofs in
		ofs := !ofs + 4*sz;
		r		

let save ma name =
  	let f = open_out_bin name in
	let sw = output_binary_int f in
	output_char f 'r';
	output_char f 'u';
	output_char f 't';
	output_char f '\n';
	sw !(ma.letters);
	sw ma.states;
	sw 0;
	let ofs = ref (4*(4+ma.states)) in
	let ocode = ocode !(ma.letters) ofs in
	for s = 0 to ma.states-1 do
		try
  			sw (ocode (Hashtbl.find ma.src s))
		with Not_found -> 
			let t = Matching.create () in
			sadd ma.src s (Match t);
			sw (ocode (Match t))
			(* failwith (sprintf "instruction %i (%s) not found" s (gs ma s)) *)
	done;
	let pos = pos_out f in
	seek_out f 12;
	sw !ofs;
	seek_out f pos;
	for s = 0 to ma.states-1 do
		match Hashtbl.find ma.src s with
		| Match t ->
			if (pos_out f) mod 2 = 1 then sw 0;
			Matching.iter (fun a (b,s) -> sw ((a lsl 16)+2*b+1); sw s) t;
			sw 0
		| Move _ -> ()
		| MatchRall (sd,but,t) ->
			if (pos_out f) mod 2 = 1 then sw 0;
			Matching.iter (fun a (b,s) -> sw ((a lsl 16)+2*b+1); sw s) t;
			Matching.buter (fun a -> sw ((a lsl 16)+2*a+1); sw sd) but !(ma.letters);
			sw 0
		| MatchEall (sd,c,but,t) ->
			if (pos_out f) mod 2 = 1 then sw 0;
			Matching.iter (fun a (b,s) -> sw ((a lsl 16)+2*b+1); sw s) t;
			Matching.buter (fun a -> sw ((a lsl 16)+2*c+1); sw sd) but !(ma.letters);
			sw 0
			
	done;
	for l = 0 to !(ma.letters)-1 do
		output_charz f (Stab.of_int ma.lsymb l)
	done;
	for s = 0 to ma.states-1 do
		try
			let name = Stab.of_int ma.ssymb s in
			sw s;
			output_charz f name
		with Not_found -> ()
	done;
	close_out f

let virg st = function
	| [] -> ""
	| x::xs -> List.fold_left (fun s i -> s ^ st ^ i) x xs

let prmatch gs gl t =
	virg " | " (Matching.mapl (fun a (b,s) -> sprintf "%s:%s, %s" (gl a) (gl b) (gs s)) t)

let prbutl gs gl = function
	| [] -> ""
	| a::ls ->
		List.fold_left (fun s b -> s ^ ", " ^ (gl b)) (" but " ^ (gl a)) ls

let block (i,n,o) = (virg "," i) ^ "|" ^ (virg " " n) ^ "|" ^ (virg "," o)

let dump ma oc pre =
	let gs = gs ma
	and gl = gl ma in
	let prmatch = prmatch gs gl
	and prbutl = prbutl gs gl in
	let dumpi = function
	 	| (s,Move(d,s')) ->
		 	fprintf oc "%s%s. %s, %s\n" pre (gs s) (match d with Left -> "<-" | Right -> "->" | Here -> "!") (gs s')
		| (s,Match t) -> 
			fprintf oc "%s%s. %s\n" pre (gs s) (prmatch t) 
		| (s,MatchRall (sd, butl, t)) -> fprintf oc "%s%s. %s else %s%s\n" pre (gs s) (prmatch t) (gs sd) (prbutl (Matching.del butl t))
		| (s,MatchEall (sd, c, butl, t)) -> fprintf oc "%s%s. %s else %s write %s%s\n" pre (gs s) (prmatch t) (gs sd) (gl c) (prbutl (Matching.del butl t))
			in
	Hashtbl.iter (fun x y -> dumpi (x,y)) ma.src;
	Hashtbl.iter (fun (rev,lr,n) ll ->
		List.iter (fun (l,i,o) -> Printf.fprintf oc "%scall %c%s%c from %s\n" pre (if rev then '<' else '[') (block (List.map gs i,(if lr then ["lr";n] else [n]),List.map gs o)) (if rev then ']' else '>') (gl l)) ll) ma.c
		
let runit m mx a s pos =
	let sN = Array.length a in
	let cur = ref pos
	and s = ref (sint m s) in
	let pr () =
		for i = 0 to !cur-1 do
			Printf.printf "%s" a.(i)
		done;
		Printf.printf "(%s)" a.(!cur);
		for i = !cur+1 to sN-1 do
			Printf.printf "%s" a.(i)
		done;
		Printf.printf ", %s\n" (gs m !s) in
	let tick () =
		let (s',b,dz) = step m !s (lint m a.(!cur)) in
		a.(!cur) <- lname m b;
		cur := !cur+dz;
		s := s' in
	try
		match mx with 
		| None -> 
			while (!cur >= 0) && (!cur < sN) do
				pr ();
				tick ()
			done
		| Some mx ->
			let st = ref 0 in
			while (!cur >= 0) && (!cur < sN) && (!st <= mx) do
				pr ();
				tick ();
				incr st
			done			
	with Not_found -> ()

let cv s =
	let n = String.length s in
	let a = Array.create n "" in
	for i = 0 to n-1 do
		a.(i) <- String.sub s i 1
	done;
	a
let showa m mx s pos tape = runit m mx tape s pos
let show m mx s pos tape = runit m mx (cv tape) s pos