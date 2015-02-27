open Rtm
open Parsetree
open Instr

type env = (string, frtm) Hashtbl.t
type state = { plvl: int; mutable ilvl: int option; def: (string list * string * string list) option; func: bool; mutable m: rtm; e: env }
type t = env * state list ref

exception Bad_Indent
exception NIY

let create () = Hashtbl.create 97, ref [ { plvl= -1; ilvl=Some 0; def=None; func=false; m=Rtm.create (); e=Hashtbl.create 97 } ]

let dumpc ca oc pre =
	Hashtbl.iter (fun n f -> 
		let (ni,no) = params f in
		Printf.fprintf oc "%s=> %s (%i,%i)\n" pre n ni no) ca

let rec eat (callables,t) lexbuf oc run =
	let cur = List.hd !t in
	(match fst (Gniyacc.input Gnilex.token lexbuf) with
	| Code (i,x) ->
		(match cur.ilvl with
			| None ->
				if i > cur.plvl then
					cur.ilvl <- Some i
				else
					raise Bad_Indent
			| Some j ->
     			if i > j then 
					raise Bad_Indent
				else if i < j then
					match cur.def with
					| Some (i,n,o) ->
						List.iter (fun x -> phantom cur.m x) o;
						let f = defun cur.m i o in
						t := List.tl !t;
						if cur.func then
							Hashtbl.replace callables n f
						else
							Hashtbl.replace (List.hd !t).e n f
					| None -> failwith "argghhh!");
		let cur = List.hd !t in
		(match x with
			| None -> ()
			| Some (Single (a,b)) -> add cur.m a b
			| Some (Def (i,n,o)) ->
				let ilvl = match cur.ilvl with Some i -> i | None -> failwith "urggh?" in
				let ns = { plvl = ilvl; ilvl=None; def=Some (i,n,o); func=false; m=Rtm.create_lshared cur.m; e=Hashtbl.copy cur.e } in
				t := ns::(!t)
			| Some (Fun (i,n,o)) ->
				let ilvl = match cur.ilvl with Some i -> i | None -> failwith "urggh?" in
				let ns = { plvl = ilvl; ilvl=None; def=Some (i,n,o); func=true; m=Rtm.create_lshared cur.m; e=Hashtbl.copy cur.e } in
				t := ns::(!t)
			| Some (Forward (i,l,o)) ->
				(match l with
				| [n] -> copy cur.m (Hashtbl.find cur.e n) i o
				| ["lr"; n] -> lr cur.m (Hashtbl.find cur.e n) i o
				| _ -> raise NIY)
			| Some (Backward (i,l,o)) ->	
				(match l with
				| [n] -> revert cur.m (Hashtbl.find cur.e n) o i
				| _ -> raise NIY)
			| Some (Call (rev,lr,i,n,o,l)) ->
				(match n,l with
				| [n],[l] -> addentry cur.m (rev,lr,i,n,o,l)
				| _ -> raise NIY))
	| Clear ->
			cur.m <- create_lshared cur.m
	| Show (mx,s,pos,tape) -> show cur.m mx s pos tape
	| Info -> raise NIY
	| Link st ->
		t := [ { plvl= -1; ilvl=Some 0; def=None; func=false; m=link cur.m callables st; e=Hashtbl.create 97 } ]
	| Letters ->
			let k = letters cur.m in
			Printf.fprintf oc "%i letters: " k;
			for i = 0 to k-1 do
				Printf.fprintf oc "%s%s" (if i>0 then ", " else "") (lname cur.m i)
			done;
			Printf.fprintf oc "\n%!"
	| States -> raise NIY
	| Load s ->
		t := [ { plvl= -1; ilvl=Some 0; def=None; func=false; m=load s; e=Hashtbl.create 97 } ]
	| Save s -> save cur.m s
	| Use n -> use (callables,t) n oc run
	| Run -> run cur.m
	| Dump None -> 
		dump cur.m oc "  ";
		dumpc callables oc "  "
	| Dump (Some i) -> 
		let f = 
		if Hashtbl.mem cur.e i then 
			Hashtbl.find cur.e i
		else
			Hashtbl.find callables i in
			dump (inside f) oc "  "
	| Torus -> raise NIY);
	(List.hd !t).plvl > -1
and use t n oc run =
	let f = open_in n 
	and l = ref 0 in
	let lexbuf = Lexing.from_channel f in
	try
		while true do
			incr l;
			ignore (eat t lexbuf oc run)
		done
	with 
	| End_of_file -> close_in f
	| e -> close_in f; failwith ("error line " ^ (string_of_int !l))

let save (callables,t) s = save (List.hd !t).m s