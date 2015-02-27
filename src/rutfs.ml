open Graphics
open Rtm

let l=100
and sc=4

let pset x y c =
	set_color c;
	fill_rect ((x+l)*sc) ((y+l/2)*sc) (sc-1) (sc-1)
	
let w=2*l+1
and h=2*(l/2)+1

let qset x c =
	set_color c;
	fill_rect ((x+l)*sc) 0 (sc-1) (h*sc)

let _ =
	Random.self_init ();
	open_graph (Printf.sprintf " %ix%i" (w*sc) (h*sc));
	auto_synchronize false

let a=Array.create w 0
and s=ref 0
and xpos=ref 0
and ypos=ref 0

let finit np m =
	s := sint m "s";
	xpos := 0;
	ypos := 1;
	for i=0 to w-1 do
		a.(i) <- 0
	done;
	a.(l+ !xpos) <- 4

let binit np m =
	s := sint m "s";
	xpos := 0;
	ypos := 1;
	for i=0 to w-1 do
		a.(i) <- 0
	done;
	for i=l+5 to l+10 do
		a.(i) <- 2
	done;
	a.(l+4) <- 1;
	a.(l+11) <- 3;
	a.(l+ !xpos) <- (4 + (Random.int np))

let init np m =
	s := Random.int (states m);
	xpos := 0;
	ypos := 0;
	for i=0 to w-1 do
		a.(i) <- -1
	done

	
let dy=function
	| -1 -> 0
	| 0 -> 0
	| 1 -> 0
	| 2 -> 0
	| 3 -> -1
	| _ -> 1

let choose m nl s =
	match get m s with
	| Move _ -> -1
	| Match t ->
		let v = ref (Random.int nl) in
		while not (Matching.mem !v t) do
			v := Random.int nl
		done;
		!v
	| MatchRall (_,butl,_) | MatchEall (_,_,butl,_) -> 
		let v = ref (Random.int nl) in
		while Matching.bmem !v butl do
			v := Random.int nl
		done;
		!v

let step nl m =
	let iy = dy a.(l+ !xpos)
	and (s',b,dz) = 
		let v = a.(l + !xpos) in
		if v = -1 then
			(let c = choose m nl !s in
			a.(l + !xpos) <- c);
		step m !s a.(l+ !xpos) in
	let oy = dy b in
	a.(l+ !xpos) <- b;
	xpos := !xpos+dz;
	s := s';
	ypos := !ypos + (match dz with
		| 0 -> oy-iy
		| 1 -> oy
		| -1 -> -(dy a.(l+ !xpos))
		| _ -> failwith "bad move")

let draw col m =
	clear_graph ();
	let sy = match dy a.(l+ !xpos) with 1 -> -1 | _ -> 0 in
	let y=ref sy in
	qset !xpos cyan;
	for x= !xpos to l do
		let v = a.(x+l) in
		let d = dy v in
		(match v with
			| -1 -> qset x magenta
			| v -> pset x !y (col v));
		(match d with
		| 1 -> pset x (!y+1) (col v)
		| -1 -> pset x (!y-1) (col v)
		| _ -> ());
		y := !y + (dy v)
	done;
	if a.(l+ !xpos) = -1 then qset !xpos cyan;
	let y=ref sy in
	for x= !xpos-1 downto -l do
		let v = a.(x+l) in
		let d = dy v in
		y := !y - (dy v);
		(match v with
			| -1 -> qset x magenta
			| v -> pset x !y (col v));
		(match d with
		| 1 -> pset x (!y+1) (col v)
		| -1 -> pset x (!y-1) (col v)
		| _ -> ());
	done

let col np=function
	| 0 -> red
	| 1 -> yellow
	| 2 -> blue
	| 3 -> green
	| i -> 0x010101 * ((i*200)/(np-1))

exception Break

let go fn =
	let ir = ref false
	and oy = ref None
	and fwd = ref true
	and cs = ref 1
	and st = ref 0 in
	let fm = Rtm.load fn in
	if (List.map (lint fm) ["1";"x";"2";"y"] <> [0;1;2;3]) then failwith "Bad letters order (should be 1x2y)";
	let rm =
		let i = snames fm in
		let f = defun fm i [] in
		let m' = create_lshared (inside f) in
		revert m' f i [];
		m' in
	let np = ((letters fm)-4) in
	let col = col np
	and init = init np
	and finit = finit np
	and binit = binit np
	and step = step (letters fm) in
	init fm;
	while true do
	  try
		let m = if !fwd then fm else rm in
		draw col m;
		set_color black;
		moveto 0 0;
		draw_string (Printf.sprintf "%s #%-7i (x%i) pos:%3i,%3i step %i %s" (match !fwd with true -> "->" | false -> "<-") !s !cs !xpos !ypos !st (match !oy with None -> "" | Some y -> Printf.sprintf "<%i>" y));
		synchronize ();
		Gc.minor ();
		(match if (not !ir) || (key_pressed ()) then (!ir, read_key ()) else (false,'\000') with
			| false,'q' -> exit 0
			| false,'r' -> init m; st := 0; fwd := true
			| false,'u' -> finit m; st := 0; fwd := true
			| false,'U' -> binit m; st := 0; fwd := true
			| false,'c' -> oy := None; ir := true
			| false,'i' -> oy := Some (!ypos+1); ir := true
			| false,'d' -> oy := Some (!ypos-1); ir := true
			| false,'f' -> ir := true
			| false,'+' -> cs := 2 * !cs
			| false,'-' -> if !cs > 1 then cs := !cs / 2
			| false,'o' -> for i=1 to 1 do step m; if !fwd then incr st else decr st done
			| false,'t' -> for i=1 to 10 do step m; if !fwd then incr st else decr st done
			| false,'h' -> for i=1 to 100 do step m; if !fwd then incr st else decr st done
			| false,'m' -> for i=1 to 1000 do step m; if !fwd then incr st else decr st done
			| false,'T' -> for i=1 to 10000 do step m; if !fwd then incr st else decr st done
			| false,'H' -> for i=1 to 100000 do step m; if !fwd then incr st else decr st done
			| false,'M' -> for i=1 to 1000000 do step m; if !fwd then incr st else decr st done
			| false,'z' -> fwd := not !fwd
			| true,_ -> ir := false
			| _ ->
				try
					for i = 1 to !cs do
						step m;
						if !fwd then incr st else decr st;
						if !ir then
							(match !oy with
								| Some y -> if !ypos = y then (ir := false; oy := None; raise Break)
								| None -> ())
					done
				with Break -> ())
	  with Not_found -> ir := false
	done

let _ =
	Printf.printf "Rutrun version 0\n\n%!";
	Printexc.print go Sys.argv.(1)
