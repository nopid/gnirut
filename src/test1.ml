open Rtm

let info m =
	Printf.printf "\n  states: %i\n  letters: %i\n\n" (states m) (letters m)

let gs m s =
	try
		sname m s
	with Not_found -> Printf.sprintf "#%i" s

let runit m a s pos =
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
		while true do
			pr ();
			tick ()
		done
	with Not_found -> ()


let _ =
	let m = create () in
	add m "s" (Match (Matching.single ("1",("1","s'")))); (* s. 1:1,s' *)
 	add m "s'" (Move (Right,"s''")); (* s'. ->,s'' *)
	add m "s''" (Match (Matching.add ("0",("0","s'")) (Matching.single ("1",("2","t"))))); (* s''. 0:0,s' | 1:2,t *)
	phantom m "t";
	info m;
	dump m stdout "  ";
	Printf.printf "\n";
	runit m [|"0";"1";"0";"0";"1";"0"|] "s" 1;
	Printf.printf "\n";
	Printf.printf "Saving machine...\n%!";
	save m "test1.rut";
	Printf.printf "\nLoading back machine...\n%!";
	let m' = load "test1.rut" in
	info m';
	dump m' stdout "  ";
	Printf.printf "\n";
	runit m' [|"0";"1";"0";"0";"1";"0"|] "s" 1;
	Printf.printf "\n";
	Printf.printf "Defining as function [s|test|t>\n";
	let sf = defun m ["s"] ["t"] in
	Printf.printf "\nAdding [enter|test|paf> to empty\n";
	let m2 = create_lshared m in
	copy m2 sf ["enter"] ["paf"];
	info m2;
	dump m2 stdout "  ";
	Printf.printf "\n";
	runit m2 [|"0";"1";"0";"0";"1";"0"|] "enter" 1;
	Printf.printf "\nAdding <paf|test|blop] to it\n";
	revert m2 sf ["blop"] ["paf"];
	info m2;
	dump m2 stdout "  ";
	Printf.printf "\n";
	runit m2 [|"0";"1";"0";"0";"1";"0"|] "enter" 1;
	Printf.printf "\n";
	Printf.printf "Saving machine...\n%!";
	phantom m2 "blop";
	save m2 "test2.rut";
	Printf.printf "\nLoading back machine...\n%!";
	let m' = load "test2.rut" in
	info m';
	dump m' stdout "  ";
	Printf.printf "\n";
	runit m' [|"0";"1";"0";"0";"0";"1";"0"|] "enter" 1;
	Printf.printf "\n";	
	Printf.printf "\nAdding [a|lr test|b> to empty\n";
	let m2 = create_lshared m in
	lr m2 sf ["a"] ["b"];
	info m2;
	dump m2 stdout "  ";
	Printf.printf "\n";
	runit m2 [|"0";"1";"0";"0";"1";"0"|] "a" 4;

	Printf.printf "\nDefining [s|tonext|t>\n%!";
	let m = create () in
	add m "s" (Match (Matching.single ("x",("x","loop"))));
 	add m "loop" (Move (Right,"paf"));
	add m "paf" (Match (Matching.add ("_",("_","loop")) (Matching.single ("x",("x","t")))));
	phantom m "t";
	dump m stdout "  ";
	Printf.printf "\n";
	let tonext = defun m ["s"] ["t"] in
	Printf.printf "\nDefining [s|back|t>\n%!";
	let m' = create_lshared m in
	lr m' tonext ["s"] ["t"];
	phantom m' "t";
	dump m' stdout "  ";
	Printf.printf "\n";
	let back = defun m' ["s"] ["t"] in
	Printf.printf "\nDefining [s|test1|z,p>\n%!";
	let m' = create_lshared m in
 	add m' "s" (Move (Right,"test1"));
	add m' "test1" (Match (Matching.add ("_",("_","pa")) (Matching.single ("x",("x","za")))));
 	add m' "za" (Move (Left,"z"));
 	add m' "pa" (Move (Left,"p"));
	phantom m' "z";
	phantom m' "p";
	dump m' stdout "  ";
	Printf.printf "\n";
	let test1 = defun m' ["s"] ["z";"p"] in
	Printf.printf "\nDefining [s|test2|z,p>\n%!";
	let m' = create_lshared m in
	copy m' tonext ["s"] ["t"];
	copy m' test1 ["t"] ["za";"pa"];
	copy m' back ["za"] ["z"];
	copy m' back ["pa"] ["p"];
	phantom m' "z";
	phantom m' "p";
	dump m' stdout "  ";
	Printf.printf "\n";
	let test2 = defun m' ["s"] ["z";"p"] in
	Printf.printf "Testing [s|compose test1 test2|zz,zp,pz,pp>\n\n%!";
	let m2 = create_lshared m in
	copy m2 test1 ["start"] ["z";"p"];
	copy m2 test2 ["z"] ["zz";"zp"];
	copy m2 test2 ["p"] ["pz";"pp"];
	runit m2 [|"_";"x";"_";"_";"x";"x";"_";"_"|] "start" 1;
	Printf.printf "\n";
	runit m2 [|"_";"x";"_";"_";"x";"_";"x";"_"|] "start" 1;
	Printf.printf "\n";
	runit m2 [|"_";"x";"x";"x";"_";"_";"_";"_"|] "start" 1;
	Printf.printf "\n";
	runit m2 [|"_";"x";"x";"_";"_";"_";"x";"_"|] "start" 1;
	Printf.printf "\n";
	Printf.printf "Testing <zero,positive|test2|start]\n\n%!";
	let m2 = create_lshared m in
	revert m2 test2 ["start"] ["zero";"positive"];
	runit m2 [|"_";"x";"_";"_";"x";"x";"_";"_"|] "zero" 1;
	Printf.printf "\n";
	runit m2 [|"_";"x";"_";"_";"x";"x";"_";"_"|] "positive" 1;
	Printf.printf "\n";
