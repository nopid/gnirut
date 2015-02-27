open Rtm

let go s =
	let m = Rtm.load s in
	let s = Sys.argv.(2)
	and pos = int_of_string Sys.argv.(3)
	and tape = Sys.argv.(4) in
	show m None s pos tape

let _ =
	Printf.printf "Rutrun version 0\n\n%!";
	Printexc.print go Sys.argv.(1)
