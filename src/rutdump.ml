let go s =
	let m = Rtm.load s in
	Printf.printf "# %i states\n# %i symbols\n\n" (Rtm.states m) (Rtm.letters m);
	Rtm.dump m stdout ""

let _ =
	Printf.printf "Rutdump version 0\n\n%!";
	Printexc.print go Sys.argv.(1)
