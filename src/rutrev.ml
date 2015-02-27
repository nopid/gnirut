open Rtm

let go s =
	let ns = 
		if Array.length Sys.argv = 3 then
			Sys.argv.(2)
		else
			let s' = Filename.basename s in
			(if Filename.check_suffix s' ".rut" then Filename.chop_suffix s' ".rut" else s') ^ "-rev.rut" in
	let n = load s in
	let i = snames n in
	let f = defun n i [] in
	let m = create_lshared (inside f) in
	revert m f i [];
	save m ns

let _ =
	Printf.printf "Rutrev version 0\n\n%!";
	Printexc.print go Sys.argv.(1)
