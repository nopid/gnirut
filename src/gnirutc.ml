open Interp

let go s =
	let t = create () in
		try
			Interp.use t s stdout (fun _ -> failwith "No run in compiler");
			let ns = 
				if Array.length Sys.argv = 3 then
					Sys.argv.(2)
				else
					let s' = Filename.basename s in
					(if Filename.check_suffix s' ".gni" then Filename.chop_suffix s' ".gni" else s') ^ ".rut" in
			save t ns
		 with e -> Printf.eprintf "ERROR: %s\n%!" (Printexc.to_string e)

let _ =
	Printf.printf "Gnirutc version 0\n%!";
	Printexc.print go Sys.argv.(1)
