open Interp

let main () =
	let lexbuf = Lexing.from_channel stdin 
	and pt = ref false 
	and t = create () in
	try
		while true do
		  try
			Printf.printf "%s %!" (if !pt then "..." else ">>>");
			pt := Interp.eat t lexbuf stdout (fun _ -> failwith "Not implemented yet!")
		  with 
			| End_of_file -> raise End_of_file
			| e -> Printf.eprintf "ERROR: %s\n%!" (Printexc.to_string e)
		done
	with 
	| End_of_file -> ()

let _ =
	Printf.printf "Gnirut version 0\n%!";
	Printexc.print main ()
