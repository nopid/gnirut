open Parsetree

let soi x = if x>=0 then "+" ^ (string_of_int x) else string_of_int x

let sp i = String.make i ' '

let virg st = function
	| [] -> ""
	| x::xs -> List.fold_left (fun s i -> s ^ st ^ i) x xs
	
let block (i,n,o) = (virg "," i) ^ "|" ^ (virg " " n) ^ "|" ^ (virg "," o)

let cdisplay = function
	| Code (i,Some j) -> Printf.printf "(%i) %s%s" i (sp i) (Instr.to_str j)
	| Code (i,None) -> Printf.printf "(%i) %s" i (sp i)
	| Clear -> Printf.printf "@clear"
	| Show (Some a,b,c,d) -> Printf.printf "@show %s %s %s \"%s\"" (soi a) b (soi c) d
	| Show (None,b,c,d) -> Printf.printf "@show %s %s \"%s\"" b (soi c) d
	| Info -> Printf.printf "@info"
	| Letters -> Printf.printf "@letters"
	| States -> Printf.printf "@states"
	| Load s -> Printf.printf "@load \"%s\"" s
	| Save s -> Printf.printf "@save \"%s\"" s
	| Use s -> Printf.printf "@use \"%s\"" s
	| Run -> Printf.printf "@run"
	| Link None -> Printf.printf "@link"
	| Link (Some (i,n,o)) -> Printf.printf "@link [%s>" (block (i,[n],o))
	| Dump None -> Printf.printf "@dump"
	| Dump (Some i) -> Printf.printf "@dump %s" i
	| Torus -> Printf.printf "@torus"

let display = function
	| i,None -> cdisplay i; Printf.printf "\n%!"
	| i,Some c -> cdisplay i; Printf.printf " #%s\n%!" c

let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
	while true do
		Printf.printf ">>> %!";
    	display (Gniyacc.input Gnilex.token lexbuf)
	done
  with End_of_file -> ()

let _ =
	Printf.printf "Gnirut Parser version 0\n";
	Printexc.print main ()
