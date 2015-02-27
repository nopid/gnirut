open Printf
open Parsetree
open Instr
open Rtm

let soi x = if x>=0 then "+" ^ (string_of_int x) else string_of_int x

let sp oc i =
	for k=1 to i do
		fprintf oc "\\quad"
	done

let rp s =
	let n = ref 0 in
	for i = 0 to (String.length s) - 1 do
		incr n;
		match String.unsafe_get s i with
			| '_' -> incr n
			| _ -> ()
	done;
	let s' = String.create !n in
	n := 0;
	for i = 0 to (String.length s) - 1 do
		match String.unsafe_get s i with
			| '_' -> String.unsafe_set s' !n '\\'; incr n; String.unsafe_set s' !n '_'; incr n
			| c -> String.unsafe_set s' !n c; incr n
	done;
	s'
	
	
let ts s = "\\mbox{\\em " ^ (rp s) ^ "}"
let tl s = "\\mbox{\\tt " ^ (rp s) ^ "}"
let tf s = "\\mbox{\\rm " ^ (rp s) ^ "}"


let virg st = function
	| [] -> ""
	| x::xs -> List.fold_left (fun s i -> s ^ st ^ i) x xs
	
let block (i,n,o) = (virg "," (List.map ts i)) ^ " \\middle| " ^ (virg "~" (List.map tf n)) ^ " \\middle| " ^ (virg "," (List.map ts o))

let prmatch t =
	virg " \\mid " (Matching.mapl (fun a (b,s) -> sprintf "%s\\mbox{:}%s, %s" (tl a) (tl b) (ts s)) t)

let prbutl = function
	| [] -> ""
	| a::ls ->
		List.fold_left (fun s b -> (ts s) ^ ", " ^ (tl b)) (" \\mbox{\\sf ~but~} " ^ (tl a)) ls

let single_to_tex s = function
	| Move (Left,s') -> sprintf "%s.~\\leftarrow, %s" (ts s) (ts s')
	| Move (Right,s') -> sprintf "%s.~\\rightarrow, %s" (ts s) (ts s')
	| Move (Here,s') -> sprintf "%s.~\\downarrow, %s" (ts s) (ts s')
	| Match t -> sprintf "%s.~%s" (ts s) (prmatch t)
	| MatchRall (s', butl, t) -> sprintf "%s.~%s \\mbox{\\sf ~else~} %s%s" (ts s) (prmatch t) (ts s') (prbutl (Matching.ldel butl t))
	| MatchEall (s', a, butl, t) -> sprintf "%s.~%s \\mbox{\\sf ~else~} %s \\mbox{\\sf ~write~} %s%s" (ts s) (prmatch t) (ts s') (tl a) (prbutl (Matching.ldel butl t))

let to_tex = function
	| Single (s, i) -> single_to_tex s i
	| Forward (i,n,o) -> "\\left[" ^ (block (i,n,o)) ^ "\\right>"
	| Backward (i,n,o) -> "\\left<" ^ (block (i,n,o)) ^ "\\right]"
	| Def (i,n,o) -> "\\mbox{\\sf def~}\\left[" ^ (block (i,[n],o)) ^ "\\right>:"
	| Fun (i,n,o) -> "\\mbox{\\sf fun~}\\left[" ^ (block (i,[n],o)) ^ "\\right>:"
	| Call (false,false,i,n,o,l) -> "\\mbox{\\sf call~}\\left[" ^ (block (i,n,o)) ^ "\\right> \\mbox{\\sf from~}" ^ (virg "," (List.map tl l))
	| Call (true,false,i,n,o,l) -> "\\mbox{\\sf call~}\\left<" ^ (block (i,n,o)) ^ "\\right] \\mbox{\\sf from~}" ^ (virg "," (List.map tl l))
	| Call (false,true,i,n,o,l) -> "\\mbox{\\sf call~}\\left[" ^ (block (i,"lr"::n,o)) ^ "\\right> \\mbox{\\sf from~}" ^ (virg "," (List.map tl l))
	| Call (true,true,i,n,o,l) -> "\\mbox{\\sf call~}\\left<" ^ (block (i,"lr"::n,o)) ^ "\\right] \\mbox{\\sf from~}" ^ (virg "," (List.map tl l))


let cdisplay oc = function
	| Code (i,Some j) -> sp oc i; fprintf oc "%s" (to_tex j)
	| Code (i,None) -> sp oc i; fprintf oc ""
	| Clear -> fprintf oc "\\mbox{@clear}"
	| Show (Some a,b,c,d) -> fprintf oc "\\mbox{@show} %s %s %s \"%s\"" (soi a) b (soi c) d
	| Show (None,b,c,d) -> fprintf oc "\\mbox{@show} %s %s \"%s\"" b (soi c) d
	| Info -> fprintf oc "\\mbox{@info}"
	| Letters -> fprintf oc "\\mbox{@letters}"
	| States -> fprintf oc "\\mbox{@states}"
	| Load s -> fprintf oc "\\mbox{@load} \"%s\"" s
	| Save s -> fprintf oc "\\mbox{@save} \"%s\"" s
	| Use s -> fprintf oc "\\mbox{@use} \"%s\"" s
	| Run -> fprintf oc "\\mbox{@run}"
	| Dump None -> fprintf oc "\\mbox{@dump}"
	| Dump (Some i) -> fprintf oc "\\mbox{@dump} %s" (tf i)
	| Torus -> fprintf oc "\\mbox{@torus}"
	| Link None -> fprintf oc "\\mbox{@link}"
	| Link (Some (i,n,o))-> fprintf oc "\\mbox{@link} \\left[%s\\right>" (block (i,[n],o))

let tdisplay oc l i =
	fprintf oc "$\\null\\llap{\\tiny %i\\kern 1.5em}" l;
	cdisplay oc i

let display oc l = function
	| i,None -> tdisplay oc l i; fprintf oc "$\\\\\n%!"
	| i,Some c -> tdisplay oc l i; fprintf oc "${\\small\\verb+#%s+}\\\\\n%!" c

let go s =
	let ic = open_in s in
	let ns = 
		if Array.length Sys.argv = 3 then
			Sys.argv.(2)
		else
			let s' = Filename.basename s in
			(if Filename.check_suffix s' ".gni" then Filename.chop_suffix s' ".gni" else s') ^ ".tex" in
	let oc = open_out ns in
    let lexbuf = Lexing.from_channel ic 
	and l = ref 0 in
  try
	while true do
		incr l;
    	display oc !l (Gniyacc.input Gnilex.token lexbuf)
	done
  with End_of_file -> close_in ic; close_out oc

let _ =
	Printf.printf "Gnirut PrettyPrinter version 0\n%!";
	Printexc.print go Sys.argv.(1)
