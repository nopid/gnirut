{
	open Gniyacc
	
	exception Bad_Char
	
	let indenting = ref true
} 

let id = ['.''a'-'z''A'-'Z''0'-'9''_''\'']*['a'-'z''A'-'Z''0'-'9''_''\'']
let spaces = [' ''\t']*

rule token = parse
   | spaces as i { if !indenting then (indenting := false; INDENT (String.length i)) else token lexbuf }
   | "def" { indenting := false; DEF }
   | "fun" { indenting := false; FUN }
   | "but" { indenting := false; BUT }
   | "else" { indenting := false; ELSE }
   | "write" { indenting := false; WRITE }
   | "from" { indenting := false; FROM }
   | "call" { indenting := false; CALL }
   | "lr" { indenting := false; LR }
   | "@clear" { indenting := false; DOCLEAR }
   | "@show" { indenting := false; DOSHOW }
   | "@info" { indenting := false; DOINFO }
   | "@letters" { indenting := false; DOLETTERS }
   | "@states" { indenting := false; DOSTATES }
   | "@load" { indenting := false; DOLOAD }
   | "@save" { indenting := false; DOSAVE }
   | "@use" { indenting := false; DOUSE }
   | "@run" { indenting := false; DORUN }
   | "@dump" { indenting := false; DODUMP }
   | "@torus" { indenting := false; DOTORUS }
   | "@link" { indenting := false; DOLINK }
   | id as i { indenting := false; ID i }
   | '+'(['0'-'9']+ as i) { indenting := false; NUM (int_of_string i) }
   | ('-'['0'-'9']+ as i) { indenting := false; NUM (int_of_string i) }
   | '"' ([^'"']* as s) '"' {  indenting := false; STR s }
   | "[" { indenting := false; LBRACKET }
   | "]" { indenting := false; RBRACKET }
   | "|" { indenting := false; BAR }
   | ":" { indenting := false; COLON }
   | "," { indenting := false; COMMA }
   | "<-" { indenting := false; DIR Rtm.Left }
   | "->" { indenting := false; DIR Rtm.Right }
   | "<" { indenting := false; LT }
   | ">" { indenting := false; GT }
   | '.' { indenting := false; DOT }
   | '#' ([^'\n']* as s) '\n' { indenting := true; COMMENT s }
   | '\n' { indenting := true; NEWLINE }
   | '\r' { token lexbuf }
   | _ { raise Bad_Char }
   | eof { EOF }

{}