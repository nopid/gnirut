%{
open Printf
open Instr
open Parsetree
open Rtm
%}

%token <string> COMMENT
%token <string> STR
%token <int> NUM
%token <string> ID
%token <int> INDENT
%token LBRACKET
%token RBRACKET
%token BAR
%token COLON
%token COMMA
%token <Rtm.dir> DIR
%token LT
%token GT
%token DOT
%token DEF
%token FUN
%token NEWLINE
%token DOCLEAR
%token DOSHOW
%token DOINFO
%token DOLETTERS
%token DOSTATES
%token DOLOAD
%token DOLINK
%token DOSAVE
%token DOUSE
%token DORUN
%token DODUMP
%token DOTORUS
%token CALL
%token FROM
%token EOF
%token ELSE
%token BUT
%token WRITE
%token LR

%start input
%type <Parsetree.cmd * string option> input

%%
input: { Code (0,None), None }
	| go COMMENT { $1, Some $2 }
	| go NEWLINE { $1, None }
	| go EOF { $1, None }
	| NEWLINE { Code (0,None), None }
	| COMMENT { Code (0,None), Some $1 }
	| INDENT NEWLINE { Code($1,None), None }
	| INDENT COMMENT { Code($1,None), Some $2 }
	| EOF { raise End_of_file }

go: line { Code (0,$1) }
	| INDENT line { Code ($1,$2) }
	| FUN LBRACKET params BAR ID BAR params GT COLON { Code (0, Some (Fun ($3,$5,$7))) }
	| DOCLEAR { Clear }
	| DOSHOW NUM ID NUM STR { Show (Some $2,$3,$4,$5) }
	| DOSHOW ID NUM STR { Show (None,$2,$3,$4) }
	| DOINFO { Info }
	| DOLETTERS { Letters }
	| DOSTATES { States }
	| DOLOAD STR { Load $2 }
	| DOSAVE STR { Save $2 }
	| DOUSE STR { Use $2 }
	| DORUN { Run }
	| DODUMP ID { Dump (Some $2) }
	| DODUMP { Dump None }
	| DOTORUS { Torus }
	| DOLINK LBRACKET params BAR ID BAR params GT { Link (Some ($3,$5,$7)) }
	| DOLINK { Link None }
;

line:   
        | DEF LBRACKET params BAR ID BAR params GT COLON { Some (Def ($3,$5,$7)) }
		| ID DOT DIR COMMA ID { Some (Single ($1, Move ($3, $5))) }
		| ID DOT matches { Some (Single ($1, Match (Matching.of_list $3))) }
		| ID DOT matches ELSE ID butl { Some (Single ($1, MatchRall ($5, $6, Matching.of_list $3))) }
		| ID DOT matches ELSE ID WRITE ID butl { Some (Single ($1, MatchEall ($5, $7, $8, Matching.of_list $3))) }
		| LBRACKET params BAR words BAR params GT { Some (Forward ($2,$4,$6)) }
		| LBRACKET params BAR LR words BAR params GT { Some (Forward ($2,"lr"::$5,$7)) }
		| LT params BAR words BAR params RBRACKET { Some (Backward ($2,$4,$6)) }
		| LT params BAR LR words BAR params RBRACKET { Some (Backward ($2,"lr"::$5,$7)) }
		| CALL LBRACKET params BAR words BAR params GT FROM params { Some (Call (false,false,$3,$5,$7,$10)) }
		| CALL LT params BAR words BAR params RBRACKET FROM params { Some (Call (true,false,$3,$5,$7,$10)) }
		| CALL LBRACKET params BAR LR words BAR params GT FROM params { Some (Call (false,true,$3,$6,$8,$11)) }
		| CALL LT params BAR LR words BAR params RBRACKET FROM params { Some (Call (true,true,$3,$6,$8,$11)) }
;

butl: {[]}
	| BUT params {$2}
;

amatch: ID COLON ID COMMA ID { ($1,($3,$5)) }
;

matches: {[]}
	|  amatch mmatches { $1::$2 }
;

mmatches: {[]}
	| BAR amatch mmatches { $2::$3 }
;

words: {[]}
	| ID words {$1::$2}
;

params: {[]}
		| ID mparams		{$1::$2}
;

mparams: {[]}
		| COMMA ID mparams  {$2::$3}
;
%%
