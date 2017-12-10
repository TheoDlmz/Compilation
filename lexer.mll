{
open Lexing
open Ast
open Parser
exception Lexing_error of string
exception Eof

let keyword_list = 
[ "else", ELSE;
  "false", FALSE;
  "fn", FN;
  "if", IF;
  "let", LET;
  "mut", MUT;
  "return", RETURN;
  "struct", STRUCT;
  "true", TRUE;
  "while", WHILE;
  "vec", VEC;
  "print", PRINT]


type token = 
 |Tstring of string
 |Tint of int
 |Teof 
}


let chiffre = ['0'-'9']+
let entier = chiffre+
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha(alpha|chiffre|'_')*

rule token = parse
 |'\n' {new_line lexbuf;token lexbuf}
 |[' ' '\t'] {token lexbuf}
 |['0'-'9']+ as i {Tint (int_of_string i)}
 |"//"  {comment_line lexbuf}
 |"/*" { multicomment lexbuf; token lexbuf}
 |'"' {Tstring (parse_string lexbuf); token lexbuf}
 |"len" {LEN}
 |ident as s {let s = Lexing.lexeme lexbuf in
		try List.assoc s keywords
		with Not_found -> IDENT s}
 |"<" {INF}
 |">" {SUP}
 |"=" {EGAL}
 |"+" {PLUS}
 |"-" {MOINS}
 |"*" {FOIS}
 |"/" {DIVISE}
 |"%" {MOD}
 |"&&" {AND}
 |"==" {DBLEGAL}
 |"!=" {NOTEGAL}
 |"<=" {INFEGAL}
 |">=" {SUPEGAL}
 |"&" {ET}
 |"&mut" {ETMUT}
 |"||" {OR}
 |"!" {EXCL}
 |"(" {LEFTPAR}
 |")" {RIGHTPAR}
 |":" {TO}
 |";" {ENDLINE}
 |"," {COMMA}
 |"." {POINT}
 |"{" {LEFTG}
 |"}" {RIGHTG}
 |"[" {LEFTC}
 |"]" {RIGHTC}
 |eof {EOF}
 |_ { raise Lexing_error "Illegal caracter at position : line "^string_of_int(lexbuf.lex_curr_p.pos_lnum)^"; column "^string_of_int(lexbuf.lex_curr_p.pos_cnum)}

and comment_line = parse
 |'\n' {token lexbuf}
 |_ { comment_line lexbuf}
 | eof {EOF}

and multicomment = parse
 |"*)" {()}
 |"(*" {multicomment lexbuf; multicomment lexbuf;}
 |_ {multicomment lexbuf}
 |eof { raise Lexing_error "commentaire non terminé"}

and parse_string = parse
 |'"' {()}
 |"\\\\" {"\\"^(parse_string lexbuf)}
 |"\\n" {"\n"^(parse_string lexbuf)}
 |"\\\"" {"\""^(parse_string lexbuf)}
 |['\\' '"'] {raise Lexing_error "caractère illicite"}
 |_ as s {s^(parse_string lexbuf)}
 |eof {Lexing_error "chaîne de caractère non terminée"}

