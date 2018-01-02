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
  "print", PRINT;
   "len", LEN]


}
let chiffre = ['0'-'9']
let entier = chiffre+
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha(alpha|chiffre|'_')*

rule token = parse
 |'\n' {new_line lexbuf;token lexbuf}
 |[' ' '\t'] {token lexbuf}
 |['0'-'9']+ as i {Tint (int_of_string i)}
 |"//"  {comment_line lexbuf}
 |"/*" { multicomment lexbuf; token lexbuf}
 |'"' {Tstring (parse_string lexbuf)}
 |ident as id {let s = Lexing.lexeme lexbuf in
		try List.assoc s keyword_list
		with Not_found -> IDENT id}
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
 |_ as r{ raise (Lexing_error  (Printf.sprintf "Illegal caracter %C at position : line %d; column %i" r lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_p.pos_cnum))}

and comment_line = parse
 |'\n' {token lexbuf}
 |_ { comment_line lexbuf}
 | eof {EOF}

and multicomment = parse
 |"*/" {()}
 |"/*" {multicomment lexbuf; multicomment lexbuf;}
 |_ {multicomment lexbuf}
 |eof { raise (Lexing_error (Printf.sprintf "commentaire non terminé at position : line %d; column %i" lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_p.pos_cnum))}

and parse_string = parse
 |'"' {""}
 |"\\\\" {"\\"^(parse_string lexbuf)}
 |"\\n" {"\n"^(parse_string lexbuf)}
 |"\\\"" {"\""^(parse_string lexbuf)}
 |['\\' '"'] {raise (Lexing_error  (Printf.sprintf "caractère illicite at position : line %d; column %i" lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_p.pos_cnum))}
 |_ as s {(String.make 1 s)^(parse_string lexbuf)}
 |eof {raise (Lexing_error  (Printf.sprintf "chaîne de caractère non terminée at position : line %d; column %i" lexbuf.lex_curr_p.pos_lnum lexbuf.lex_curr_p.pos_cnum))}

