
(* The type of tokens. *)

type token = 
  | WHILE
  | VEC
  | Tstring of (string)
  | Tint of (int)
  | TRUE
  | TO
  | SUPEGAL
  | SUP
  | STRUCT
  | RIGHTPAR
  | RIGHTG
  | RIGHTC
  | RETURN
  | PRINT
  | POINT
  | PLUS
  | OR
  | NOTEGAL
  | MUT
  | MOINS
  | MOD
  | LET
  | LEN
  | LEFTPAR
  | LEFTG
  | LEFTC
  | INFEGAL
  | INF
  | IF
  | IDENT of (Ast.ident)
  | FOIS
  | FN
  | FALSE
  | EXCL
  | ETMUT
  | ET
  | EOF
  | ENDLINE
  | ELSE
  | EGAL
  | DIVISE
  | DBLEGAL
  | COMMA
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val fichier: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.fichier)
