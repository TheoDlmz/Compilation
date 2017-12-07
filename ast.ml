type ident = string
type mutident = bool*ident

type tipe =
   Tident of ident
 | Ttyid of ident*tipe
 | Ttype of tipe
 | Ttypemut of tipe


type bop = Equiv | Diff | Inf | Infeg | Sup | Supeg | Add | Sub | Times | Div | Mod | And | Or | Egal

type uop = Neg | Not | Star | And | Mutand 

type expr = 
   Cint of int 
 | Cbool of bool 
 | Cident of ident 
 | Binop of bop*expr*expr 
 | Unop of uop*expr
 | Cselect of expr*ident 
 | Clen of expr 
 | Ctab of expr*expr
 | Ccall of ident*( expr list) 
 | Cvec of (expr list) 
 | Fprint of string 
 | Cbloc of block 
 | Cexpr of expr

and isexpr = None | Eexpr of expr

and block = {instruction : instr list(*; expression : isexpr*)}

and ift = 
   If1 of expr*block 
 | If2 of expr*block*block 
 | If3 of expr*block*ift

and instr = 
   Inone 
 | Iexpr of expr 
 | Iinit of mutident*expr
 | IinitS of mutident*ident*((ident*expr) list) 
 | Iwhile of expr*block 
 | Ireturn of isexpr 
 | Iif of ift

type argument = {nom : mutident; typ : tipe}

type dfun = {nom : ident; args : argument list; typ : tipe; bloc : block}

type dstruct = {nom : ident; struc : ident*tipe list}

type decl = Dfun of  dfun | Dstruct of dstruct

type fichier = decl list
