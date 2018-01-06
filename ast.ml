type ident = string

module Smap = Map.Make(String)

type varstate = 
  | Empty
  | Full
  | Borrowed of bool

type bop = 
   Equiv 
 | Diff 
 | Inf
 | Infeg
 | Sup
 | Supeg
 | Add
 | Sub
 | Times
 | Div
 | Mod
 | And
 | Or
 | Egal

type uop = 
   Neg 
 | Not
 | Star
 | Ref
 | MutRef


type mutident = bool*ident


type tipe_desc =
   Tx of ident
 | Tvec of ident*tipe
 | Tref of tipe
 | Trefmut of tipe

and tipe = { pt_desc : tipe_desc; pt_pos : Lexing.position*Lexing.position}



type istipe = 
 |None
 |T of tipe

type pexpr = {	pe_desc: pexpr_desc; pe_pos : Lexing.position * Lexing.position }

and pexpr_desc = 
   PEint of int 
 | PEbool of bool 
 | PEident of ident 
 | PEbinop of bop*pexpr*pexpr 
 | PEunop of uop*pexpr
 | PEselect of pexpr*ident 
 | PElen of pexpr 
 | PEtab of pexpr*pexpr
 | PEcall of ident*( pexpr list) 
 | PEvec of (pexpr list) 
 | PEprint of string 
 | PEbloc of pblock 
 | PEexpr of pexpr


and pblock = 
  B of pinstr*pblock
 |I of pinstr
 |E of pexpr
 |EmptyBloc

and pif = 
   PIfThen of pexpr*pblock 
 | PIfElse of pexpr*pblock*pblock 
 | PIfElseIf of pexpr*pblock*pif

and pinstr = {pi_desc : pinstr_desc; pi_pos : Lexing.position * Lexing.position}

and pinstr_desc = 
   PInone 
 | PIexpr of pexpr 
 | PIinit of mutident*pexpr
 | PIinitStruct of mutident*ident*((ident*pexpr) list) 
 | PIwhile of pexpr*pblock 
 | PIend
 | PIreturn of pexpr 
 | PIif of pif

type argument = {nom : mutident; typ : tipe}

type pdfun = {nom : ident; args : argument list; typ : istipe; bloc : pblock}

type pdstruct = {nom : ident; struc : (ident*tipe) list}

type pdecl_desc = PDfun of  pdfun | PDstruct of pdstruct

type pdecl = {pd_desc : pdecl_desc; pos : Lexing.position*Lexing.position}

type pfichier = pdecl list


(*********)

type texpr =
   TEint of int 
 | TEbool of bool
 | TEident of ident*int (*on veut la taille de l'ident*)
 | TEbinop of bop*texpr*texpr
 | TEunop of uop*texpr
 | TEselect of texpr*int(*position*)*int(*size*)
 | TElen of texpr
 | TEtab of texpr*texpr*int(*taille des elements du tableau*)
 | TEcall of ident*( texpr list)
 | TEvec of (texpr list)*int(*taille des elements du tableau*)
 | TEprint of string 
 | TEbloc of tblock
 | TEexpr of texpr


and tblock = 
   TB of tinstr*tblock
 | TI of tinstr
 | TE of texpr
 | TEmptyBloc

and tif =
   TIfThen of texpr*tblock
 | TIfElse of texpr*tblock*tblock
 | TIfElseIf of texpr*tblock*tif

and tinstr =
   TInone
 | TIexpr of texpr
 | TIinit of ident*texpr*int (*taille de x*)
 | TIinitStruct of ident*((texpr*int(*position*)*int(*taille de e.x*)) Smap.t)*int (*taille totale*)
 | TIwhile of texpr*tblock
 | TIend
 | TIreturn of texpr
 | TIif of tif

type targument = {nom : ident; size: int}

type tdfun = {nom : ident; targs : targument list; size:int;bloc : tblock}

(* Probablement mettre dans le typer : type tdstruct = {nom : ident; tstruct : (int(*position*)*int(*size*)) Smap.t; tsize : int}
*)
(*
type tdecl = TDfun of  tdfun | TDstruct of tdstruct
*)
type tfichier = tdfun list (* En gros ton fichier c'est une liste de fonction car tu as verifié que les structutres étaient correctes
et comme on les ecrit pas dans le code X86 j'en ai pas besoin moi, j'ai juste besoin quand je rencontre :
  - Une initialisation de structure let x = voiture {passager = 4; garanti= true} : il me faut la taille de chaque expression comme ça
    je les empiles correctement sur le tas
  - Une selection peugeot.passager, il me faut la taille et la position de l'expression "passager" comme ça je le récupère direct
Pareil pour les tableaux et les idents en eux memes !*)


(**********************************)

type cexpr =
  |Cint of int
  |Cbool of bool
  |Cident of int*int
  |Cbinop of binop*cexpr*cexpr
  |Cunop of unop*cexpr
  |Cselect of cexpr*int*int
  |Clen of cexpr
  |Ctab of cexpr*cexpr*int
  |Ccall of ident*(cexpr list)
  |Cvec of (cexpr list)*int
  |Cprint of string
  |Cbloc of cbloc
  
and cbloc =
  |CB of cinstr*cbloc
  |CI of cinstr
  |CE of cexpr
  |CEmptyBloc
 
 and cinstr =
  |CInone
  |CIexpr of cexpr
  |CIinit of ident*cexpr*int
  |CIinitStruct of ident*((cexpr*int (*position*)*int (*taille de e.x*)) Smap.t)*int (*taille totale*)
  |CIwhile of cexpr*cbloc
  |CIreturn of cexpr
  |CIend
  |CIif oc cif
  
and cif =
  |CifThen of cexpr*cbloc
  |CifElse of cexpr*cbloc*cbloc
  |CifElseIf of cexpr*cbloc*cif
  
 let cfun = {nom : ident; targs: targument list; size:int; lbloc : cbloc}
 
 let cfichier = cfun list
 
 

