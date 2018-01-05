type typ =
  | Tunit
  | Ti32
  | Tbool
  | Tstruct of typ list
  | Tvec of typ
  | Tpointer of bool*typ
  
type bop = Equiv | Diff | Inf | Infeg | Sup | Supeg | Add | Sub | Times | Div | Mod | And | Or | Egal

type uop = Neg | Not | Star | Ref | MutRef

type ident = string

type mutident = bool*ident

type tipe =
   Tx of ident
 | Tvec of ident*tipe
 | Tref of tipe
 | Trefmut of tipe


type istipe = 
 |None
 |T of tipe

type pexpr = {
	pe_desc: pexpr_desc; pe_pos : Lexing.position * Lexing.position }

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


and pblock = B of pinstr*pblock |I of pinstr |E of pexpr |EmptyBloc

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
   TEint of int * typ
 | TEbool of bool * typ
 | TEident of ident * typ
 | TEbinop of bop*texpr*texpr * typ
 | TEunop of uop*texpr* typ
 | TEselect of texpr*ident * typ
 | TElen of texpr * typ
 | TEtab of texpr*texpr* typ
 | TEcall of ident*( texpr list) * typ
 | TEvec of (texpr list) * typ
 | TEprint of string * typ
 | TEbloc of tblock * typ
 | TEexpr of texpr* typ


and tblock = TB of tinstr*tblock | TI of tinstr | TE of texpr | TEmptyBloc

and tif =
   TIfThen of texpr*tblock
 | TIfElse of texpr*tblock*tblock
 | TIfElseIf of texpr*tblock*tif

and tinstr =
   TInone
 | TIexpr of texpr
 | TIinit of mutident*texpr
 | TIinitStruct of mutident*ident*((ident*texpr) list)
 | TIwhile of texpr*tblock
 | TIend
 | TIreturn of texpr
 | TIif of tif

type targument = {nom : mutident; typ : typ}

type tdfun = {nom : ident; args : targument list; typ : typ; bloc : tblock}

type tdstruct = {nom : ident; struc : (ident*typ) list}

type tdecl = TDfun of  tdfun | TDstruct of tdstruct
type tfichier = tdecl list
