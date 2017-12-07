%{

	open Ast
%}

%token ELSE FALSE FN IF LET MUT RETURN STRUCT TRUE WHILE VEC PRINT
%token <int> Tint
%token <string> Tstring
%token <string> IDENT
%token INF SUP EGAL PLUS MOINS FOIS DIVISE LEN 
%token MOD AND DBLEGAL NOTEGAL INFEGAL SUPEGAL 
%token ET OR EXCL LEFTPAR RIGHTPAR TO ENDLINE ETMUT 
%token POINT LEFTG RIGHTG LEFTC RIGHTC COMMA EMUT
%token EOF

%right EGAL
%left OR
%left AND
%nonassoc DBLEGAL NOTEGAL INF INFEGAL SUP SUPEGAL
%left PLUS MOINS
%left FOIS DIVISE MOD
%nonassoc EXCL ufois umoins ET ETMUT
%nonassoc LEFTC RIGHTC
%nonassoc POINT

%start fichier
(* déclaration des tokens *)
(* précédences et associatiités *)
(* points d'entrées *)
%type <Ast.fichier> fichier

%%

fichier:
   l =  list(decl) EOF {l}
;

decl:
 |df = decl_fun {Dfun df}
 |ds = decl_struct {Dstruct ds}
;

decl_struct:
 STRUCT i = IDENT LEFTG l = list(decl_sous_struct) RIGHTG {{nom = i; struc = l}}
;

decl_sous_struct:
 x = IDENT TO t = typ {(x,t)}
;

decl_fun:
 |FN i = IDENT LEFTPAR la = list(argument) RIGHTPAR b = bloc {{nom=i;args = la;typ = none; bloc = b}}
 |FN i = IDENT LEFTPAR la = list(argument) RIGHTPAR MOINS SUP t = typ b = bloc {{nom=i;args=la;typ = t;bloc = b}}
;

typ:
 |i = IDENT {Tident i}
 |i = IDENT INF t= typ SUP {Ttyid (i,t)}
 |ET t = typ {Ttype t}
 |ET MUT t = typ {TtypeMut t}
;

argument:
 b = boption(MUT) i = IDENT TO t = typ {{nom=(b,i);typ=t}}
;

bloc:
 |LEFTG l = list(instr) e = expr RIGHTG {{instruction = l; expression = Eexpr e}}
 |LEFTG l = list(instr) RIGHTG {{instruction = l; expression = None}}
;

instr:
 |ENDLINE {Inone}
 |e = expr ENDLINE {Iexpr e}
 |LET b = boption(MUT) i = IDENT EGAL e = expr ENDLINE {Iinit ((b,i),e)}
 |LET b = boption(MUT) i = IDENT EGAL j = IDENT LEFTG l = separated_list(COMMA,sous_instr) RIGHTG ENDLINE {IinitS ((b,i),j,l)}
 |WHILE e = expr b = bloc {Iwhile (e,b)}
 |RETURN ENDLINE {Ireturn None}
 |RETURN e = expr ENDLINE {Ireturn (Eexpr e)}
 |i = ifb {Iif i}
;

sous_instr:
 i = IDENT TO e = expr {(i,e)}
;

ifb:
 |IF e = expr b = bloc {If1 (e,b)}
 |IF e = expr b = bloc ELSE b2 = bloc {If2 (e,b,b2)} 
 |IF e = expr b = bloc ELSE i = ifb {If3 (e,b,i)}
;

expr:
 |i = Tint {Cint i}
 |TRUE {Cbool true}
 |FALSE {Cbool false}
 |i = IDENT {Cident i}
 |e = expr b = binaire e2 = expr {Binop(b,e,e2)}
 |MOINS e = expr %prec umoins {Unop (Neg,e)}
 |FOIS e = expr %prec ufois {Unop (Star,e)}
 |u = unaire e = expr {Unop(u,e)}
 |e = expr POINT i = IDENT {Cselect (e,i)}
 |e = expr POINT LEN LEFTG RIGHTG {Clen e}
 |e = expr LEFTC e2 = expr RIGHTC {Ctab(e,e2)}
 |i = IDENT LEFTPAR l = separated_list(COMMA,expr) RIGHTPAR {Ccall(i,l)}
 |VEC EXCL LEFTC l = separated_list(COMMA,expr) RIGHTC {Cvec l}
 |PRINT EXCL LEFTPAR c = Tstring  RIGHTPAR {Fprint c}
 | b = bloc {Cbloc b}
 |LEFTPAR e = expr RIGHTPAR {Cexpr e}
;

binaire:
 |DBLEGAL	{Equiv}
 |NOTEGAL	{Diff}
 |INF  		{Inf}
 |INFEGAL	{InfEg}
 |SUP		{Sup}
 |SUPEGAL	{SupEg}
 |PLUS		{Add}
 |MOINS		{Sub}
 |FOIS		{Times}
 |DIVISE	{Div}
 |MOD		{Mod}
 |AND		{And}
 |OR		{Or}
 |EGAL		{Egal}
;

unaire:
 |MOINS		{Neg}
 |EXCL		{Not}
 |FOIS		{Star}
 |ET		{And}
 |ETMUT		{MutAnd}
;
