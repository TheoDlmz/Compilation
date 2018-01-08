%{

	open Ast
%}

%token ELSE FALSE FN IF LET MUT RETURN STRUCT TRUE WHILE VEC PRINT
%token <int> Tint
%token <string> Tstring
%token <Ast.ident> IDENT
%token INF SUP EGAL PLUS MOINS FOIS DIVISE 
%token MOD AND DBLEGAL NOTEGAL INFEGAL SUPEGAL 
%token ET OR EXCL LEFTPAR RIGHTPAR TO ENDLINE ETMUT 
%token POINT LEFTG RIGHTG LEFTC RIGHTC COMMA LEN  
%token EOF

%right EGAL
%left OR
%left AND
%nonassoc DBLEGAL NOTEGAL INF INFEGAL SUP SUPEGAL
%left PLUS MOINS
%left FOIS DIVISE MOD
%nonassoc EXCL ufois umoins ET ETMUT
%nonassoc LEFTC 
%nonassoc POINT

%start fichier
(* déclaration des tokens *)
(* précédences et associatiités *)
(* points d'entrées *)
%type <Ast.pfichier> fichier

%%

fichier:
   l =  list(decl) EOF {l}
;

decl:
 desc = decl_desc { {pd_desc = desc;pos = ($startpos,$endpos) }}
;

decl_desc:
 |df = decl_fun {PDfun df}
 |ds = decl_struct {PDstruct ds}
;

decl_struct:
 STRUCT i = IDENT LEFTG l = separated_list(COMMA,decl_sous_struct) RIGHTG {{nom_pstruct = i; val_pstruct = l}}
;

decl_sous_struct:
 x = IDENT TO t = tip {(x,t)}
;

decl_fun:
 |FN i = IDENT LEFTPAR la = list(argument) RIGHTPAR b = bloc {{nom_pfun =i;arg_pfun = la;typ_pfun = None; bloc_pfun = b}}
 |FN i = IDENT LEFTPAR la = list(argument) RIGHTPAR MOINS SUP t = tip b = bloc {{nom_pfun =i;arg_pfun =la;typ_pfun = T t;bloc_pfun = b}}
;

tip:
 t = tip_desc {{pt_desc = t;pt_pos = ($startpos,$endpos)}}
;

tip_desc:
 |i = IDENT {Tx i}
 |i = IDENT INF t= tip SUP {Tvec (i,t)}
 |ET t = tip {Tref t}
 |ET MUT t = tip {Trefmut t}
;

argument:
 b = boption(MUT) i = IDENT TO t = tip {{nom_arg =(b,i);typ_arg=t}}
;

bloc:
 LEFTG b = blockbody  RIGHTG {b }
 | LEFTG RIGHTG { EmptyBloc}
;

blockbody:
i = instr b = blockbody {B (i,b)}
|i = instr  {I i}
|e  = expr {E e}
;

instr:
 desc = instr_desc { {pi_desc = desc;pi_pos = ($startpos, $endpos)} }
;

instr_desc:
 |ENDLINE {PInone}
 |e = expr ENDLINE {PIexpr e}
 |LET b = boption(MUT) i = IDENT EGAL e = expr ENDLINE {PIinit ((b,i),e)}
 |LET b = boption(MUT) i = IDENT EGAL j = IDENT LEFTG l = separated_list(COMMA,sous_instr) RIGHTG ENDLINE {PIinitStruct ((b,i),j,l)}
 |WHILE e = expr b = bloc {PIwhile (e,b)}
 |RETURN ENDLINE {PIend}
 |RETURN e = expr ENDLINE {PIreturn e}
 |i = ifb {PIif i}
;

sous_instr:
 i = IDENT TO e = expr {i,e}
;

ifb:
 |IF e = expr b = bloc {PIfThen (e,b)}
 |IF e = expr b = bloc ELSE b2 = bloc {PIfElse (e,b,b2)} 
 |IF e = expr b = bloc ELSE i = ifb {PIfElseIf (e,b,i)}
;

expr:
desc = expr_desc {{pe_desc = desc; pe_pos = ($startpos,$endpos)}}
;

expr_desc:
 |i = Tint {PEint i}
 |TRUE {PEbool true}
 |FALSE {PEbool false}
 |i = IDENT {PEident i}
 |e = expr b = binaire e2 = expr {PEbinop (b,e,e2)}
 |MOINS e = expr %prec umoins {PEunop (Neg,e)}
 |FOIS e = expr %prec ufois {PEunop (Star,e)}
 |u = unaire e = expr {PEunop(u,e)}
 |e = expr POINT i = IDENT {PEselect (e,i)}
 |e = expr POINT LEN LEFTPAR RIGHTPAR {PElen e}
 |e = expr LEFTC e2 = expr RIGHTC {PEtab(e,e2)}
 |i = IDENT LEFTPAR l = separated_list(COMMA,expr) RIGHTPAR {PEcall(i,l)}
 |VEC EXCL LEFTC l = separated_list(COMMA,expr) RIGHTC {PEvec l}
 |PRINT EXCL LEFTPAR c = Tstring  RIGHTPAR {PEprint c}
 | b = bloc {PEbloc b}
 |LEFTPAR e = expr RIGHTPAR {PEexpr e}
;




%inline binaire:
 |DBLEGAL	{Equiv}
 |NOTEGAL	{Diff}
 |INF  		{Inf}
 |INFEGAL	{Infeg}
 |SUP		{Sup}
 |SUPEGAL	{Supeg}
 |PLUS		{Add}
 |MOINS		{Sub}
 |FOIS		{Times}
 |DIVISE	{Div}
 |MOD		{Mod}
 |AND		{And}
 |OR		{Or}
 |EGAL		{Egal}
;

%inline unaire:
 |EXCL		{Not}
 |ET		{Ref}
 |ETMUT		{MutRef}
;
