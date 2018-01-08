open Ast





let pp_bop = function
	|Equiv -> "=="
	|Diff -> "!="
	|Inf -> "<"
	|Infeg -> "<="
	|Sup -> ">"
	|Supeg -> ">="
	|Add -> "+"
	|Sub -> "-"
	|Times -> "x"
	|Div -> "/"
	|Mod -> "mod"
	|And -> "&&"
	|Or -> "||"
	|Egal -> "<-"

let pp_uop = function
	|Neg -> "-"
	|Not -> "not "
	|Star -> "*"
	|Ref -> "&"
	|MutRef -> "&mut"

let rec pp_i i = match i.pi_desc with
	|PInone -> ()
	|PIexpr e -> begin print_string "PIexpr :"; pp_e e; print_newline() end
	|PIinit (m,e) -> begin print_string "PIinit :"; print_newline(); pp_e e; print_newline() end
	|PIinitStruct (m,i,l) -> begin print_string "PIinitStruct :";
					List.iter pp_se l; print_newline(); end
	|PIwhile (e,b) -> begin print_string "PIwhile :"; pp_e e; print_newline(); pp_b b;
				print_newline();end
	|PIend -> begin print_string "return";print_newline() end
	|PIreturn e-> begin print_string "PIreturn"; pp_e e; print_newline() end
	|PIif i -> begin print_string "PIif"; pp_if i; print_newline() end

and pp_se (s,e) = 
	begin print_string s; print_string ":"; pp_e e end
and pp_e e = match e.pe_desc with
	|PEint i -> print_int i
	|PEbool b -> print_string "boolean"
	|PEident x -> print_string x
	|PEbinop (b,e1,e2) -> begin print_string "("; pp_e e1; print_string (pp_bop b); pp_e e2; print_string ")" end
	|PEunop (u,e) -> begin print_string (pp_uop u); pp_e e end
	|PEselect (e,x) -> begin pp_e e; print_string "."; print_string x end
	|PElen e -> begin pp_e e; print_string ".len()" end
	|PEtab (e1,e2) -> begin pp_e e1; print_string "["; pp_e e2; print_string "]" end
	|PEcall (f,l) -> begin print_string f; print_string "(flemme)" end
	|PEvec l -> print_string "flemme"
	|PEprint s -> begin print_string "PRINT :"; print_string s end
	|PEbloc b -> pp_b b
	|PEexpr e -> pp_e e


and pp_if = function
	|PIfThen (e,b) -> begin print_string "If :"; print_newline(); pp_e e; print_newline();
				print_string "THen :";print_newline (); pp_b b end
	|PIfElse (e,b1,b2) -> begin print_string "If"; print_newline(); pp_e e; print_newline();
				print_string "THen"; print_newline(); pp_b b1; print_newline();
				print_string "else"; print_newline(); pp_b b2 end
	|PIfElseIf (e,b1,i) -> begin print_string "If"; print_newline(); pp_e e; print_newline();
				print_string "THen"; print_newline(); pp_b b1; print_newline();
				print_string "Else"; print_newline();pp_if i end
and  pp_b = function
	B (i,b) -> begin pp_i i; pp_b b; end
	|I i -> pp_i i
	|E e -> pp_e e
	|EmptyBloc -> ()


let pp_s_aux (s,t) = 
	begin
	print_string s;
	print_newline ();
	end


let pp_f pf = begin
	print_string pf.nom_pfun;
	print_newline ();
	pp_b pf.bloc_pfun;
	end


let pp_s ps = begin
	print_string ps.nom_pstruct;
	print_newline ();
	List.iter pp_s_aux ps.val_pstruct;
	print_newline ();
	end
	

let pp_decl pd = match pd.pd_desc with
	|PDfun pf -> pp_f pf
	|PDstruct ps -> pp_s ps

let pprint p = List.iter pp_decl p
