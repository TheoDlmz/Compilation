open Format
open X86_64
open Ast


exception VarUndef of string


module Smap = Map.Make(String)

let label_count = ref 0

let segment_donnes = ref nop

let string_count = ref 0

let rec alloc_expr env next = function
	|TEint i -> Cint i, next
	|TEbool b -> Cbool b, next
	|TEident (x,size) -> begin try
			let ofs_x = Smap.find x env in
				Cident (ofs_x,size),next
		      with Not_found -> raise (VarUndef x)
	|TEbinop(o,e1,e2) -> let e1,fpmax1 = alloc_expr env next e1 in
				let e2,fpmax2 = alloc_expr env next e2 in
				Cbinop(o,e1,e2), max fpmax1 fpmax2
	|TEunop(u,e1) -> let e1,fpmax1 = alloc_expr env next e1 in Cunop(u,e1), fpmax1
	|TEselect(e,pos,size) -> let e,fpmax1 = alloc_expr env next e in  Cselect(e,pos,size),fpmax1
	|TElen e -> let e,fpmax = alloc_expr env next e in Clen(e), fpmax
	|TEtab (e1,e2,size) -> let e1,fpmax1 = alloc_expr env next e1 in
				let e2,fpmax2 = alloc_expr env next e2 in
				Ctab(e1,e2,size), max fpmax1 fpmax2
	|TEcall (x,l) -> let l, fpmax =
      			List.fold_left  (fun (l, fpmax) e -> let e, fpmax' = alloc_expr env next e in
         		 				e::l, max fpmax fpmax') 
					([], next) l in
				Ccall (x,List.rev l), fpmax
	|TEvec (l,size) -> let l, fpmax =
      			List.fold_left  (fun (l, fpmax) e -> let e, fpmax' = alloc_expr env next e in
         		 				e::l, max fpmax fpmax') 
					([], next) l in
				(Cvec ((List.rev l),size), fpmax
	|TEprint s -> Cprint s
	|TEbloc b -> let b, fpmax = alloc_bloc env next b in Cbloc b,fpmax
	|TEexpr e -> let e, fpmax = alloc_expr env next e in e,fpmax

and rec alloc_bloc env next b = match b with
	|TB (i,b) -> let i,fpmaxi,newenv = alloc_instr env next i in
			let b, fpmaxb = alloc_bloc newenv next b in
			 CB (i,b), max fpmaxi fpmaxb
	|TI i -> let i,fpmax,newenv = alloc_instr env next i in CI i,fpmax
	|TE e -> let e,fpmax = alloc_expr env next e in CE e,fpmax
	|TEmptyBloc -> CEmptyBloc, next

and rec alloc_instr env next i = match i with
	|TInone -> CInone, next, env
	|TIexpr e -> let e,fpmax = alloc_expr env next e in CIexpr,fpmax
	|TIinit (x, e, size) -> let e,fpmax = alloc_expr env next e in
			 let next = next + size in
			  CIinit (x, e, size),max fpmax next,(Smap.add x (-next) env)
	|TIinitStruct (x, env, size) ->  let newenv, fpmax =
      			Smap.iter  (fun x (e,i,j) (s,fpmax) -> let e, fpmax' = alloc_expr env next e in
         		 				 (Smap.add x (e,i,j) s, max fpmax fpmax')) 
					(Smap.empty, next) l in
				let next = next + size  in
				CIinitStruct ( x, newenv,size),max fpmax next, (Smap.add x (-next) env)
	|TIwhile (e,b) -> let e,fpmax1 = alloc_expr env next e in
			  let b, fpmax2 = alloc_bloc env next b in
			   CIwhile (e,b), max fpmax1 fpmax2,env
	|TIend -> CIend, next,env
	|TIreturn e -> let e,fpmax = alloc_expr env next e in CIreturn e,fpmax, env
	|TIif i -> let i,fpmax = alloc_if env next e in CIif i,fpmax, env
	
and rec alloc_if env next i = match i with
	|TifThen (e,b) -> let e,fpmaxe = alloc_expr env next e in 
			   let b,fpmaxb = alloc_bloc env next b in
			    CifThen(e,b), max fpmaxe fpmaxb
	|TifElse (e,b1,b2) -> let e,fpmaxe = alloc_expr env next e in 
			   let b1,fpmaxb1 = alloc_bloc env next b1 in
			   let b2,fpmaxb2 = alloc_bloc env next b2 in
			   CifElse(e,b1,b2), max (max fpmaxb1 fpmaxb2) fpmaxe
	|TifElseIf (e,b,i) -> let e, fpmaxe = alloc_expr env next e in
				let b,fpmaxb = alloc_bloc env next b in
				 let i,fpmaxi = alloc_if env next i in
				  CifElseIf(e,b,i), max (max fpmaxb fpmaxe) fpmaxi


let alloc_fun f = 
	let b, fpmax = alloc_bloc (Smap.empty) 0 f.bloc in ({nom = f.nom; targs = f.targs; lbloc = b},fpmax)

let alloc fichier = List.map (alloc_fun) fichier

let popn n = adds (imm n) (reg rsp)
let pushn n = subs (imm n) (reg rsp)

let rec compile_expr expr = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
		match expr with
	|Cint i -> pushq (imm i)
	|Cbool b -> if b then pushq (imm 1) else pushq (imm 0)
	|Cident fp_x -> pushq (ind ~ofs:fp_x rbp)
	|Binop (o,e1,e2) ->
		let code1 = compile_expr e1 in
		let code2 = compile_expr e2 in
		(match o with
		  | (Inf |Infeg| Sup| Supeg) ->
			let abr = (match op with
				|Inf -> jl
				|Infeg -> jle
				|Sup -> jg
				|Supeg -> jge) in
			code1 ++
			code2 ++
			popq rbx ++
			popq rax ++
			cmpq (reg rbx) (reg rax) ++
			abr ("l1_"^label_string) ++
			pushq (imm 0) ++
			jmp ("l0_"^label_string) ++
			label ("l1_"^label_string) ++
			pushq (imm 1) ++
			label ("l0_"^label_string)
		  | (Equiv |Diff) -> 
		   	let abr = (match op with
				|Equiv -> jz
				|Diff -> jnz) in
		  	code1 ++
			code2 ++
			popq rbx ++
			popq rax ++
			subq (reg rbx) (reg rax) ++
			abr ("l1_"^label_string) ++
			pushq (imm 0) ++
			jmp ("l0_"^label_string) ++
			label ("l1_"^label_string) ++
			pushq (imm 1) ++
			label ("l0_"^label_string)
		 | (Add |Sub |Times|Div |And|Or|Mod) -> 
		 code1 + code2 ++ popq rbx ++ popq rax ++
		(match o with
			|Add -> addq (reg rbx) (reg rax)
			|Sub -> subq (reg rbx) (reg rax)
			|Times -> imulq (reg rbx) (reg rax)
			|Div -> cqto ++ idivq (reg rbx)
			|And -> andq (reg rbx) (reg rax)
			|Or -> orq (reg rbx) (reg rax)
			|Mod -> cqto ++ idivq (reg rbx)
				++ movq (reg rdx) (reg rax))
		++ pushq (reg rax)
		|Egal -> (* WTFUCKKKKKKKKKKKKKKKKKKKKKK *)
	|Unop (u,e) -> compile_expr e ++ popq rax ++
			(match u with
			   |Neg -> negq (reg rax)
			   |Not -> notq (reg rax)
			   |Star ->
			   |Ref -> 
			   |RefMut ->
			   ) ++ pushq (reg rax)
	|Cselect (e,x) -> (* e.x  chiaaaaanttt *)
	|Clen e -> compile_expr e++
		   popq (reg rbx) ++
		   movl (ind ~ofs:8 rbx) (reg rax)
	|Ctab (e1,e2) -> compile_expr e2 ++
			 compile_expr e1 ++
			 popq rax ++
			 movl (ind ~ofs:16 rax) (reg rbx) ++
			 (* doit recuperer la taille du type des elems de e1 puis multiplier par e2 et le renvoyer *)
			 
	|Ccall (x,l) -> (*appel de fonction*) (* on mets tous les args dans la pile *) ++ call x ++ (* on desallou la pile *)
	|Cvec l -> let n = List.length l (* construit le vecteur l, n est son premier element et n*t l'espace alloué sur le tas *)
	|Fprint s -> begin
			string_count := !string_count + 1;
			segment_donnes := !segment_donnes ++ label ("chaine_"^string_of_int(string_count)) ++ string (s(*^"\\n"*));
			mov (imm s) (reg rdi) ++
			movq (imm 0) (reg rax) ++
			call printf
		      end
	|Cbloc b -> compile_bloc b
	|Cexpr e -> compile_expr e
	
and let rec compile_bloc = function
	|EmptyBloc -> nop
	|E e -> compile_expr e
	|I i -> compile_instr i
	|B (i,b) -> compile_instr i ++ compile_bloc b
	
and let rec compile_instr instr = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
	match instr with
	|Inone -> nop
	|Iexpr e -> compile_expr e
	|Iinit (ofs_x,e) -> compile_expr e ++ popq rax
			++ movq (reg rax) (ind ~ofs:fp_x rbp)
	|IinitS (x,y,l) -> (* INIT STRUCT CHIANT *)
	|Iwhile (e,b) ->
		label ("w_deb_"^label_string) ++
		compile_expr e ++
		popq rax ++
		testq (reg rax) (reg rax) ++
		jz ("w_end_"^label_string) ++
		compile_bloc b ++
		jmp ("w_deb_"^label_string) ++
		label ("w_end_"^label_string)
	|Iend -> pushq (imm 0) (reg rax) ++ ret 
	|Ireturn e -> compile_expr e ++ popq rax ++ ret
	|Iif i -> compile_if i
	
and let rec compile_if i = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
	compile_expr e ++
			popq rax ++
			testq (reg rax) (reg rax) ++
	(match i with
	|If1 (e,b) -> 
			jz ("if_end_"^label_string) ++
			compile_bloc b ++
			label ("if_end_"^label_string)
	|If2 (e,b1,b2) -> 
			jz ("if_else_"^label_string) ++
			compile_bloc b1 ++
			jmp ("if_end_"^label_string) ++
			label ("if_else_"^label_string)  ++
			compile_bloc b2 ++
			label ("if_end_"^label_string) 
	|If3 (e,b1,i2) -> 
			jz ("if_else_"^label_string) ++
			compile_bloc b1 ++
			jmp ("if_end_"^label_string) ++
			label ("if_else_"^label_string)  ++
			compile_if i2 ++
			label ("if_end_"^label_string))


	



let compile_decl d = match d with
	|Dfun df -> match df.nom with
		"Main" -> (label "main" ++ compile_bloc df.bloc,nop)
		|_ -> (* on recupere les arguments sur la pile blabla *)
			(nop,label df.nom ++ compile_bloc df.bloc)
			(* on désalloue la pile ou jspas quoi *)
	|Dstruct ds -> (nop,nop) (**)



let compile_program p ofile =
	let f = alloc f in
	let (codemain, codefuns) = List.fold_left compile_decl  (nop, nop) f in
		let p =
		{ text = globl "main" ++ codemain ++
		movq (imm 0) (reg rax) ++
		ret ++
		codefuns;
		data = 	!segment_donnes
		
