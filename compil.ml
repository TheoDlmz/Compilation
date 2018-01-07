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
		end
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
				Cvec ((List.rev l),size), fpmax
	|TEprint s -> Cprint s, next
	|TEbloc b -> let b, fpmax = alloc_bloc env next b in Cbloc b,fpmax
	|TEexpr e -> let e, fpmax = alloc_expr env next e in e,fpmax

and alloc_bloc env next b = match b with
	|TB (i,b) -> let i,fpmaxi,newenv = alloc_instr env next i in
			let b, fpmaxb = alloc_bloc newenv next b in
			 CB (i,b), max fpmaxi fpmaxb
	|TI i -> let i,fpmax,newenv = alloc_instr env next i in CI i,fpmax
	|TE e -> let e,fpmax = alloc_expr env next e in CE e,fpmax
	|TEmptyBloc -> CEmptyBloc, next

and alloc_instr env next i = match i with
	|TInone -> CInone, next, env
	|TIexpr e -> let e,fpmax = alloc_expr env next e in CIexpr e,fpmax,env
	|TIinit (x, e, size) -> let e,fpmax = alloc_expr env next e in
			 let next = next + size in
			  CIinit (-next, e, size),max fpmax next,(Smap.add x (-next) env)
	|TIinitStruct (x, structenv, size) ->  let newenv, fpmax =
      			Smap.fold (fun x (e,i,j) (s,fpmax) -> let e, fpmax' = alloc_expr env next e in
         		 				 (Smap.add x (e,i,j) s, next)) 
					structenv (Smap.empty, next)  in
				let next = next + size  in
				CIinitStruct ( -next, newenv,size),max fpmax next, (Smap.add x (-next) env)
	|TIwhile (e,b) -> let e,fpmax1 = alloc_expr env next e in
			  let b, fpmax2 = alloc_bloc env next b in
			   CIwhile (e,b), max fpmax1 fpmax2,env
	|TIend -> CIend, next,env
	|TIreturn e -> let e,fpmax = alloc_expr env next e in CIreturn e,fpmax, env
	|TIif i -> let i,fpmax = alloc_if env next i in CIif i,fpmax, env
	
and alloc_if env next i = match i with
	|TIfThen (e,b) -> let e,fpmaxe = alloc_expr env next e in 
			   let b,fpmaxb = alloc_bloc env next b in
			    CifThen(e,b), max fpmaxe fpmaxb
	|TIfElse (e,b1,b2) -> let e,fpmaxe = alloc_expr env next e in 
			   let b1,fpmaxb1 = alloc_bloc env next b1 in
			   let b2,fpmaxb2 = alloc_bloc env next b2 in
			   CifElse(e,b1,b2), max (max fpmaxb1 fpmaxb2) fpmaxe
	|TIfElseIf (e,b,i) -> let e, fpmaxe = alloc_expr env next e in
				let b,fpmaxb = alloc_bloc env next b in
				 let i,fpmaxi = alloc_if env next i in
				  CifElseIf(e,b,i), max (max fpmaxb fpmaxe) fpmaxi


let alloc_fun f = 
	let env, next, l = List.fold_left (fun (env, next, largs) x -> let next = next + x.arg_size in
			(Smap.add x.nom next env, next, (-next,x.arg_size)::largs))  (Smap.empty, 8, []) (f.targs) in
		let b, fpmax = alloc_bloc env next f.bloc in ({nom = f.nom; cargs = List.rev l;size = f.size; lbloc = b},fpmax)

let alloc fichier = List.map (alloc_fun) fichier

let popn n = addq (imm n) (reg rsp)
let pushn n = subq (imm n) (reg rsp)

let rec compile_expr expr = label_count := !label_count + 1;  
(* Manque binop(egal), Select, Unop (star, ref, refMut), tab, call, vec *)
	let label_string = string_of_int !label_count in
		match expr with
	|Cint i -> 
		pushq (imm i),8
	|Cbool b -> 
		(if b then pushq (imm 1) else pushq (imm 0)),8
	|Cident (fp_x,size) -> let p = (size/8 - 1) and forcode = ref nop in begin
		for i = 0 to p
		do
			forcode := !forcode ++ pushq (ind ~ofs:(fp_x + i*8)  rbp) 
		done; 
		!forcode,size ;
		end
	|Cbinop (o,e1,e2) ->
		let code1,p1 = compile_expr e1 in
		let code2,p2 = compile_expr e2 in
		(match o with
		  | (Inf |Infeg| Sup| Supeg) ->
			(let abr = (match o with
				|Inf -> jl
				|Infeg -> jle
				|Sup -> jg
				|Supeg -> jge
				|_ -> failwith "impossible") in
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
			label ("l0_"^label_string)),8
		  | (Equiv |Diff) -> 
		   	(let abr = (match o with
				|Equiv -> jz
				|Diff -> jnz
				|_-> failwith "impossible") in
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
			label ("l0_"^label_string)),8
		 | (Add |Sub |Times|Div |And|Or|Mod) -> 
		 (code1 ++ code2 ++ popq rbx ++ popq rax ++
		(match o with
			|Add -> addq (reg rbx) (reg rax) 
			|Sub -> subq (reg rbx) (reg rax)
			|Times -> imulq (reg rbx) (reg rax)
			|Div -> cqto ++ idivq (reg rbx)
			|And -> andq (reg rbx) (reg rax)
			|Or -> orq (reg rbx) (reg rax)
			|Mod -> cqto ++ idivq (reg rbx)
				++ movq (reg rdx) (reg rax)
			|_ -> failwith "impossible")
		++ pushq (reg rax)),8
		|Egal -> (let coderef,p1 = compile_expr (Cunop(Ref,e)) in 
				coderef ++
				popq rax ++
				code2 ++
				popq rbx ++
				nop),0
	|Cunop (u,e) -> match u with
			|Neg|Not -> (fst(compile_expr e) ++ popq rax ++
			(match u with
			   |Neg -> negq (reg rax)
			   |Not -> notq (reg rax)
			   |_ -> failwith "erreur"
			  ) ++ pushq (reg rax)),8
			   |Star -> nop,0
			   |Ref -> nop ,0
			   |MutRef -> nop,0
			   
	|Cselect (e,pos,size) -> (fst(compile_expr e) ++ nop),size
	|Clen e -> (fst(compile_expr e) ++
		   popq rbx ++
		   movl (ind ~ofs:8 rbx) (reg rax) ++
		   pushq (reg rax)),8
	|Ctab (e1,e2,size) -> 
			 (fst(compile_expr e2) ++
			 fst(compile_expr e1) ++
			 popq rax ++ (*taille = inutile *)
			 popq rax ++ (*adresse des elements *)
			 popq rbx ++ (* # de l'élement *) 
			 (* doit recuperer la taille du type des elems de e1 puis multiplier par e2 et le renvoyer *)
			nop ),size
	|Ccall (f,l) -> 
		List.fold_left (fun code e -> code ++ compile_expr e) nop l ++
				call f ++ popn (8*List.length l) ++ pushq (reg rax)
				(*appel de fonction*) 
				(* on mets tous les args dans la pile *) 
				(* on desallou la pile *)
	|Cvec (l,size) -> let n = List.length l in
			pushq (imm n) ++
		    (* construit le vecteur l, n est son premier element et n*t l'espace alloué sur le tas *)
	|Cprint s -> (begin
			string_count := !string_count + 1;
			segment_donnes := !segment_donnes ++ label ("chaine_"^string_of_int(!string_count)) ++ string (s(*^"\\n"*));
			movq (ilab s) (reg rdi) ++
			movq (imm 0) (reg rax) ++
			call "printf"
		      end),0
	|Cbloc b -> compile_bloc b
	
and compile_bloc = function
	|CEmptyBloc -> nop,0
	|CE e -> compile_expr e
	|CI i -> compile_instr i
	|CB (i,b) -> let (c,size) = compile_bloc b in (fst(compile_instr i) ++ c),size
	
and compile_instr instr = label_count := !label_count + 1; 

	let label_string = string_of_int !label_count in
	match instr with
	|CInone -> nop,0
	|CIexpr e -> fst(compile_expr e),0
	|CIinit (fp_x,e) -> (let (code,size) = compile_expr e in
				(let forcode = ref nop and p = (size/8 - 1) in begin
				for i = 0 to p do
					forcode := !forcode ++ popq rax ++ movq (reg rax) (ind ~ofs:(fp_x +8*(p-i)) rbp)
				done ;
				code ++ !forcode;
				end),0
	|CIinitStruct (fp_x,structenv,size) ->  (Smap.fold (fun x (e,x_pos) precode -> 
					let (code,x_size) = compile_expr e in 
					let forcodebis = ref nop 
					and pbis = (x_size/8-1) in begin
					for i = 0 to pbis do
						forcodebis := !forcodebis ++ popq rax ++ 
						movq (reg rax) (ind ~ofs:(fp_x + x_pos + 8*(pbis-i)) rbp)
					done;
					precode ++ code ++ !forcodebis; 
					end) structenv nop),0
	|CIwhile (e,b) -> let (c,size) = compile_bloc b in
		(label ("w_deb_"^label_string) ++
		fst(compile_expr e) ++
		popq rax ++
		testq (reg rax) (reg rax) ++
		jz ("w_end_"^label_string) ++
		c ++
		jmp ("w_deb_"^label_string) ++
		label ("w_end_"^label_string)),size
	|CIend -> (movq (imm 0) (reg rax) ++ ret),0
	|CIreturn e -> let (c,size) = compile_expr e in (c ++ popq rax ++ ret),size
	|CIif i -> compile_if i
	
and compile_if i = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
	(match i with
	|CifThen (e,b) -> let (c,size) = compile_bloc b in
	(fst(compile_expr e) ++
			popq rax ++
			testq (reg rax) (reg rax) ++
			jz ("if_end_"^label_string) ++
			c ++
			label ("if_end_"^label_string)),size
	|CifElse (e,b1,b2) -> let (c1,size1) = compile_bloc b in
				let (c2,size2) = compile_bloc b in
	(fst(compile_expr e) ++
			popq rax ++
			testq (reg rax) (reg rax) ++
			jz ("if_else_"^label_string) ++
			c1 ++
			jmp ("if_end_"^label_string) ++
			label ("if_else_"^label_string)  ++
			c2 ++
			label ("if_end_"^label_string)),size1
	|CifElseIf (e,b1,i2) -> let c1,size1 = compile_bloc b in 
	(compile_expr e ++
			popq rax ++
			testq (reg rax) (reg rax) ++
			jz ("if_else_"^label_string) ++
			c1 ++
			jmp ("if_end_"^label_string) ++
			label ("if_else_"^label_string)  ++
			fst(compile_if i2) ++
			label ("if_end_"^label_string)),size1


	



let compile_fun (codemain, codefuns) (df,fpmax) =
(* a refaire *)
	match df.nom with
		"Main" -> (codemain ++ label "main" ++ pushq (reg rbp)
					++ movq (reg rsp) (reg rbp)
					++ fst(compile_bloc df.lbloc),
			    codefuns)
		|_ -> (* on recupere les arguments sur la pile blabla *)
			(codemain, (*let forcode = ref nop and p = (fp.size/8 - 1) in begin
				for i = 0 to*)
			let codebloc,size = compile_bloc df.lbloc in
			codefuns ++
			label df.nom ++
			pushq (reg rbp) ++
			movq (reg rsp) (reg rbp) ++
			pushn fpmax ++
			codebloc ++
			
			popq rax ++
			popn fpmax ++
			popq rbp ++
			ret)
			(* on désalloue la pile ou jspas quoi *)




let compile_program f ofile =
	let f = alloc f in
	let (codemain, codefuns) = List.fold_left compile_fun  (nop, nop) f in
		let p =
		{ text = glabel "main" ++ codemain ++
		movq (imm 0) (reg rax) ++
		ret ++
		codefuns;
		data = 	!segment_donnes  }
in
let f = open_out ofile in
let fmt = formatter_of_out_channel f in
X86_64.print_program fmt p;
fprintf fmt "@?";
close_out f
