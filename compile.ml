open Format
open X86_64
open Ast


exception VarUndef of string


module Smap = Map.Make(String)

let label_count = ref 0


type structs = (tipe Smap.t) Smap.t


let structlist (i,t) S = Smap.add i t S

let segment_donnes = ref nop
let string_count = ref 0

let rec alloc_expr env next = function
	|Cint i -> Cst i, next
	|Cbool b -> Cbool b, next
	|Cident x -> begin try 
			let ofs_x = Smap.find x env in
				Cident ofs_x,next
		      with Not_found -> raise (VarUndef x)
	|Binop(o,e1,e2) -> let e1,fpmax1 = alloc_expr env next e1 in
				let e2,fpmax2 = alloc_expr env next e2 in
				Binop(o,e1,e2), max fpmax1 fpmax2
	|Unop(u,e1) -> let e1,fpmax1 = alloc_expr env next e1 in Unop(u,e1), fpmax1
	|Cselect(e,x) -> let e,fpmax1 = alloc_expr env next e in (* verifier que x est bien dans la structure de e*)
			 Cselect(e,x),fpmax1
	|

let alloc_struct env next struct =
	Smap.add struct.nom (List.fold_left structlist Smap.empty) structs

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
		
