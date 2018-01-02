open Format
open X86_64
open Ast


exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

let label_count = ref 0

type local_env = ident Smap.t

let rec alloc_expr env next = function
	()


let popn n = adds (imm n) (reg rsp)
let pushn n = subs (imm n) (reg rsp)

let rec compile_expr expr = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
		match expr with
	|Cint i -> pushq (imm i)
	|Cbool b -> if b then pushq (imm 1) else pushq (imm 0)
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
		|Egal -> 
	|Unop (u,e) -> compile_expr e ++ popq rax ++
			(match u with
			   |Neg -> negq (reg rax)
			   |Not -> notq (reg rax)
			   |Star ->
			   |Ref -> 
			   |RefMut ->
			   ) ++ pushq (reg rax)
	|Cselect (e,x) -> 
	|Clen e -> compile_expr e++
		   popq (reg rbx) ++
		   movl (ind 8 rbx) (reg rax)
	|Ctab (e1,e2) ->
	|Ccall (x,l) ->
	|Cvec l -> 
	|Fprint s ->
	|Cbloc b -> compile_bloc b
	|Cexpr e -> compile_expr e
	
and let rec compile_bloc = function
	|EmptyBloc -> ()
	|E e -> compile_expr e
	|I i -> compile_instr i
	|B (i,b) -> compile_instr i ++ compile_bloc b
	
and let rec compile_instr instr = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
	match instr with
	|Inone -> ()
	|Iexpr e -> compile_expr e
	|Iinit (x,e) ->
	|IinitS (x,y,l) ->
	|Iwhile (e,b) ->
		label ("w_deb_"^label_string) ++
		compile_expr e ++
		popq rax ++
		testq (reg rax) (reg rax) ++
		jz ("w_end_"^label_string) ++
		compile_bloc b ++
		jmp ("w_deb_"^label_string) ++
		label ("w_end_"^label_string)
	|Ireturn e ->
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

