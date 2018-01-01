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

let rec compile_expr expr = label_count := !label_count + 1; match expr with
	|Cint i -> pushq (imm i)
	|Cbool b -> if b then pushq (imm 1) else pushq (imm 0)
	|Binop (o,e1,e2) ->
		let code1 = compile_expr e1 in
		let code2 = compile_expr e2 in
		let label_string = string_of_int !label_count in
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
		 code1 + code2 ++
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
	|Clen e -> compile_expr e++
		   popq (reg rbx) ++
		   movl (ind 8 rbx) (reg rax)
	|Cexpr e -> compile_expr e
	|
