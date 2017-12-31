open Format
open X86_64
open Ast


exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr env next = function
	()


let popn n = adds (imm n) (reg rsp)
let pushn n = subs (imm n) (reg rsp)

let rec compile_expr = function
	|Cint i -> pushq (imm i)
	|Cbool b -> if b then pushq (imm 1) else pushq (imm 0)
	|Binop (o,e1,e2) ->
		compile_expr e1 ++
		compile_expr e2 ++
		popq rbx ++
		popq rax ++
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
