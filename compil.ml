open Format
open X86_64
open Ast


exception VarUndef of string


module Smap = Map.Make(String)


let ret_fun = (Hashtbl.create 16:(string,int) Hashtbl.t)

let label_count = ref 0

let segment_donnes = ref nop

let string_count = ref 0


(* ALLOCATION D'EXPRESSION *)

let rec alloc_expr env next = function
 |TEint i -> 
   Cint i, next

 |TEbool b -> 
   Cbool b, next

 |TEident (x,size) -> 
 begin 
  try
  let ofs_x = Smap.find x env in
   Cident (ofs_x,size),next
  with Not_found -> raise (VarUndef x)
 end

 |TEbinop(o,e1,e2) -> 
   let e1,fpmax1 = alloc_expr env next e1 in
   let e2,fpmax2 = alloc_expr env next e2 in
    Cbinop(o,e1,e2), max fpmax1 fpmax2

 |TEunop(u,e1) -> 
   let e1,fpmax1 = alloc_expr env next e1 in 
    Cunop(u,e1), fpmax1

 |TEderef(u,e,i) -> 
   let e,fpmax = alloc_expr env next e in 
    Cderef(u,e,i), fpmax

 |TEselect(e,pos,size) -> 
   let e,fpmax1 = alloc_expr env next e in  
    Cselect(e,pos,size),fpmax1

 |TElen e -> 
   let e,fpmax = alloc_expr env next e in 
    Clen(e), fpmax

 |TEtab (e1,e2,size) -> 
   let e1,fpmax1 = alloc_expr env next e1 in
   let e2,fpmax2 = alloc_expr env next e2 in
    Ctab(e1,e2,size), max fpmax1 fpmax2

 |TEcall (x,l) -> 
   let l, fpmax =
    List.fold_left  (fun (l, fpmax) e -> 
		     let e, fpmax' = alloc_expr env next e in
         	     e::l, max fpmax fpmax') 
		    ([], next) l in
   Ccall (x,List.rev l), fpmax

 |TEvec (l,size) -> 
   let l, fpmax =
    List.fold_left  (fun (l, fpmax) e -> 
 		     let e, fpmax' = alloc_expr env next e in
         	     e::l, max fpmax fpmax') 
		     ([], next) l in
   Cvec ((List.rev l),size), fpmax

 |TEprint s -> 
   Cprint s, next

 |TEbloc b -> 
   let b, fpmax = alloc_bloc env next b in 
    Cbloc b,fpmax

 |TEexpr e -> 
   let e, fpmax = alloc_expr env next e in 
    e,fpmax



(* Allocation du Bloc *)

and alloc_bloc env next b = match b with
 |TB (i,b) -> 
   let i,fpmaxi,newenv = alloc_instr env next i in
   let b, fpmaxb = alloc_bloc newenv next b in
     CB (i,b), max fpmaxi fpmaxb

 |TI i -> 
   let i,fpmax,newenv = alloc_instr env next i in 
     CI i,fpmax

 |TE e -> 
   let e,fpmax = alloc_expr env next e in 
     CE e,fpmax

 |TEmptyBloc -> 
     CEmptyBloc, next


(* Allocation d'instruction *)

and alloc_instr env next i = match i with
 |TInone -> 
    CInone, next, env
 |TIexpr e -> 
   let e,fpmax = alloc_expr env next e in 
    CIexpr e,fpmax,env

 |TIinit (x, e, size) -> 
   let e,fpmax = alloc_expr env next e in
   let next = next + size in
     CIinit (-next, e),max fpmax next,(Smap.add x (-next) env)

 |TIinitStruct (x, structenv,size) ->  
   let newenv, fpmax =
   Smap.fold (fun x (e,i) (s,fpmax) -> 
      let e, fpmax' = alloc_expr env next e in
      (Smap.add x (e,i) s, next)) 
              structenv (Smap.empty, next)  in
   let next = next + size  in
     CIinitStruct ( -next, newenv,size),
		   max fpmax next, 
		  (Smap.add x (-next) env)

 |TIwhile (e,b) -> 
   let e,fpmax1 = alloc_expr env next e in
   let b, fpmax2 = alloc_bloc env next b in
     CIwhile (e,b), max fpmax1 fpmax2,env

 |TIend -> 
   CIend, next,env

 |TIreturn e -> 
   let e,fpmax = alloc_expr env next e in 
    CIreturn e,fpmax, env

 |TIif i -> 
   let i,fpmax = alloc_if env next i in 
    CIif i,fpmax, env


(* ALlocation if*)

and alloc_if env next i = match i with
 |TIfThen (e,b) -> 
   let e,fpmaxe = alloc_expr env next e in 
   let b,fpmaxb = alloc_bloc env next b in
    CifThen(e,b), max fpmaxe fpmaxb

 |TIfElse (e,b1,b2) -> 
   let e,fpmaxe = alloc_expr env next e in 
   let b1,fpmaxb1 = alloc_bloc env next b1 in
   let b2,fpmaxb2 = alloc_bloc env next b2 in
    CifElse(e,b1,b2), max (max fpmaxb1 fpmaxb2) fpmaxe

 |TIfElseIf (e,b,i) -> 
   let e, fpmaxe = alloc_expr env next e in
   let b,fpmaxb = alloc_bloc env next b in
   let i,fpmaxi = alloc_if env next i in
    CifElseIf(e,b,i), max (max fpmaxb fpmaxe) fpmaxi


(* Alloc fun *)

let alloc_fun f = 
 let env, next, l = List.fold_left 
 		    (fun (env, next, largs) x -> let next = next + x.size_targ in
		    (Smap.add x.nom_targ next env, next, (next,x.size_targ)::largs))  
                    (Smap.empty, f.size_tfun, []) (f.arg_tfun) in
 let b, fpmax = alloc_bloc env 0 f.bloc_tfun in begin
   Hashtbl.add ret_fun f.nom_tfun f.size_tfun;
   ({nom_cfun = f.nom_tfun; arg_cfun = List.rev l; bloc_cfun = b},
    (fpmax,(next-f.size_tfun)));
  end


(* Alloc fichier *)
let alloc fichier = List.map (alloc_fun) fichier





let popn n = addq (imm n) (reg rsp)
let pushn n = subq (imm n) (reg rsp)
(* cmov cf simon c/c les sets *)






(* COMPILE EXPRESSION : FAIT :D *)

let rec compile_expr expr fpmax = label_count := !label_count + 1;  
	let label_string = string_of_int !label_count in
		match expr with
 |Cint i -> 
   pushq (imm i),8

 |Cbool b -> 
   (if b then pushq (imm 1) else pushq (imm 0)),8

 |Cident (fp_x,size) -> 
   let p = (size/8 - 1) and forcode = ref nop in begin
   for i = 0 to p
   do
	forcode := !forcode ++ pushq (ind ~ofs:(fp_x + i*8)  rbp) 
   done; 
  !forcode,size ;
  end

 |Cbinop (o,e1,e2) ->
   let code1,p1 = compile_expr e1 fpmax in
   let code2,p2 = compile_expr e2 fpmax in
   (match o with
    |(Inf|Infeg|Sup|Supeg) ->
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

    |(Equiv|Diff) -> 
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

    |(Add|Sub|Times|Div|Mod) -> 
      (code1 ++ 
       code2 ++ 
       popq rbx ++ 
       popq rax ++
       (match o with
	|Add -> addq (reg rbx) (reg rax) 
	|Sub -> subq (reg rbx) (reg rax)
	|Times -> imulq (reg rbx) (reg rax)
	|Div -> cqto ++ idivq (reg rbx)
	|Mod -> cqto ++ idivq (reg rbx)
		++ movq (reg rdx) (reg rax)
	|_ -> failwith "impossible")
      ++ pushq (reg rax)),8
    |And -> 
	(code1 ++
	popq rax ++
	movq (imm 0) (reg rbx) ++
	testq (reg rax) (reg rax) ++
	jz ("lfin_"^label_string) ++
	code2 ++
	popq rax ++
	movq (imm 0) (reg rbx) ++
	testq (reg rax) (reg rax) ++
	jz ("lfin_"^label_string) ++
	movq (imm 1) (reg rbx) ++
	label ("lfin_"^label_string) ++
	pushq (reg rbx)),8
    |Or ->  
	(code1 ++
	popq rax ++
	movq (imm 1) (reg rbx) ++
	testq (reg rax) (reg rax) ++
	jnz ("lfin_"^label_string) ++
	code2 ++
	popq rax ++
	movq (imm 1) (reg rbx) ++
	testq (reg rax) (reg rax) ++
	jnz ("lfin_"^label_string) ++
	movq (imm 0) (reg rbx) ++
	label ("lfin_"^label_string) ++
	pushq (reg rbx)),8
    |Egal -> 
     (let coderef,size = compile_expr (Cunop(Ref,e1)) fpmax in 
      let p = (p2/8 - 1) and forcode = ref nop in begin
      for i = 0 to p
      do
	forcode := popq rbx ++ movq (reg rbx) (ind ~ofs:(i*8)  rax) ++ !forcode 
      done; 
      code2 ++
      coderef ++
      popq rax ++
      !forcode end),0)

 |Cunop (u,e) -> 
  (match u with
   |Neg|Not -> 
   (fst(compile_expr e fpmax) ++ 
    popq rax ++
    (match u with
     |Neg -> negq (reg rax)
     |Not -> notq (reg rax)
     |_ -> failwith "erreur"
     ) 
    ++ pushq (reg rax)),8
   |Ref |MutRef ->  
    (match e with
     |Cderef(u,e1,size) -> compile_expr e1 fpmax
     |Cident (fp_x,size) -> (movq (reg rbp) (reg rax) ++ 
			     addq (imm fp_x) (reg rax) ++ 
			     pushq (reg rax)),8
     |Cselect (e1,pos,size) -> nop,8
     |Ctab (e1,e2,size) -> (fst(compile_expr e1 fpmax) ++ 
			   popq rax ++ 
			   fst(compile_expr e2 fpmax) ++ 
			   popq rbx ++
			   popq rax ++ 
       			   imulq (imm (size/8)) (reg rbx) ++
			   addq (reg rbx) (reg rax) ++
			   pushq (reg rax)),8
     |_ -> failwith "not a good ref")
    |_ -> failwith "not a good unary"
			)
 |Cderef (u,e,size) -> 	
   let p = (size/8 - 1) and forcode = ref nop in begin
   for i = 0 to p
   do
	forcode := !forcode ++ pushq (ind ~ofs:(i*8)  rax) 
   done; 
   fst(compile_expr e fpmax) ++ 
   popq rax ++ 
   !forcode,size ;	
   end

 |Cselect (e,pos,size) -> 
  let code,total_size = compile_expr e fpmax in
  let movselect = ref nop and
      popselect = ref nop in begin
  for i = 0 to (size/8-1) do
      movselect := !movselect ++ 
		   movq (ind ~ofs:(-total_size+pos+i*8) rsp) (reg rax) ++
		   movq (reg rax) (ind ~ofs:(-total_size + i*8) rsp)
  done;
  for i = 0 to ((total_size-size)/8 - 1) do
      popselect := !popselect ++
		   popq (rax)
  done;
  code ++
  !movselect ++
  !popselect, size;
  end
  (*let code,total_size = compile_expr e fpmax
					and firstpop = ref nop 
				        and choose = ref nop 
					and lastpop = ref nop 
					and pushcode = ref nop in
   begin
   for i = 0 to (pos/8 - 1)
   do
	firstpop := !firstpop ++ popq rax
   done;
   for i = 0 to (size/8 - 1) 
   do
	choose := !choose ++ popq rax ++ movq (reg rax) (ind ~ofs:(i*8) rdi);
	pushcode := pushq (ind ~ofs:(i*8) rbx) ++ !pushcode
   done;
   for i = 0 to ((total_size-pos-size)/8 - 1)
   do
	lastpop := !lastpop ++ popq rax
   done;
   (code ++ 
    !firstpop ++
    pushq (imm size) ++
    call "malloc" ++
    !choose ++ 
    !lastpop ++ 
    !pushcode 
    (*liberere la mémoire c'est cool *)
    ),size *)
 
 |Clen e -> 
  (fst(compile_expr e fpmax) ++
   popq rbx ++
   popq rax ++
   pushq (reg rbx)),8

 |Ctab (e1,e2,size) -> 
  let p = (size/8 - 1) and forcode = ref nop in begin
  for i = 0 to p
  do
	forcode := !forcode ++ pushq (ind ~ofs:(i*8)  rax) 
  done; 
  (fst(compile_expr e2 fpmax) ++
   fst(compile_expr e1 fpmax) ++
   popq rax ++ (*taille = inutile *)
   popq rax ++ (*adresse des elements *)
   popq rbx ++ (* # de l'élement *)
   imulq (imm (size/8)) (reg rbx) ++
   addq (reg rbx) (reg rax) ++
   !forcode),size;
   end

 |Ccall (f,l) -> 
   let (code_arg, size_arg) = List.fold_left (fun (code,size) e -> 
	let (code_e,size_e) = compile_expr e fpmax in
	(code_e ++ code,size+size_e)) (nop,0) l
   and size_f = Hashtbl.find ret_fun f in
   pushn size_f ++
   code_arg ++
   call f ++
   popn size_arg, size_f
 	(*let (code_arg, size_arg) = List.fold_left(fun (code,size) e -> 
		       let (code_e, size_e) = compile_expr e fpmax in
		       (code ++ code_e, size + size_e))
		      (nop,0) l 
	and size_f = Hashtbl.find ret_fun f in
	let pushcode = ref nop in begin
	for i = 0 to (size_f/8 - 1) do
		pushcode := !pushcode ++ pushq (ind ~ofs:(i*8) rdi)
	done; 
	code_arg ++
	call f ++
	popn size_arg ++
	!pushcode,size_f
	(* on free le malloc *)end *)

 |Cvec (l,size) -> 
   let n = List.length l and 
       compilcode = ref nop and 
       l2 = ref l and
       initvect = ref nop in begin
   for i = 0 to (n-1) do
     let x = List.hd(!l2) in begin
	l2 := List.tl(!l2);
	compilcode := fst(compile_expr x fpmax) ++ !compilcode
     end
   done;
   for i = 0 to ((size/8-1)*n) do
        initvect := !initvect ++ 
		    popq rax ++
		    movq (reg rax) (ind ~ofs:(i*8) rdi)
   done;
   !compilcode ++
   movq (imm (size*n)) (reg rax) ++
   call "malloc" ++
   !initvect ++
   pushq (reg rdi) ++
   pushq (imm n),16
   end

 |Cprint s -> 
  (begin
   string_count := !string_count + 1;
   segment_donnes := !segment_donnes ++ 
                     label ("chaine_"^string_of_int(!string_count)) ++ 
            	     string (s(*^"\\n"*));
		     movq (ilab s) (reg rdi) ++
		     movq (imm 0) (reg rax) ++
		     call "printf"
		     end),0

 |Cbloc b -> 
   compile_bloc b fpmax







(* COMPILE BLOC : FAIT *)
	
and compile_bloc bloc fpmax = match bloc with 
 |CEmptyBloc -> 
   nop,0
 |CE e -> 
   compile_expr e fpmax
 |CI i -> 
   compile_instr i fpmax
 |CB (i,b) -> 
  let (c,size) = compile_bloc b fpmax in 
  (fst(compile_instr i fpmax) ++ c),size
   



(* COMPILE INSTR : FAIT *)

and compile_instr instr fpmax = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
	match instr with
 |CInone -> 
   nop,0

 |CIexpr e -> 
   let (code,size) = compile_expr e fpmax in  
   code ++ popn size ,0

 |CIinit (fp_x,e) -> 
   let (code,size) = compile_expr e fpmax in
   (let forcode = ref nop and p = (size/8 - 1) in begin
   for i = 0 to p do
       forcode := !forcode ++ 
		   popq rax ++ 
 		   movq (reg rax) (ind ~ofs:(fp_x +8*(p-i)) rbp)
   done;
   code ++ !forcode;
   end),0

 |CIinitStruct (fp_x,structenv,size) ->  
   (Smap.fold (fun x (e,x_pos) precode -> 
		let (code,x_size) = compile_expr e fpmax in 
		let forcodebis = ref nop 
		and pbis = (x_size/8-1) in begin
		for i = 0 to pbis do
		  let pos_act = fp_x + x_pos + 8*(pbis - i) in
		  forcodebis := !forcodebis ++ 
				popq rax ++ 
				movq (reg rax) (ind ~ofs:(pos_act) rbp)
		done;
		precode ++ code ++ !forcodebis; 
		end) structenv nop),0

 |CIwhile (e,b) -> 
   let (c,size) = compile_bloc b fpmax in
   (jmp ("w_cond_"^label_string) ++
    label ("w_bloc_"^label_string) ++
    c ++
    label ("w_cond_"^label_string) ++
    fst(compile_expr e fpmax ) ++
    testq (reg rax) (reg rax) ++
    jnz ("w_bloc_"^label_string)),0
 
 |CIend -> 
     popn (fst(fpmax)) ++ 
     popq rbp ++  
     movq (imm 0) (reg rax) ++ 
     ret,0

 |CIreturn e -> 
    let (c,size) = compile_expr e fpmax in
    let retcode = ref nop 
    and pos = snd(fpmax) in begin
    for i = 0 to (size/8-1) do
	retcode := !retcode ++ 
		   popq rax ++
		   movq (reg rax) (ind ~ofs:(pos + i*8) rbp)
    done;
    c ++
    !retcode ++
    popn (fst(fpmax)) ++
    popq rbp ++
    movq (imm 0) (reg rax) ++
    ret,0;
    end
  


  (* let (c,size) = compile_expr e fpmax in 
     let ramcode = ref nop in begin
     for i = 0 to (size/8 - 1) do
	ramcode := !ramcode ++ 
		     popq (rax) ++
		     movq (reg rax) (ind ~ofs:(i*8) rdi)
     done;
     (c ++ 
     movq (imm size) (reg rax) ++
     call "malloc" ++
     !ramcode ++
     popn fpmax ++
     popq rbp ++
     ret),0 
     end
 *)
 |CIif i -> 
    compile_if i fpmax




(* COMPILE IF : FAIT *)
	
and compile_if i fpmax = label_count := !label_count + 1; 
	let label_string = string_of_int !label_count in
	match i with
 |CifThen (e,b) -> 
   let (c,size) = compile_bloc b fpmax in
   (fst(compile_expr e fpmax) ++
    popq rax ++
    testq (reg rax) (reg rax) ++
    jz ("if_end_"^label_string) ++
    c ++
    label ("if_end_"^label_string)),0

 |CifElse (e,b1,b2) -> let (c1,size1) = compile_bloc b1 fpmax in
			let (c2,size2) = compile_bloc b2 fpmax in
   (fst(compile_expr e fpmax) ++
   popq rax ++
   testq (reg rax) (reg rax) ++
   jz ("if_else_"^label_string) ++
   c1 ++
   jmp ("if_end_"^label_string) ++
   label ("if_else_"^label_string)  ++
   c2 ++
   label ("if_end_"^label_string)),size1

 |CifElseIf (e,b1,i2) -> let c1,size1 = compile_bloc b1 fpmax and
			     c2,size2 = compile_if i2 fpmax in 
   (fst(compile_expr e fpmax) ++
   popq rax ++
   testq (reg rax) (reg rax) ++
   jz ("if_else_"^label_string) ++
   c1 ++
   jmp ("if_end_"^label_string) ++
   label ("if_else_"^label_string)  ++
   c2 ++
   label ("if_end_"^label_string)),min size2 size1


	

(* COMPILE FUN : FINIS *)

let compile_fun (codemain, codefuns) (df,fpmax) =
   match df.nom_cfun with
    | "Main" -> 
	(codemain ++ 
         label "main" ++ 
      	 pushq (reg rbp) ++
      	 movq (reg rsp) (reg rbp) ++ 
       	 fst(compile_bloc df.bloc_cfun fpmax),
         codefuns)
    |_ -> 
	(codemain, 
      	let codebloc,size = compile_bloc df.bloc_cfun fpmax
	and size_ret = Hashtbl.find ret_fun df.nom_cfun  
	and retcode = ref nop 
	and pos = snd(fpmax) in begin
	 for i = 0 to (size_ret/8 - 1) do
	    retcode := !retcode ++ 
			popq rax ++
			movq (reg rax) (ind ~ofs:(pos + i*8) rbp)
	 done;
         codefuns ++
	 label df.nom_cfun ++
	 pushq (reg rbp) ++
	 movq (reg rsp) (reg rbp) ++
	 pushn (fst(fpmax)) ++
	 codebloc ++
	 !retcode ++
	 popn (fst(fpmax)) ++
	 popq rbp ++
	 ret
	end)


(* COMPILE FILE : FINIS *)

let compile_program f ofile =
	let f = alloc f in
	let (codemain, codefuns) = List.fold_left compile_fun  (nop, nop) f
	 in
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
