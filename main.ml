open Format;;
open Lexing;;
open Parser;;

let parse_only = ref false and type_only = ref false and file = ref "" in

let getpos pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1) in
	let printpos (p1,p2) = 
	 let p1  = getpos p1 and p2 = getpos p2 in
	  Printf.printf "File \"%s\", line %d, characters %d - %d : \n" !file (fst p1) (snd p1) (snd p2) in
	

	Arg.parse ["--parse_only",Arg.Set parse_only, "Lexer + Parser";
		   "--type_only",Arg.Set type_only, "Typer"]
		  (fun s -> print_string s; print_newline(); 
			if s = "" then (Printf.printf "Missing file\n"; exit 1)
			else if not(Filename.check_suffix s ".rs") then (Printf.printf "file must have .rs extension \n";exit 1) else file := s) "";

	let lexbuf = Lexing.from_channel (open_in !file) in
	try
	  let pass1 = Parser.fichier Lexer.token lexbuf in
	  if not(!parse_only) then ((*
		let pass2 = Typer.program pass1 in
		if not(!type_only) then (
		let out = open_out ((Filename.chop_suffix !file ".rs")^".s") in
		let fmt = Format.formatter_of_out_channel out in
			X86_64.print_program fmt (Compiler.program pass2);
			Format.fprintf fmt "@?";
			close_out out;
		)
	*));
	exit 0

	with 
	|Lexer.Lexing_error c ->
		printpos (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);
		Printf.printf "lexical error : %s\n" c;
		exit 1
	|Parsing.Parse_error |Parser.Error ->
		printpos (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);
		Printf.printf "Syntax error\n";
		exit 1
