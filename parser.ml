
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | VEC
    | Tstring of (
# 8 "parser.mly"
       (string)
# 13 "parser.ml"
  )
    | Tint of (
# 7 "parser.mly"
       (int)
# 18 "parser.ml"
  )
    | TRUE
    | TO
    | SUPEGAL
    | SUP
    | STRUCT
    | RIGHTPAR
    | RIGHTG
    | RIGHTC
    | RETURN
    | PRINT
    | POINT
    | PLUS
    | OR
    | NOTEGAL
    | MUT
    | MOINS
    | MOD
    | LET
    | LEN
    | LEFTPAR
    | LEFTG
    | LEFTC
    | INFEGAL
    | INF
    | IF
    | IDENT of (
# 9 "parser.mly"
       (Ast.ident)
# 48 "parser.ml"
  )
    | FOIS
    | FN
    | FALSE
    | EXCL
    | ETMUT
    | ET
    | EOF
    | ENDLINE
    | ELSE
    | EGAL
    | DIVISE
    | DBLEGAL
    | COMMA
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState143
  | MenhirState136
  | MenhirState134
  | MenhirState128
  | MenhirState125
  | MenhirState123
  | MenhirState121
  | MenhirState117
  | MenhirState115
  | MenhirState114
  | MenhirState112
  | MenhirState106
  | MenhirState104
  | MenhirState103
  | MenhirState101
  | MenhirState99
  | MenhirState96
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState82
  | MenhirState78
  | MenhirState76
  | MenhirState75
  | MenhirState66
  | MenhirState65
  | MenhirState52
  | MenhirState51
  | MenhirState46
  | MenhirState45
  | MenhirState43
  | MenhirState42
  | MenhirState34
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState26
  | MenhirState23
  | MenhirState19
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  

	open Ast

# 135 "parser.ml"

let rec _menhir_goto_decl_fun : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dfun) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (df : (Ast.dfun)) = _v in
    let _v : (Ast.decl) = 
# 39 "parser.mly"
                (Dfun df)
# 145 "parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ifb : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ift) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (Ast.ift)) = _v in
        let (((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (Ast.ift) = 
# 91 "parser.mly"
                                    (If3 (e,b,i))
# 162 "parser.ml"
         in
        _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (Ast.ift)) = _v in
        let _v : (Ast.instr) = 
# 81 "parser.mly"
          (Iif i)
# 172 "parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_binaire : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.bop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ET ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | ETMUT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | EXCL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | FOIS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | IDENT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | LEFTPAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | MOINS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | PRINT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | RIGHTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState121 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (l : (Ast.instr list))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.block) = 
# 70 "parser.mly"
                               ({instruction = l; expression = None})
# 256 "parser.ml"
             in
            _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v
        | TRUE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | Tint _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
        | VEC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.instr))), _, (xs : (Ast.instr list))) = _menhir_stack in
        let _v : (Ast.instr list) = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 276 "parser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 298 "parser.ml"
            ))), _, (xs0 : (Ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) = let l =
              let xs = xs0 in
              
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 307 "parser.ml"
              
            in
            
# 106 "parser.mly"
                                                            (Ccall(i,l))
# 313 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (xs0 : (Ast.expr list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Ast.expr) = let l =
              let xs = xs0 in
              
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 341 "parser.ml"
              
            in
            
# 107 "parser.mly"
                                                       (Cvec l)
# 347 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_bloc : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 | MenhirState125 | MenhirState121 | MenhirState114 | MenhirState99 | MenhirState103 | MenhirState92 | MenhirState31 | MenhirState34 | MenhirState42 | MenhirState43 | MenhirState45 | MenhirState83 | MenhirState46 | MenhirState51 | MenhirState65 | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (b : (Ast.block))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 109 "parser.mly"
            (Cbloc b)
# 370 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.instr) = 
# 78 "parser.mly"
                          (Iwhile (e,b))
# 381 "parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IF ->
                _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | LEFTG ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState117
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
        | ENDLINE | ET | ETMUT | EXCL | FALSE | FOIS | IDENT _ | IF | LEFTG | LEFTPAR | LET | MOINS | PRINT | RETURN | RIGHTG | TRUE | Tint _ | VEC | WHILE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.ift) = 
# 89 "parser.mly"
                       (If1 (e,b))
# 409 "parser.ml"
             in
            _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))), _, (b2 : (Ast.block))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (Ast.ift) = 
# 90 "parser.mly"
                                      (If2 (e,b,b2))
# 427 "parser.ml"
         in
        _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 436 "parser.ml"
        ))), _, (la : (Ast.argument list))), _), _, (t : (Ast.tipe))), _, (b : (Ast.block))) = _menhir_stack in
        let _7 = () in
        let _6 = () in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.dfun) = 
# 54 "parser.mly"
                                                                               ({nom=i;args=la;typ = T t;bloc = b})
# 446 "parser.ml"
         in
        _menhir_goto_decl_fun _menhir_env _menhir_stack _menhir_s _v
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 455 "parser.ml"
        ))), _, (la : (Ast.argument list))), _, (b : (Ast.block))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.dfun) = 
# 53 "parser.mly"
                                                             ({nom=i;args = la;typ = None; bloc = b})
# 463 "parser.ml"
         in
        _menhir_goto_decl_fun _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.ident * Ast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ast.ident * Ast.expr) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ident * Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.ident * Ast.expr) list) = 
# 217 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 481 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Ast.ident * Ast.expr) list)) = _v in
        let _v : ((Ast.ident * Ast.expr) list) = 
# 130 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x )
# 491 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_sous_instr__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState34 | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 130 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x )
# 507 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 217 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 519 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 119 "parser.mly"
          (Supeg)
# 533 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 118 "parser.mly"
       (Sup)
# 545 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (
# 9 "parser.mly"
       (Ast.ident)
# 562 "parser.ml"
        )) = _v in
        let ((_menhir_stack, _menhir_s, (e : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 103 "parser.mly"
                           (Cselect (e,i))
# 569 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | LEN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RIGHTG ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, (e : (Ast.expr))), _) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _v : (Ast.expr) = 
# 104 "parser.mly"
                                  (Clen e)
# 594 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 120 "parser.mly"
        (Add)
# 624 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 126 "parser.mly"
      (Or)
# 636 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 115 "parser.mly"
          (Diff)
# 648 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 121 "parser.mly"
         (Sub)
# 660 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run64 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 124 "parser.mly"
       (Mod)
# 672 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 117 "parser.mly"
          (Infeg)
# 721 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 116 "parser.mly"
         (Inf)
# 733 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 122 "parser.mly"
        (Times)
# 745 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 127 "parser.mly"
        (Egal)
# 757 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 123 "parser.mly"
         (Div)
# 769 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run73 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 114 "parser.mly"
          (Equiv)
# 781 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run74 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.bop) = 
# 125 "parser.mly"
       (And)
# 793 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_separated_nonempty_list_COMMA_decl_sous_struct_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.ident * Ast.tipe) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Ast.ident * Ast.tipe) list)) = _v in
        let _v : ((Ast.ident * Ast.tipe) list) = 
# 130 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x )
# 807 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_decl_sous_struct__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ast.ident * Ast.tipe) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ident * Ast.tipe))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.ident * Ast.tipe) list) = 
# 217 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 819 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_decl_sous_struct_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.instr list) = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 830 "parser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run92 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDLINE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState92 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.instr) = 
# 79 "parser.mly"
                 (Ireturn None)
# 888 "parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState92 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState92
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92

and _menhir_run96 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | IDENT _ ->
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState96
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96

and _menhir_run114 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState114
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114

and _menhir_run120 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.instr) = 
# 74 "parser.mly"
          (Inone)
# 982 "parser.ml"
     in
    _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) = 
# 128 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 991 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDLINE ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | IF ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | LET ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | RETURN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | WHILE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | RIGHTG ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 1048 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTPAR ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTC | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | POINT | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | COMMA | ENDLINE | LEFTG | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (u : (Ast.uop))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 102 "parser.mly"
                      (Unop(u,e))
# 1113 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | RIGHTC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState66 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) = 
# 105 "parser.mly"
                                  (Ctab(e,e2))
# 1164 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | COMMA | ENDLINE | LEFTG | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _, (b : (Ast.bop))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 99 "parser.mly"
                                 (Binop(b,e,e2))
# 1218 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 101 "parser.mly"
                            (Unop (Star,e))
# 1241 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState34 | MenhirState83 | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState82 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ET ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | ETMUT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | EXCL ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | FOIS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | IDENT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | LEFTG ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | LEFTPAR ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | MOINS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | PRINT ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | TRUE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | Tint _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
            | VEC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83)
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | RIGHTC | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 215 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1328 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState85 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 110 "parser.mly"
                            (Cexpr e)
# 1379 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 100 "parser.mly"
                              (Unop (Neg,e))
# 1406 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | ENDLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState94 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.instr) = 
# 80 "parser.mly"
                          (Ireturn (Eexpr e))
# 1480 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState94
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState104
        | COMMA | RIGHTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 1553 "parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ident * Ast.expr) = 
# 85 "parser.mly"
                       (i,e)
# 1559 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COMMA ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDENT _v ->
                    _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
            | RIGHTG ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.ident * Ast.expr))) = _menhir_stack in
                let _v : ((Ast.ident * Ast.expr) list) = 
# 215 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1583 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | ENDLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState112 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (b : (bool))), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 1617 "parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.instr) = 
# 76 "parser.mly"
                                                       (Iinit ((b,i),e))
# 1625 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState115
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | RIGHTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState123 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (l : (Ast.instr list))), _, (e : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.block) = 
# 69 "parser.mly"
                                          ({instruction = l; expression = Eexpr e})
# 1743 "parser.ml"
             in
            _menhir_goto_bloc _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState123
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
    | MenhirState30 | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | DBLEGAL ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | DIVISE ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | EGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | ENDLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState128 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.instr) = 
# 75 "parser.mly"
                   (Iexpr e)
# 1777 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | FOIS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | INF ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | INFEGAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LEFTC ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MOINS ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | NOTEGAL ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | OR ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PLUS ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | POINT ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | SUP ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | SUPEGAL ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | _ ->
        _menhir_fail ()

and _menhir_goto_unaire : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.uop) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.tipe) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _, (t : (Ast.tipe))) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.tipe) = 
# 61 "parser.mly"
                 (Ttypemut t)
# 1862 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (t : (Ast.tipe))) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.tipe) = 
# 60 "parser.mly"
             (Ttype t)
# 1873 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SUP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 1888 "parser.ml"
            ))), _, (t : (Ast.tipe))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.tipe) = 
# 59 "parser.mly"
                           (Ttyid (i,t))
# 1895 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 9 "parser.mly"
       (Ast.ident)
# 1910 "parser.ml"
        ))), _, (t : (Ast.tipe))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ident * Ast.tipe) = 
# 49 "parser.mly"
                      ((x,t))
# 1916 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | RIGHTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.ident * Ast.tipe))) = _menhir_stack in
            let _v : ((Ast.ident * Ast.tipe) list) = 
# 215 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1940 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_decl_sous_struct_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (b : (bool))), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 1966 "parser.ml"
        ))), _, (t : (Ast.tipe))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.argument) = 
# 65 "parser.mly"
                                       ({nom=(b,i);typ=t})
# 1972 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | RIGHTPAR ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | IDENT _ ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | _ ->
        _menhir_fail ()

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXCL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ET ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | ETMUT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | EXCL ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FOIS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | IDENT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LEFTG ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LEFTPAR ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | MOINS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | PRINT ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TRUE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | Tint _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | VEC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | RIGHTC ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (int)
# 2056 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 7 "parser.mly"
       (int)
# 2064 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 95 "parser.mly"
           (Cint i)
# 2069 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 96 "parser.mly"
       (Cbool true)
# 2081 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXCL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Tstring _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RIGHTPAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), (c : (
# 8 "parser.mly"
       (string)
# 2114 "parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ast.expr) = 
# 108 "parser.mly"
                                           (Fprint c)
# 2123 "parser.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MOINS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.uop) = 
# 131 "parser.mly"
         (Neg)
# 2188 "parser.ml"
         in
        _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDLINE ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | IF ->
        _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run96 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | RETURN ->
        _menhir_run92 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WHILE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | RIGHTG ->
        _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_reduce29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2285 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2291 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.expr) = 
# 98 "parser.mly"
            (Cident i)
# 2296 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2303 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | RIGHTPAR ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_goto_loption_separated_nonempty_list_COMMA_sous_instr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.ident * Ast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHTG ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ENDLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (b : (bool))), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2361 "parser.ml"
            ))), _, (j : (
# 9 "parser.mly"
       (Ast.ident)
# 2365 "parser.ml"
            ))), _, (xs0 : ((Ast.ident * Ast.expr) list))) = _menhir_stack in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.instr) = let l =
              let xs = xs0 in
              
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 2377 "parser.ml"
              
            in
            
# 77 "parser.mly"
                                                                                                          (IinitS ((b,i),j,l))
# 2383 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run102 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 2402 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TO ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ET ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | ETMUT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EXCL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | FOIS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | IDENT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LEFTPAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MOINS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PRINT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | TRUE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | Tint _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
        | VEC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FOIS | MOINS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : (Ast.uop) = 
# 133 "parser.mly"
        (Star)
# 2486 "parser.ml"
         in
        _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 97 "parser.mly"
        (Cbool false)
# 2502 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.uop) = 
# 132 "parser.mly"
        (Not)
# 2514 "parser.ml"
     in
    _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.uop) = 
# 135 "parser.mly"
         (Mutand)
# 2526 "parser.ml"
     in
    _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.uop) = 
# 134 "parser.mly"
      (And)
# 2538 "parser.ml"
     in
    _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.decl) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState143
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 2568 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INF ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7)
    | COMMA | IDENT _ | LEFTG | MUT | RIGHTG | RIGHTPAR | SUP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2593 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.tipe) = 
# 58 "parser.mly"
            (Tident i)
# 2598 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IDENT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | MUT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState8 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_goto_list_argument_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.argument list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LEFTG ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState26
            | MOINS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState26 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | SUP ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ET ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState28
                    | IDENT _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.argument))), _, (xs : (Ast.argument list))) = _menhir_stack in
        let _v : (Ast.argument list) = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2697 "parser.ml"
         in
        _menhir_goto_list_argument_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_MUT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EGAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ET ->
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | ETMUT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | EXCL ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | FOIS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | IDENT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState99 in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LEFTG ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | IDENT _v ->
                            _menhir_run102 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
                        | RIGHTG ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_s = MenhirState101 in
                            let _v : ((Ast.ident * Ast.expr) list) = 
# 128 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2753 "parser.ml"
                             in
                            _menhir_goto_loption_separated_nonempty_list_COMMA_sous_instr__ _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101)
                    | LEFTPAR ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
                    | AND | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTC | MOD | MOINS | NOTEGAL | OR | PLUS | POINT | SUP | SUPEGAL ->
                        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | LEFTG ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | LEFTPAR ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | MOINS ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | PRINT ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | TRUE ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | Tint _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                | VEC ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState136 | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | IDENT _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | TO ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState134
                | IDENT _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.fichier) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (l : (Ast.fichier))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 30 "parser.mly"
      (Ast.fichier)
# 2856 "parser.ml"
            ) = 
# 35 "parser.mly"
                       (l)
# 2860 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 30 "parser.mly"
      (Ast.fichier)
# 2867 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.decl))), _, (xs : (Ast.fichier))) = _menhir_stack in
        let _v : (Ast.fichier) = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2883 "parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_decl_sous_struct__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.ident * Ast.tipe) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHTG ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2903 "parser.ml"
        ))), _, (xs0 : ((Ast.ident * Ast.tipe) list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.dstruct) = let l =
          let xs = xs0 in
          
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 2913 "parser.ml"
          
        in
        
# 44 "parser.mly"
                                                                          ({nom = i; struc = l})
# 2919 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ds : (Ast.dstruct)) = _v in
        let _v : (Ast.decl) = 
# 40 "parser.mly"
                   (Dstruct ds)
# 2927 "parser.ml"
         in
        _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 2940 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TO ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bool) = 
# 119 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( false )
# 2972 "parser.ml"
     in
    _menhir_goto_boption_MUT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.argument list) = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 2981 "parser.ml"
     in
    _menhir_goto_list_argument_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (bool) = 
# 121 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( true )
# 2993 "parser.ml"
     in
    _menhir_goto_boption_MUT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState101 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState92 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.fichier) = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3201 "parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | RIGHTG ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState3 in
                let _v : ((Ast.ident * Ast.tipe) list) = 
# 128 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3230 "parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_decl_sous_struct__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | MUT ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | RIGHTPAR ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | IDENT _ ->
                _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and fichier : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 30 "parser.mly"
      (Ast.fichier)
# 3305 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 219 "/home/theoz/.opam/system/lib/menhir/standard.mly"
  


# 3334 "parser.ml"
