
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
  | MenhirState154
  | MenhirState147
  | MenhirState145
  | MenhirState136
  | MenhirState134
  | MenhirState130
  | MenhirState128
  | MenhirState127
  | MenhirState125
  | MenhirState119
  | MenhirState117
  | MenhirState116
  | MenhirState114
  | MenhirState112
  | MenhirState109
  | MenhirState107
  | MenhirState105
  | MenhirState103
  | MenhirState100
  | MenhirState98
  | MenhirState96
  | MenhirState95
  | MenhirState91
  | MenhirState90
  | MenhirState89
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState84
  | MenhirState83
  | MenhirState82
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState78
  | MenhirState77
  | MenhirState76
  | MenhirState75
  | MenhirState73
  | MenhirState72
  | MenhirState71
  | MenhirState70
  | MenhirState69
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState64
  | MenhirState63
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState57
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
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

# 163 "parser.ml"

let rec _menhir_goto_decl_fun : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.dfun) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (df : (Ast.dfun)) = _v in
    let _v : (Ast.decl) = 
# 39 "parser.mly"
                (Dfun df)
# 173 "parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_ifb : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ift) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (Ast.ift)) = _v in
        let (((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : (Ast.ift) = 
# 96 "parser.mly"
                                    (If3 (e,b,i))
# 190 "parser.ml"
         in
        _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (i : (Ast.ift)) = _v in
        let _v : (Ast.instr) = 
# 86 "parser.mly"
          (Iif i)
# 200 "parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
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
# 222 "parser.ml"
            ))), _, (xs0 : (Ast.expr list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) = let l =
              let xs = xs0 in
              
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 231 "parser.ml"
              
            in
            
# 111 "parser.mly"
                                                            (Ccall(i,l))
# 237 "parser.ml"
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
# 265 "parser.ml"
              
            in
            
# 112 "parser.mly"
                                                       (Cvec l)
# 271 "parser.ml"
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

and _menhir_goto_blockbody : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.block) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (i : (Ast.instr))), _, (b : (Ast.block))) = _menhir_stack in
        let _v : (Ast.block) = 
# 73 "parser.mly"
                        (B (i,b))
# 294 "parser.ml"
         in
        _menhir_goto_blockbody _menhir_env _menhir_stack _menhir_s _v
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RIGHTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (b : (Ast.block))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.block) = 
# 69 "parser.mly"
                             (b )
# 312 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState30 | MenhirState134 | MenhirState127 | MenhirState112 | MenhirState116 | MenhirState105 | MenhirState31 | MenhirState34 | MenhirState42 | MenhirState43 | MenhirState45 | MenhirState96 | MenhirState46 | MenhirState48 | MenhirState49 | MenhirState50 | MenhirState57 | MenhirState87 | MenhirState75 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState72 | MenhirState59 | MenhirState70 | MenhirState61 | MenhirState68 | MenhirState66 | MenhirState63 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (b : (Ast.block))) = _menhir_stack in
                let _v : (Ast.expr) = 
# 114 "parser.mly"
            (Cbloc b)
# 323 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
            | MenhirState103 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))) = _menhir_stack in
                let _1 = () in
                let _v : (Ast.instr) = 
# 83 "parser.mly"
                          (Iwhile (e,b))
# 334 "parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
            | MenhirState128 ->
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
                        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | LEFTG ->
                        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130)
                | ENDLINE | ET | ETMUT | EXCL | FALSE | FOIS | IDENT _ | IF | LEFTG | LEFTPAR | LET | MOINS | PRINT | RETURN | RIGHTG | TRUE | Tint _ | VEC | WHILE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))) = _menhir_stack in
                    let _1 = () in
                    let _v : (Ast.ift) = 
# 94 "parser.mly"
                       (If1 (e,b))
# 362 "parser.ml"
                     in
                    _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState130 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (b : (Ast.block))), _, (b2 : (Ast.block))) = _menhir_stack in
                let _4 = () in
                let _1 = () in
                let _v : (Ast.ift) = 
# 95 "parser.mly"
                                      (If2 (e,b,b2))
# 380 "parser.ml"
                 in
                _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v
            | MenhirState29 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 389 "parser.ml"
                ))), _, (la : (Ast.argument list))), _), _, (t : (Ast.tipe))), _, (b : (Ast.block))) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.dfun) = 
# 54 "parser.mly"
                                                                               ({nom=i;args=la;typ = T t;bloc = b})
# 399 "parser.ml"
                 in
                _menhir_goto_decl_fun _menhir_env _menhir_stack _menhir_s _v
            | MenhirState26 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 408 "parser.ml"
                ))), _, (la : (Ast.argument list))), _, (b : (Ast.block))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.dfun) = 
# 53 "parser.mly"
                                                             ({nom=i;args = la;typ = None; bloc = b})
# 416 "parser.ml"
                 in
                _menhir_goto_decl_fun _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Ast.ident * Ast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((Ast.ident * Ast.expr) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.ident * Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : ((Ast.ident * Ast.expr) list) = 
# 217 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 442 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((Ast.ident * Ast.expr) list)) = _v in
        let _v : ((Ast.ident * Ast.expr) list) = 
# 130 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x )
# 452 "parser.ml"
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
# 468 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 217 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 480 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
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

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
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
# 1017 "parser.ml"
        )) = _v in
        let ((_menhir_stack, _menhir_s, (e : (Ast.expr))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr) = 
# 108 "parser.mly"
                           (Cselect (e,i))
# 1024 "parser.ml"
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
# 109 "parser.mly"
                                  (Clen e)
# 1049 "parser.ml"
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

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

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
# 1118 "parser.ml"
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
# 1130 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_decl_sous_struct_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

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

and _menhir_run105 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDLINE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState105 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Ast.instr) = 
# 84 "parser.mly"
                 (Ireturn None)
# 1190 "parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState105 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUT ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | IDENT _ ->
        _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState109
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run127 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run133 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.instr) = 
# 79 "parser.mly"
          (Inone)
# 1284 "parser.ml"
     in
    _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce63 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expr list) = 
# 128 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 1293 "parser.ml"
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
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
    | IF ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | LET ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | RETURN ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | WHILE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState134
    | RIGHTG ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (i : (Ast.instr))) = _menhir_stack in
        let _v : (Ast.block) = 
# 74 "parser.mly"
            (I i)
# 1346 "parser.ml"
         in
        _menhir_goto_blockbody _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let u =
              let _1 = _10 in
              
# 140 "parser.mly"
      (And)
# 1376 "parser.ml"
              
            in
            
# 107 "parser.mly"
                      (Unop(u,e))
# 1382 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | RIGHTC ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState58 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.expr) = 
# 110 "parser.mly"
                                  (Ctab(e,e2))
# 1433 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | AND | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 127 "parser.mly"
          (Supeg)
# 1472 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1478 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | AND | COMMA | DBLEGAL | EGAL | ENDLINE | INF | INFEGAL | LEFTG | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 128 "parser.mly"
        (Add)
# 1509 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1515 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 132 "parser.mly"
       (Mod)
# 1540 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1546 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 130 "parser.mly"
        (Times)
# 1571 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1577 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 131 "parser.mly"
         (Div)
# 1602 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1608 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
        | AND | COMMA | DBLEGAL | EGAL | ENDLINE | INF | INFEGAL | LEFTG | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 129 "parser.mly"
         (Sub)
# 1639 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1645 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
        | AND | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 126 "parser.mly"
       (Sup)
# 1680 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1686 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 134 "parser.mly"
      (Or)
# 1735 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1741 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState78
        | AND | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 123 "parser.mly"
          (Diff)
# 1776 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1782 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | AND | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 125 "parser.mly"
          (Infeg)
# 1817 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1823 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | AND | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 124 "parser.mly"
         (Inf)
# 1858 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1864 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState84
        | AND | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 122 "parser.mly"
          (Equiv)
# 1899 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1905 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | AND | COMMA | EGAL | ENDLINE | LEFTG | OR | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 133 "parser.mly"
       (And)
# 1952 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 1958 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | COMMA | ENDLINE | LEFTG | RIGHTC | RIGHTG | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (e : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let b =
              let _1 = _10 in
              
# 135 "parser.mly"
        (Egal)
# 2011 "parser.ml"
              
            in
            
# 104 "parser.mly"
                                 (Binop(b,e,e2))
# 2017 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let u =
              let _1 = _10 in
              
# 141 "parser.mly"
         (Mutand)
# 2042 "parser.ml"
              
            in
            
# 107 "parser.mly"
                      (Unop(u,e))
# 2048 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState90
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _10 = () in
            let _v : (Ast.expr) = let u =
              let _1 = _10 in
              
# 139 "parser.mly"
        (Not)
# 2073 "parser.ml"
              
            in
            
# 107 "parser.mly"
                      (Unop(u,e))
# 2079 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 106 "parser.mly"
                            (Unop (Star,e))
# 2102 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
    | MenhirState34 | MenhirState96 | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState95 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ET ->
                _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | ETMUT ->
                _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | EXCL ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | FALSE ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | FOIS ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | IDENT _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | LEFTG ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | LEFTPAR ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | MOINS ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | PRINT ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | TRUE ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | Tint _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState96 _v
            | VEC ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState96
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96)
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState95
        | RIGHTC | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 215 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 2189 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | RIGHTPAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState98 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 115 "parser.mly"
                            (Cexpr e)
# 2240 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState98
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _1 = () in
            let _v : (Ast.expr) = 
# 105 "parser.mly"
                              (Unop (Neg,e))
# 2267 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | ENDLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState107 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.instr) = 
# 85 "parser.mly"
                          (Ireturn (Eexpr e))
# 2341 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState107
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState117
        | COMMA | RIGHTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2414 "parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.ident * Ast.expr) = 
# 90 "parser.mly"
                       (i,e)
# 2420 "parser.ml"
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
                    _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119)
            | RIGHTG ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.ident * Ast.expr))) = _menhir_stack in
                let _v : ((Ast.ident * Ast.expr) list) = 
# 215 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 2444 "parser.ml"
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | ENDLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (b : (bool))), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2478 "parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Ast.instr) = 
# 81 "parser.mly"
                                                       (Iinit ((b,i),e))
# 2486 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState128
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
    | MenhirState30 | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | DBLEGAL ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | DIVISE ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | EGAL ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | ENDLINE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState136 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.instr) = 
# 80 "parser.mly"
                   (Iexpr e)
# 2583 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v
        | FOIS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | INF ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | INFEGAL ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | LEFTC ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MOD ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | MOINS ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | NOTEGAL ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | OR ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | PLUS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | POINT ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | SUP ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | SUPEGAL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState136
        | RIGHTG ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.block) = 
# 75 "parser.mly"
           (E e)
# 2616 "parser.ml"
             in
            _menhir_goto_blockbody _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
    | _ ->
        _menhir_fail ()

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 2629 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTPAR ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTC | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | POINT | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

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
# 2660 "parser.ml"
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
# 2671 "parser.ml"
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
# 2686 "parser.ml"
            ))), _, (t : (Ast.tipe))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (Ast.tipe) = 
# 59 "parser.mly"
                           (Ttyid (i,t))
# 2693 "parser.ml"
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
# 2708 "parser.ml"
        ))), _, (t : (Ast.tipe))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.ident * Ast.tipe) = 
# 49 "parser.mly"
                      ((x,t))
# 2714 "parser.ml"
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
# 2738 "parser.ml"
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
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (b : (bool))), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2764 "parser.ml"
        ))), _, (t : (Ast.tipe))) = _menhir_stack in
        let _3 = () in
        let _v : (Ast.argument) = 
# 65 "parser.mly"
                                       ({nom=(b,i);typ=t})
# 2770 "parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MUT ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | RIGHTPAR ->
            _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | IDENT _ ->
            _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState147
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147)
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
                _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState34
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
# 2854 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (i : (
# 7 "parser.mly"
       (int)
# 2862 "parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 100 "parser.mly"
           (Cint i)
# 2867 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 101 "parser.mly"
       (Cbool true)
# 2879 "parser.ml"
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
# 2912 "parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : (Ast.expr) = 
# 113 "parser.mly"
                                           (Fprint c)
# 2921 "parser.ml"
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
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42
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
        _menhir_run133 _menhir_env (Obj.magic _menhir_stack) MenhirState30
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
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LET ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | RETURN ->
        _menhir_run105 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | WHILE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_reduce17 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3073 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 3079 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.expr) = 
# 103 "parser.mly"
            (Cident i)
# 3084 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run45 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3091 "parser.ml"
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
        _menhir_reduce63 _menhir_env (Obj.magic _menhir_stack) MenhirState45
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
# 3149 "parser.ml"
            ))), _, (j : (
# 9 "parser.mly"
       (Ast.ident)
# 3153 "parser.ml"
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
# 3165 "parser.ml"
              
            in
            
# 82 "parser.mly"
                                                                                                          (IinitS ((b,i),j,l))
# 3171 "parser.ml"
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

and _menhir_run115 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 3190 "parser.ml"
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
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | ETMUT ->
            _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | EXCL ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | FALSE ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | FOIS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | IDENT _v ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | LEFTG ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | LEFTPAR ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | MOINS ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | PRINT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | TRUE ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | Tint _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
        | VEC ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState116
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
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
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
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
# 102 "parser.mly"
        (Cbool false)
# 3284 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49

and _menhir_run50 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | ETMUT ->
        _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | EXCL ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FALSE ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FOIS ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | IDENT _v ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LEFTG ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MOINS ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | PRINT ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TRUE ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | Tint _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | VEC ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

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
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | EOF ->
        _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState154
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 3425 "parser.ml"
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
# 3450 "parser.ml"
        ))) = _menhir_stack in
        let _v : (Ast.tipe) = 
# 58 "parser.mly"
            (Tident i)
# 3455 "parser.ml"
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
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.argument))), _, (xs : (Ast.argument list))) = _menhir_stack in
        let _v : (Ast.argument list) = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3554 "parser.ml"
         in
        _menhir_goto_list_argument_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_MUT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 ->
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
                    _menhir_run50 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | ETMUT ->
                    _menhir_run49 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | EXCL ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | FALSE ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | FOIS ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | IDENT _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_s = MenhirState112 in
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
                            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _v
                        | RIGHTG ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_s = MenhirState114 in
                            let _v : ((Ast.ident * Ast.expr) list) = 
# 128 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3610 "parser.ml"
                             in
                            _menhir_goto_loption_separated_nonempty_list_COMMA_sous_instr__ _menhir_env _menhir_stack _menhir_s _v
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114)
                    | LEFTPAR ->
                        _menhir_run45 _menhir_env (Obj.magic _menhir_stack)
                    | AND | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTC | MOD | MOINS | NOTEGAL | OR | PLUS | POINT | SUP | SUPEGAL ->
                        _menhir_reduce17 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | LEFTG ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | LEFTPAR ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | MOINS ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | PRINT ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | TRUE ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | Tint _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
                | VEC ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState112
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
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
    | MenhirState147 | MenhirState23 ->
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
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | IDENT _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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
# 3713 "parser.ml"
            ) = 
# 35 "parser.mly"
                       (l)
# 3717 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 30 "parser.mly"
      (Ast.fichier)
# 3724 "parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.decl))), _, (xs : (Ast.fichier))) = _menhir_stack in
        let _v : (Ast.fichier) = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3740 "parser.ml"
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
# 3760 "parser.ml"
        ))), _, (xs0 : ((Ast.ident * Ast.tipe) list))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (Ast.dstruct) = let l =
          let xs = xs0 in
          
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 3770 "parser.ml"
          
        in
        
# 44 "parser.mly"
                                                                          ({nom = i; struc = l})
# 3776 "parser.ml"
         in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (ds : (Ast.dstruct)) = _v in
        let _v : (Ast.decl) = 
# 40 "parser.mly"
                   (Dstruct ds)
# 3784 "parser.ml"
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
# 3797 "parser.ml"
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

and _menhir_reduce6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bool) = 
# 119 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( false )
# 3829 "parser.ml"
     in
    _menhir_goto_boption_MUT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce57 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.argument list) = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3838 "parser.ml"
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
# 3850 "parser.ml"
     in
    _menhir_goto_boption_MUT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState147 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState134 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState130 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState127 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState119 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState114 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState96 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState84 ->
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
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState76 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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

and _menhir_reduce59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.fichier) = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 4170 "parser.ml"
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
# 4199 "parser.ml"
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
                _menhir_reduce57 _menhir_env (Obj.magic _menhir_stack) MenhirState23
            | IDENT _ ->
                _menhir_reduce6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
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
# 4274 "parser.ml"
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
        _menhir_reduce59 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 219 "/home/theoz/.opam/system/lib/menhir/standard.mly"
  


# 4303 "parser.ml"
