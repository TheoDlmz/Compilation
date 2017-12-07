
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
  | MenhirState139
  | MenhirState132
  | MenhirState130
  | MenhirState124
  | MenhirState121
  | MenhirState115
  | MenhirState113
  | MenhirState112
  | MenhirState110
  | MenhirState104
  | MenhirState102
  | MenhirState101
  | MenhirState99
  | MenhirState97
  | MenhirState94
  | MenhirState92
  | MenhirState90
  | MenhirState88
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState80
  | MenhirState76
  | MenhirState74
  | MenhirState73
  | MenhirState64
  | MenhirState63
  | MenhirState50
  | MenhirState49
  | MenhirState44
  | MenhirState43
  | MenhirState41
  | MenhirState40
  | MenhirState32
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState26
  | MenhirState24
  | MenhirState21
  | MenhirState17
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  

	open Ast

# 133 "parser.ml"

let rec _menhir_goto_decl_fun : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decl_fun -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv515) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_decl_fun) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv513) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((df : 'tv_decl_fun) : 'tv_decl_fun) = _v in
    ((let _v : 'tv_decl = 
# 39 "parser.mly"
                (Dfun df)
# 148 "parser.ml"
     in
    _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv514)) : 'freshtv516)

and _menhir_goto_ifb : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ifb -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv507 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifb) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv505 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((i : 'tv_ifb) : 'tv_ifb) = _v in
        ((let (((_menhir_stack, _menhir_s), _, (e : 'tv_expr)), _, (b : 'tv_bloc)) = _menhir_stack in
        let _4 = () in
        let _1 = () in
        let _v : 'tv_ifb = 
# 90 "parser.mly"
                                    (If3 (e,b,i))
# 170 "parser.ml"
         in
        _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)) : 'freshtv508)
    | MenhirState28 | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv511) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_ifb) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv509) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((i : 'tv_ifb) : 'tv_ifb) = _v in
        ((let _v : 'tv_instr = 
# 80 "parser.mly"
          (Iif i)
# 185 "parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv510)) : 'freshtv512)
    | _ ->
        _menhir_fail ()

and _menhir_goto_binaire : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_binaire -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv503 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_binaire) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73) : 'freshtv504)

and _menhir_goto_list_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_instr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state) * _menhir_state * 'tv_list_instr_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv493 * _menhir_state) * _menhir_state * 'tv_list_instr_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv491 * _menhir_state) * _menhir_state * 'tv_list_instr_) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (l : 'tv_list_instr_)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_bloc = 
# 68 "parser.mly"
                                              ({instruction = l(*; expression = Eexpr e*)})
# 252 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv489) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_bloc) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState28 | MenhirState121 | MenhirState112 | MenhirState97 | MenhirState101 | MenhirState90 | MenhirState29 | MenhirState32 | MenhirState40 | MenhirState41 | MenhirState43 | MenhirState81 | MenhirState44 | MenhirState49 | MenhirState63 | MenhirState73 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv463 * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv461 * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (b : 'tv_bloc)) = _menhir_stack in
                let _v : 'tv_expr = 
# 108 "parser.mly"
            (Cbloc b)
# 269 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)) : 'freshtv464)
            | MenhirState88 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv467 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv465 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _menhir_s), _, (e : 'tv_expr)), _, (b : 'tv_bloc)) = _menhir_stack in
                let _1 = () in
                let _v : 'tv_instr = 
# 77 "parser.mly"
                          (Iwhile (e,b))
# 282 "parser.ml"
                 in
                _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv466)) : 'freshtv468)
            | MenhirState113 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv475 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ELSE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv469 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | IF ->
                        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | LEFTG ->
                        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState115
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115) : 'freshtv470)
                | ENDLINE | ET | ETMUT | EXCL | FALSE | FOIS | IDENT _ | IF | LEFTG | LEFTPAR | LET | MOINS | PRINT | RETURN | RIGHTG | TRUE | Tint _ | VEC | WHILE ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv471 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let (((_menhir_stack, _menhir_s), _, (e : 'tv_expr)), _, (b : 'tv_bloc)) = _menhir_stack in
                    let _1 = () in
                    let _v : 'tv_ifb = 
# 88 "parser.mly"
                       (If1 (e,b))
# 313 "parser.ml"
                     in
                    _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv473 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)) : 'freshtv476)
            | MenhirState115 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv479 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv477 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), _, (e : 'tv_expr)), _, (b : 'tv_bloc)), _, (b2 : 'tv_bloc)) = _menhir_stack in
                let _4 = () in
                let _1 = () in
                let _v : 'tv_ifb = 
# 89 "parser.mly"
                                      (If2 (e,b,b2))
# 334 "parser.ml"
                 in
                _menhir_goto_ifb _menhir_env _menhir_stack _menhir_s _v) : 'freshtv478)) : 'freshtv480)
            | MenhirState27 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((((('freshtv483 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 342 "parser.ml"
                ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state)) * _menhir_state * 'tv_typ) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((((((('freshtv481 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 348 "parser.ml"
                ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state)) * _menhir_state * 'tv_typ) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let ((((((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 353 "parser.ml"
                ))), _, (la : 'tv_list_argument_)), _), _, (t : 'tv_typ)), _, (b : 'tv_bloc)) = _menhir_stack in
                let _7 = () in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_decl_fun = 
# 53 "parser.mly"
                                                                               ({nom=i;args=la;typ = t;bloc = b})
# 363 "parser.ml"
                 in
                _menhir_goto_decl_fun _menhir_env _menhir_stack _menhir_s _v) : 'freshtv482)) : 'freshtv484)
            | MenhirState24 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv487 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 371 "parser.ml"
                ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv485 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 377 "parser.ml"
                ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state * 'tv_bloc) = Obj.magic _menhir_stack in
                ((let ((((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 382 "parser.ml"
                ))), _, (la : 'tv_list_argument_)), _, (b : 'tv_bloc)) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : 'tv_decl_fun = 
# 52 "parser.mly"
                                                             ({nom=i;args = la;typ = none; bloc = b})
# 390 "parser.ml"
                 in
                _menhir_goto_decl_fun _menhir_env _menhir_stack _menhir_s _v) : 'freshtv486)) : 'freshtv488)
            | _ ->
                _menhir_fail ()) : 'freshtv490)) : 'freshtv492)) : 'freshtv494)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv495 * _menhir_state) * _menhir_state * 'tv_list_instr_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)) : 'freshtv498)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state * 'tv_instr) * _menhir_state * 'tv_list_instr_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state * 'tv_instr) * _menhir_state * 'tv_list_instr_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_instr)), _, (xs : 'tv_list_instr_)) = _menhir_stack in
        let _v : 'tv_list_instr_ = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 411 "parser.ml"
         in
        _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv500)) : 'freshtv502)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv451 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 426 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv447 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 436 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv445 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 443 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 448 "parser.ml"
            ))), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_expr = let l =
              let xs = xs0 in
              
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 457 "parser.ml"
              
            in
            
# 105 "parser.mly"
                                                            (Ccall(i,l))
# 463 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv446)) : 'freshtv448)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv449 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 473 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)) : 'freshtv452)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv459 * _menhir_state))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv455 * _menhir_state))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv453 * _menhir_state))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_expr__)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = let l =
              let xs = xs0 in
              
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 499 "parser.ml"
              
            in
            
# 106 "parser.mly"
                                                       (Cvec l)
# 505 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv454)) : 'freshtv456)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv457 * _menhir_state))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expr__) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)) : 'freshtv460)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_sous_instr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * _menhir_state * 'tv_sous_instr)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_sous_instr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv437 * _menhir_state * 'tv_sous_instr)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_sous_instr_) : 'tv_separated_nonempty_list_COMMA_sous_instr_) = _v in
        ((let (_menhir_stack, _menhir_s, (x : 'tv_sous_instr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_sous_instr_ = 
# 217 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 535 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv438)) : 'freshtv440)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv443) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_sous_instr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv441) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_sous_instr_) : 'tv_separated_nonempty_list_COMMA_sous_instr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_sous_instr__ = 
# 130 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x )
# 550 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_sous_instr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv442)) : 'freshtv444)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expr_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState32 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv431) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv429) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 130 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x )
# 571 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv430)) : 'freshtv432)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv435 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expr_) : 'tv_separated_nonempty_list_COMMA_expr_) = _v in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 217 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 588 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv434)) : 'freshtv436)
    | _ ->
        _menhir_fail ()

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv427) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 118 "parser.mly"
          (SupEg)
# 604 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv428)

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv425) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 117 "parser.mly"
       (Sup)
# 618 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv426)

and _menhir_run53 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv409 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (Ast.ident)
# 634 "parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv407 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        let ((i : (
# 9 "parser.mly"
       (Ast.ident)
# 642 "parser.ml"
        )) : (
# 9 "parser.mly"
       (Ast.ident)
# 646 "parser.ml"
        )) = _v in
        ((let ((_menhir_stack, _menhir_s, (e : 'tv_expr)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 102 "parser.mly"
                           (Cselect (e,i))
# 653 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)) : 'freshtv410)
    | LEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv421 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv417 * _menhir_state * 'tv_expr) * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RIGHTG ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv413 * _menhir_state * 'tv_expr) * _menhir_state))) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv411 * _menhir_state * 'tv_expr) * _menhir_state))) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, (e : 'tv_expr)), _) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _v : 'tv_expr = 
# 103 "parser.mly"
                                  (Clen e)
# 682 "parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv412)) : 'freshtv414)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv415 * _menhir_state * 'tv_expr) * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv419 * _menhir_state * 'tv_expr) * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)) : 'freshtv422)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv423 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv424)

and _menhir_run58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv405) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 119 "parser.mly"
        (Add)
# 717 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv403) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 125 "parser.mly"
      (Or)
# 731 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv401) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 114 "parser.mly"
          (Diff)
# 745 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv402)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv399) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 120 "parser.mly"
         (Sub)
# 759 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv400)

and _menhir_run62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv397) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 123 "parser.mly"
       (Mod)
# 773 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)

and _menhir_run63 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv395) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 116 "parser.mly"
          (InfEg)
# 824 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv396)

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv393) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 115 "parser.mly"
         (Inf)
# 838 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv391) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 121 "parser.mly"
        (Times)
# 852 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv392)

and _menhir_run69 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv389) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 126 "parser.mly"
        (Egal)
# 866 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv387) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 122 "parser.mly"
         (Div)
# 880 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv388)

and _menhir_run71 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv385) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 113 "parser.mly"
          (Equiv)
# 894 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv383) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_binaire = 
# 124 "parser.mly"
       (And)
# 908 "parser.ml"
     in
    _menhir_goto_binaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv384)

and _menhir_reduce60 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_instr_ = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 917 "parser.ml"
     in
    _menhir_goto_list_instr_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDLINE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv381 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState90 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv379 * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_instr = 
# 78 "parser.mly"
                 (Ireturn None)
# 978 "parser.ml"
         in
        _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv380)) : 'freshtv382)
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90

and _menhir_run94 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | MUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | IDENT _ ->
        _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState94
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState112
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv377) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_instr = 
# 73 "parser.mly"
          (Inone)
# 1074 "parser.ml"
     in
    _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv378)

and _menhir_reduce62 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_loption_separated_nonempty_list_COMMA_expr__ = 
# 128 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 1083 "parser.ml"
     in
    _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_instr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_instr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv375 * _menhir_state * 'tv_instr) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDLINE ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | IF ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | LET ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | RETURN ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | WHILE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | RIGHTG ->
        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState121
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121) : 'freshtv376)

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 1141 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LEFTPAR ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
    | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTC | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | POINT | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 1159 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv374)

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv305 * _menhir_state * 'tv_unaire) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | COMMA | ENDLINE | LEFTG | RIGHTC | RIGHTG | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv303 * _menhir_state * 'tv_unaire) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (u : 'tv_unaire)), _, (e : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 101 "parser.mly"
                      (Unop(u,e))
# 1213 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv306)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | RIGHTC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState64 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv307 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _menhir_s, (e : 'tv_expr)), _), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_expr = 
# 104 "parser.mly"
                                  (Ctab(e,e2))
# 1268 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv308)) : 'freshtv310)
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64) : 'freshtv312)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_binaire) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState74
        | COMMA | ENDLINE | LEFTG | RIGHTC | RIGHTG | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv313 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_binaire) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, (e : 'tv_expr)), _, (b : 'tv_binaire)), _, (e2 : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_expr = 
# 98 "parser.mly"
                                 (Binop(b,e,e2))
# 1324 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74) : 'freshtv316)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv319 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState76
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv317 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 100 "parser.mly"
                            (Unop (Star,e))
# 1349 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv320)
    | MenhirState32 | MenhirState81 | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv325 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv321 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState80 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ET ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | ETMUT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | EXCL ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FALSE ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FOIS ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | IDENT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | LEFTG ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LEFTPAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | MOINS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | PRINT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TRUE ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | Tint _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | VEC ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv322)
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState80
        | RIGHTC | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv323 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (x : 'tv_expr)) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expr_ = 
# 215 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1439 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80) : 'freshtv326)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv329 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState83 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv327 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 109 "parser.mly"
                            (Cexpr e)
# 1494 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv328)) : 'freshtv330)
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83) : 'freshtv332)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv335 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState85
        | AND | COMMA | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTG | MOD | MOINS | NOTEGAL | OR | PLUS | RIGHTC | RIGHTG | RIGHTPAR | SUP | SUPEGAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv333 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 99 "parser.mly"
                              (Unop (Neg,e))
# 1523 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv336)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv337 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | LEFTG ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState88
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv338)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | ENDLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv341 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState92 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv339 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s), _, (e : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_instr = 
# 79 "parser.mly"
                          (Ireturn (Eexpr e))
# 1602 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv340)) : 'freshtv342)
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState92
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv344)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv357 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 1638 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState102
        | COMMA | RIGHTG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv355 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 1680 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 1685 "parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_sous_instr = 
# 84 "parser.mly"
                       (i,e)
# 1691 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv353) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_sous_instr) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv351 * _menhir_state * 'tv_sous_instr) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv345 * _menhir_state * 'tv_sous_instr) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104) : 'freshtv346)
            | RIGHTG ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv347 * _menhir_state * 'tv_sous_instr) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, (x : 'tv_sous_instr)) = _menhir_stack in
                let _v : 'tv_separated_nonempty_list_COMMA_sous_instr_ = 
# 215 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1722 "parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_sous_instr_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv348)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv349 * _menhir_state * 'tv_sous_instr) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)) : 'freshtv354)) : 'freshtv356)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv358)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv363 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 1741 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | ENDLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv361 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 1759 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState110 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv359 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 1767 "parser.ml"
            ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((((_menhir_stack, _menhir_s), _, (b : 'tv_boption_MUT_)), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 1773 "parser.ml"
            ))), _, (e : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_instr = 
# 75 "parser.mly"
                                                       (Iinit ((b,i),e))
# 1781 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv360)) : 'freshtv362)
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState110
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110) : 'freshtv364)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | LEFTG ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState113
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv366)
    | MenhirState28 | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AND ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | DBLEGAL ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | DIVISE ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | EGAL ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | ENDLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv369 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState124 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv367 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            ((let (_menhir_stack, _menhir_s, (e : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_instr = 
# 74 "parser.mly"
                   (Iexpr e)
# 1883 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv368)) : 'freshtv370)
        | FOIS ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | INF ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | INFEGAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | LEFTC ->
            _menhir_run63 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MOD ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | MOINS ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | NOTEGAL ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | OR ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | POINT ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | SUP ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | SUPEGAL ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState124
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124) : 'freshtv372)
    | _ ->
        _menhir_fail ()

and _menhir_goto_unaire : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_unaire -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv301 * _menhir_state * 'tv_unaire) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv302)

and _menhir_goto_decl : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decl -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv299 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv300)

and _menhir_goto_typ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv267 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv265 * _menhir_state) * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _, (t : 'tv_typ)) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_typ = 
# 60 "parser.mly"
                 (Ttypemut t)
# 1990 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)) : 'freshtv268)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv271 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv269 * _menhir_state) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (t : 'tv_typ)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_typ = 
# 59 "parser.mly"
             (Ttype t)
# 2003 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv270)) : 'freshtv272)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv279 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2011 "parser.ml"
        ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SUP ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv275 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2021 "parser.ml"
            ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv273 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2028 "parser.ml"
            ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2033 "parser.ml"
            ))), _, (t : 'tv_typ)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : 'tv_typ = 
# 58 "parser.mly"
                           (Ttyid (i,t))
# 2040 "parser.ml"
             in
            _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv274)) : 'freshtv276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv277 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2050 "parser.ml"
            ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)) : 'freshtv280)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv287 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2059 "parser.ml"
        ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv285 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2065 "parser.ml"
        ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : (
# 9 "parser.mly"
       (Ast.ident)
# 2070 "parser.ml"
        ))), _, (t : 'tv_typ)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_decl_sous_struct = 
# 48 "parser.mly"
                      ((x,t))
# 2076 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv283) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_decl_sous_struct) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv281 * _menhir_state * 'tv_decl_sous_struct) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | RIGHTG ->
            _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17) : 'freshtv282)) : 'freshtv284)) : 'freshtv286)) : 'freshtv288)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv289 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 2101 "parser.ml"
        ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state)) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTG ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv290)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv297 * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2117 "parser.ml"
        ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv295 * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2123 "parser.ml"
        ))) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, (b : 'tv_boption_MUT_)), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2128 "parser.ml"
        ))), _, (t : 'tv_typ)) = _menhir_stack in
        let _3 = () in
        let _v : 'tv_argument = 
# 64 "parser.mly"
                                       ({nom=(b,i);typ=t})
# 2134 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv293) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_argument) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MUT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | RIGHTPAR ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | IDENT _ ->
            _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState132
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv292)) : 'freshtv294)) : 'freshtv296)) : 'freshtv298)
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXCL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv261 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTC ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ET ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | ETMUT ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | EXCL ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | FALSE ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | FOIS ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | IDENT _v ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | LEFTG ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LEFTPAR ->
                _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | MOINS ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | PRINT ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | TRUE ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | Tint _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | VEC ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | RIGHTC ->
                _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32) : 'freshtv258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)) : 'freshtv262)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv263 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (int)
# 2227 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv255) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((i : (
# 7 "parser.mly"
       (int)
# 2237 "parser.ml"
    )) : (
# 7 "parser.mly"
       (int)
# 2241 "parser.ml"
    )) = _v in
    ((let _v : 'tv_expr = 
# 94 "parser.mly"
           (Cint i)
# 2246 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv256)

and _menhir_run34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv253) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 95 "parser.mly"
       (Cbool true)
# 2260 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXCL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv245 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | Tstring _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv241 * _menhir_state))) = Obj.magic _menhir_stack in
                let (_v : (
# 8 "parser.mly"
       (string)
# 2288 "parser.ml"
                )) = _v in
                ((let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | RIGHTPAR ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv237 * _menhir_state))) * (
# 8 "parser.mly"
       (string)
# 2299 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv235 * _menhir_state))) * (
# 8 "parser.mly"
       (string)
# 2306 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), (c : (
# 8 "parser.mly"
       (string)
# 2311 "parser.ml"
                    ))) = _menhir_stack in
                    let _5 = () in
                    let _3 = () in
                    let _2 = () in
                    let _1 = () in
                    let _v : 'tv_expr = 
# 107 "parser.mly"
                                           (Fprint c)
# 2320 "parser.ml"
                     in
                    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)) : 'freshtv238)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv239 * _menhir_state))) * (
# 8 "parser.mly"
       (string)
# 2330 "parser.ml"
                    )) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)) : 'freshtv242)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv243 * _menhir_state))) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv247 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)) : 'freshtv250)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv251 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | MOINS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_unaire = 
# 130 "parser.mly"
         (Neg)
# 2394 "parser.ml"
         in
        _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv234)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ENDLINE ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | IF ->
        _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | LET ->
        _menhir_run94 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | RETURN ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | WHILE ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | RIGHTG ->
        _menhir_reduce60 _menhir_env (Obj.magic _menhir_stack) MenhirState28
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_reduce29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2491 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2497 "parser.ml"
    ))) = _menhir_stack in
    let _v : 'tv_expr = 
# 97 "parser.mly"
            (Cident i)
# 2502 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2509 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | FOIS ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | MOINS ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | RIGHTPAR ->
        _menhir_reduce62 _menhir_env (Obj.magic _menhir_stack) MenhirState43
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_goto_loption_separated_nonempty_list_COMMA_sous_instr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_sous_instr__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((((('freshtv231 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2555 "parser.ml"
    ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2559 "parser.ml"
    ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_sous_instr__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RIGHTG ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv227 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2569 "parser.ml"
        ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2573 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_sous_instr__) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ENDLINE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv223 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2583 "parser.ml"
            ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2587 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_sous_instr__)) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv221 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2594 "parser.ml"
            ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2598 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_sous_instr__)) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), _, (b : 'tv_boption_MUT_)), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2603 "parser.ml"
            ))), _, (j : (
# 9 "parser.mly"
       (Ast.ident)
# 2607 "parser.ml"
            ))), _, (xs0 : 'tv_loption_separated_nonempty_list_COMMA_sous_instr__)) = _menhir_stack in
            let _9 = () in
            let _8 = () in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_instr = let l =
              let xs = xs0 in
              
# 206 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( xs )
# 2619 "parser.ml"
              
            in
            
# 76 "parser.mly"
                                                                                                          (IinitS ((b,i),j,l))
# 2625 "parser.ml"
             in
            _menhir_goto_instr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv222)) : 'freshtv224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv225 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2635 "parser.ml"
            ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2639 "parser.ml"
            ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_sous_instr__)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv229 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 2650 "parser.ml"
        ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2654 "parser.ml"
        ))) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_sous_instr__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)) : 'freshtv232)

and _menhir_run100 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 2662 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TO ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv217 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2674 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ET ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | ETMUT ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | EXCL ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | FALSE ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | FOIS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | IDENT _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | LEFTG ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | LEFTPAR ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | MOINS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | PRINT ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | TRUE ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | Tint _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState101 _v
        | VEC ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState101
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState101) : 'freshtv218)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv219 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2716 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ET ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | ETMUT ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | EXCL ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FALSE ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IDENT _v ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LEFTG ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LEFTPAR ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | PRINT ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TRUE ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | Tint _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | VEC ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FOIS | MOINS ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv215 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_unaire = 
# 132 "parser.mly"
        (Star)
# 2757 "parser.ml"
         in
        _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv213) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_expr = 
# 96 "parser.mly"
        (Cbool false)
# 2775 "parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv214)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv211) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unaire = 
# 131 "parser.mly"
        (Not)
# 2789 "parser.ml"
     in
    _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv209) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unaire = 
# 134 "parser.mly"
         (Mutand)
# 2803 "parser.ml"
     in
    _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv207) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_unaire = 
# 133 "parser.mly"
      (And)
# 2817 "parser.ml"
     in
    _menhir_goto_unaire _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_list_decl_sous_struct_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_decl_sous_struct_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv201 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 2835 "parser.ml"
        ))) * _menhir_state * 'tv_list_decl_sous_struct_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv197 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 2845 "parser.ml"
            ))) * _menhir_state * 'tv_list_decl_sous_struct_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv195 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 2852 "parser.ml"
            ))) * _menhir_state * 'tv_list_decl_sous_struct_) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2857 "parser.ml"
            ))), _, (l : 'tv_list_decl_sous_struct_)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_decl_struct = 
# 44 "parser.mly"
                                                          ({nom = i; struc = l})
# 2865 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv193) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_decl_struct) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_decl_struct) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv189) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((ds : 'tv_decl_struct) : 'tv_decl_struct) = _v in
            ((let _v : 'tv_decl = 
# 40 "parser.mly"
                   (Dstruct ds)
# 2882 "parser.ml"
             in
            _menhir_goto_decl _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)) : 'freshtv192)) : 'freshtv194)) : 'freshtv196)) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv199 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 2892 "parser.ml"
            ))) * _menhir_state * 'tv_list_decl_sous_struct_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state * 'tv_decl_sous_struct) * _menhir_state * 'tv_list_decl_sous_struct_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state * 'tv_decl_sous_struct) * _menhir_state * 'tv_list_decl_sous_struct_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_decl_sous_struct)), _, (xs : 'tv_list_decl_sous_struct_)) = _menhir_stack in
        let _v : 'tv_list_decl_sous_struct_ = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2905 "parser.ml"
         in
        _menhir_goto_list_decl_sous_struct_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)) : 'freshtv206)
    | _ ->
        _menhir_fail ()

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 2914 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | INF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv183 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2926 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7) : 'freshtv184)
    | IDENT _ | LEFTG | MUT | RIGHTG | RIGHTPAR | SUP ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv185 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2944 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (i : (
# 9 "parser.mly"
       (Ast.ident)
# 2949 "parser.ml"
        ))) = _menhir_stack in
        let _v : 'tv_typ = 
# 57 "parser.mly"
            (Tident i)
# 2954 "parser.ml"
         in
        _menhir_goto_typ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv186)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv187 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 2964 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv188)

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
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv181 * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState8 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv182)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_goto_list_argument_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_argument_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv175 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3009 "parser.ml"
        ))) * _menhir_state * 'tv_list_argument_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RIGHTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv171 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3019 "parser.ml"
            ))) * _menhir_state * 'tv_list_argument_) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LEFTG ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24
            | MOINS ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv169 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3031 "parser.ml"
                ))) * _menhir_state * 'tv_list_argument_)) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState24 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | SUP ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv165 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3043 "parser.ml"
                    ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ET ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
                    | IDENT _v ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26) : 'freshtv166)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((((('freshtv167 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3063 "parser.ml"
                    ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)) : 'freshtv170)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24) : 'freshtv172)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv173 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3078 "parser.ml"
            ))) * _menhir_state * 'tv_list_argument_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)) : 'freshtv176)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state * 'tv_argument) * _menhir_state * 'tv_list_argument_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv177 * _menhir_state * 'tv_argument) * _menhir_state * 'tv_list_argument_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_argument)), _, (xs : 'tv_list_argument_)) = _menhir_stack in
        let _v : 'tv_list_argument_ = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3091 "parser.ml"
         in
        _menhir_goto_list_argument_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv178)) : 'freshtv180)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_MUT_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_boption_MUT_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "parser.mly"
       (Ast.ident)
# 3113 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EGAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv145 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3124 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ET ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | ETMUT ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | EXCL ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | FALSE ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | FOIS ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | IDENT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv143 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3144 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    let (_menhir_s : _menhir_state) = MenhirState97 in
                    let (_v : (
# 9 "parser.mly"
       (Ast.ident)
# 3150 "parser.ml"
                    )) = _v in
                    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | LEFTG ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv139 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3161 "parser.ml"
                        ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3165 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | IDENT _v ->
                            _menhir_run100 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
                        | RIGHTG ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
                            let (_menhir_s : _menhir_state) = MenhirState99 in
                            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_sous_instr__ = 
# 128 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3179 "parser.ml"
                             in
                            _menhir_goto_loption_separated_nonempty_list_COMMA_sous_instr__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv138)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv140)
                    | LEFTPAR ->
                        _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
                    | AND | DBLEGAL | DIVISE | EGAL | ENDLINE | FOIS | INF | INFEGAL | LEFTC | MOD | MOINS | NOTEGAL | OR | PLUS | POINT | SUP | SUPEGAL ->
                        _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv141 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3197 "parser.ml"
                        ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3201 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)) : 'freshtv144)
                | LEFTG ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | LEFTPAR ->
                    _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | MOINS ->
                    _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | PRINT ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | TRUE ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | Tint _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                | VEC ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv146)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv147 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3230 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)) : 'freshtv150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv151 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)
    | MenhirState132 | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state * 'tv_boption_MUT_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_boption_MUT_) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "parser.mly"
       (Ast.ident)
# 3253 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | TO ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv155 * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3264 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ET ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState130
                | IDENT _v ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv156)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv157 * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3284 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_boption_MUT_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)) : 'freshtv164)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_decl_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_decl_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv125 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (l : 'tv_list_decl_)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 30 "parser.mly"
      (Ast.fichier)
# 3318 "parser.ml"
            ) = 
# 35 "parser.mly"
                       (l)
# 3322 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv123) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "parser.mly"
      (Ast.fichier)
# 3330 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 30 "parser.mly"
      (Ast.fichier)
# 3338 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv119) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 30 "parser.mly"
      (Ast.fichier)
# 3346 "parser.ml"
            )) : (
# 30 "parser.mly"
      (Ast.fichier)
# 3350 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv120)) : 'freshtv122)) : 'freshtv124)) : 'freshtv126)) : 'freshtv128)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)) : 'freshtv132)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv135 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv133 * _menhir_state * 'tv_decl) * _menhir_state * 'tv_list_decl_) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (x : 'tv_decl)), _, (xs : 'tv_list_decl_)) = _menhir_stack in
        let _v : 'tv_list_decl_ = 
# 187 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 3369 "parser.ml"
         in
        _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)) : 'freshtv136)
    | _ ->
        _menhir_fail ()

and _menhir_reduce58 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_decl_sous_struct_ = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3380 "parser.ml"
     in
    _menhir_goto_list_decl_sous_struct_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (Ast.ident)
# 3387 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | TO ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3399 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ET ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
        | IDENT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv116)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3419 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)

and _menhir_reduce18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_boption_MUT_ = 
# 119 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( false )
# 3429 "parser.ml"
     in
    _menhir_goto_boption_MUT_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce54 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_argument_ = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3438 "parser.ml"
     in
    _menhir_goto_list_argument_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv113) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_boption_MUT_ = 
# 121 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( true )
# 3452 "parser.ml"
     in
    _menhir_goto_boption_MUT_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv114)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19 * _menhir_state * 'tv_decl) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * _menhir_state * 'tv_argument) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv22)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv23 * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3474 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv26)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state * 'tv_instr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)
    | MenhirState115 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv29 * _menhir_state) * _menhir_state * 'tv_expr) * _menhir_state * 'tv_bloc)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv31 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)
    | MenhirState112 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)
    | MenhirState110 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv35 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3508 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv36)
    | MenhirState104 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv37 * _menhir_state * 'tv_sous_instr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv39 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3522 "parser.ml"
        ))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)
    | MenhirState101 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv41 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3531 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv42)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv43 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3540 "parser.ml"
        ))) * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3544 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv45 * _menhir_state) * _menhir_state * 'tv_boption_MUT_) * (
# 9 "parser.mly"
       (Ast.ident)
# 3553 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)
    | MenhirState94 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv47 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv48)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv49 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv53 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv55 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv57 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState80 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv61 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv63 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_binaire) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state * 'tv_expr) * _menhir_state * 'tv_binaire) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv69 * _menhir_state * 'tv_expr) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state * 'tv_expr) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_unaire) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv75 * _menhir_state * 'tv_unaire) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3642 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState41 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv85 * _menhir_state))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv87 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv91 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3676 "parser.ml"
        ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state)) * _menhir_state * 'tv_typ) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState26 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv93 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3685 "parser.ml"
        ))) * _menhir_state * 'tv_list_argument_)) * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv95 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3694 "parser.ml"
        ))) * _menhir_state * 'tv_list_argument_)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv97 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3703 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState17 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * 'tv_decl_sous_struct) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState7 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3727 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv107 * _menhir_state * (
# 9 "parser.mly"
       (Ast.ident)
# 3736 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState3 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3745 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv112)

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_decl_ = 
# 185 "/home/theoz/.opam/system/lib/menhir/standard.mly"
    ( [] )
# 3759 "parser.ml"
     in
    _menhir_goto_list_decl_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (Ast.ident)
# 3775 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTG ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv11 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3786 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
            | RIGHTG ->
                _menhir_reduce58 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3) : 'freshtv12)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv13 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3806 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv14)) : 'freshtv16)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv18)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (Ast.ident)
# 3830 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LEFTPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv3 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3841 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | MUT ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | RIGHTPAR ->
                _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | IDENT _ ->
                _menhir_reduce18 _menhir_env (Obj.magic _menhir_stack) MenhirState21
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv4)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv5 * _menhir_state) * (
# 9 "parser.mly"
       (Ast.ident)
# 3863 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv6)) : 'freshtv8)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv9 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv10)

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
# 3890 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FN ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRUCT ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 219 "/home/theoz/.opam/system/lib/menhir/standard.mly"
  


# 3924 "parser.ml"
