
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | UPDATE
    | UNDEFINED
    | TYPEOF
    | TRY
    | TRUE
    | TIMES
    | THROW
    | THEN
    | STRING of (
# 20 "src/parser.mly"
       (string)
# 20 "src/parser.ml"
  )
    | SEMI
    | RPAREN
    | REF
    | RBRACKET
    | RBRACE
    | PLUS
    | OR
    | NOTEQUALEQUAL
    | NOTEQUAL
    | NOT
    | MOD
    | MINUS
    | LT
    | LPAREN
    | LET
    | LEQ
    | LBRACKET
    | LBRACE
    | INT of (
# 19 "src/parser.mly"
       (string)
# 43 "src/parser.ml"
  )
    | IN
    | IF
    | ID of (
# 20 "src/parser.mly"
       (string)
# 50 "src/parser.ml"
  )
    | HANDLE
    | GT
    | GEQ
    | FUN
    | FINALLY
    | FALSE
    | EQUALEQUAL
    | EQUAL
    | EOF
    | END
    | ELSE
    | DOUBLE_SEMI
    | DOT
    | DONE
    | DO
    | DIV
    | DEREF
    | DELETE
    | COMMA
    | COLON
    | CATCH
    | BEGIN
    | ASSIGN
    | ARROW
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
  | MenhirState158
  | MenhirState152
  | MenhirState148
  | MenhirState145
  | MenhirState142
  | MenhirState139
  | MenhirState133
  | MenhirState126
  | MenhirState124
  | MenhirState120
  | MenhirState118
  | MenhirState112
  | MenhirState106
  | MenhirState104
  | MenhirState102
  | MenhirState99
  | MenhirState97
  | MenhirState94
  | MenhirState91
  | MenhirState86
  | MenhirState83
  | MenhirState82
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState42
  | MenhirState39
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState22
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState13
  | MenhirState12
  | MenhirState10
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState3
  | MenhirState1
  | MenhirState0

# 10 "src/parser.mly"
  
open Ast
open Ast_factory

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Stdlib.compare lst)

# 168 "src/parser.ml"

let rec _menhir_goto_defn : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.defn) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOUBLE_SEMI ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | EOF ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState158
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_reduce68 : _menhir_env -> ((('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (Ast.expr)) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
    let _v : (Ast.expr) = 
# 177 "src/parser.mly"
    ( make_get_field e1 e2 )
# 192 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_seq_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | UPDATE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | DELETE ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | DEREF ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | FALSE ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | FUN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | ID _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
                | IF ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | INT _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
                | LBRACE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | LET ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | LPAREN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | MINUS ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | NOT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | REF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | STRING _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
                | THROW ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | TRY ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | TYPEOF ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | UNDEFINED ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
            | AND | ASSIGN | BEGIN | CATCH | COMMA | DEREF | DIV | DO | DONE | DOT | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FALSE | FINALLY | GEQ | GT | ID _ | IN | INT _ | LBRACE | LBRACKET | LEQ | LPAREN | LT | MINUS | MOD | NOT | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STRING _ | THEN | TIMES | TRUE | TYPEOF | UNDEFINED ->
                _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e : (Ast.expr))), _, (s : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 89 "src/parser.mly"
    ( make_seq e s )
# 282 "src/parser.ml"
         in
        _menhir_goto_seq_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack)
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
        | RBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | DOT | LBRACKET ->
                _menhir_reduce68 _menhir_env (Obj.magic _menhir_stack)
            | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))) = _menhir_stack in
                let _v : (Ast.expr) = 
# 134 "src/parser.mly"
    ( make_delete_field e1 e2 )
# 318 "src/parser.ml"
                 in
                _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _, (xs : (string list))), _, (e : (Ast.expr))) = _menhir_stack in
        (match try
          Some (
# 129 "src/parser.mly"
    ( if has_dups xs then (raise _eRR) (* duplicate argument names *) else
        make_fun xs e )
# 342 "src/parser.ml"
           : (Ast.expr))
        with
        | Error ->
            None with
        | Some _v ->
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | None ->
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | DELETE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | DEREF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | FALSE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | LBRACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | LET ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | MINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | NOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
            | THROW ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | TRY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | TYPEOF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | UNDEFINED ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState97
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), (f : (
# 20 "src/parser.mly"
       (string)
# 420 "src/parser.ml"
        ))), _, (xs : (string list))), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 118 "src/parser.mly"
    ( make_let_fn f xs e1 e2 )
# 425 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (x : (
# 20 "src/parser.mly"
       (string)
# 434 "src/parser.ml"
        ))), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 115 "src/parser.mly"
    ( make_let x e1 e2 )
# 439 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 145 "src/parser.mly"
    ( e )
# 455 "src/parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CATCH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | HANDLE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BEGIN ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | DELETE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | DEREF ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | FALSE ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | FUN ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | ID _v ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                    | IF ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | INT _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                    | LBRACE ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | LET ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | LPAREN ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | MINUS ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | NOT ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | REF ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | STRING _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _v
                    | THROW ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | TRUE ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | TRY ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | TYPEOF ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | UNDEFINED ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState124
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124)
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
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FINALLY ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | DELETE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | DEREF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | FALSE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | LBRACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LET ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | MINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | THROW ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TRY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TYPEOF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | UNDEFINED ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | GEQ | GT | IN | LEQ | LT | MINUS | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), (x : (
# 20 "src/parser.mly"
       (string)
# 610 "src/parser.ml"
            ))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 120 "src/parser.mly"
    ( make_try e1 x e2 )
# 615 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), (x : (
# 20 "src/parser.mly"
       (string)
# 630 "src/parser.ml"
        ))), _, (e2 : (Ast.expr))), _, (e3 : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 123 "src/parser.mly"
    ( make_try_finally e1 x e2 e3 )
# 635 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 143 "src/parser.mly"
    ( e )
# 651 "src/parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | DELETE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | DEREF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | FALSE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | LBRACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | LET ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | MINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | NOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState133 _v
            | THROW ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | TRY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | TYPEOF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | UNDEFINED ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState133
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState133)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DONE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 132 "src/parser.mly"
    ( make_while e1 e2 )
# 735 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 63 "src/parser.mly"
        ( e )
# 756 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.expr)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((((_menhir_stack, _menhir_s), (f : (
# 20 "src/parser.mly"
       (string)
# 774 "src/parser.ml"
        ))), _, (xs : (string list))), _, (e : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.defn) = 
# 80 "src/parser.mly"
    ( make_let_fn_defn f xs e )
# 779 "src/parser.ml"
         in
        _menhir_goto_defn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), (x : (
# 20 "src/parser.mly"
       (string)
# 788 "src/parser.ml"
        ))), _, (e : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.defn) = 
# 76 "src/parser.mly"
    ( make_let_defn x e )
# 793 "src/parser.ml"
         in
        _menhir_goto_defn _menhir_env _menhir_stack _menhir_s _v
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOUBLE_SEMI ->
            _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | EOF ->
            _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState152
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_field_bind_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : ((string * Ast.expr) list)) = _v in
        let _v : ((string * Ast.expr) list) = 
# 144 "<standard.mly>"
    ( x )
# 822 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_field_bind__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : ((string * Ast.expr) list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (string * Ast.expr))) = _menhir_stack in
        let _v : ((string * Ast.expr) list) = 
# 243 "<standard.mly>"
    ( x :: xs )
# 833 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_field_bind_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run106 : _menhir_env -> ((('ttv_tail * _menhir_state) * (
# 20 "src/parser.mly"
       (string)
# 842 "src/parser.ml"
))) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState106
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106

and _menhir_run102 : _menhir_env -> (((((('ttv_tail * _menhir_state) * (
# 20 "src/parser.mly"
       (string)
# 898 "src/parser.ml"
))) * _menhir_state * (string list)))) * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_reduce53 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
    let _v : (Ast.expr) = 
# 85 "src/parser.mly"
    ( e )
# 957 "src/parser.ml"
     in
    _menhir_goto_seq_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 87 "src/parser.mly"
    ( e )
# 1014 "src/parser.ml"
         in
        _menhir_goto_seq_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run50 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_goto_nonempty_list_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (string list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | DELETE ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | DEREF ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | FALSE ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | FUN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | ID _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
                | IF ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | INT _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
                | LBRACE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | LET ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | LPAREN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | MINUS ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | NOT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | REF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | STRING _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
                | THROW ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | TRY ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | TYPEOF ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | UNDEFINED ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ARROW ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | DELETE ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | DEREF ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | FALSE ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | FUN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | ID _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
                | IF ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | INT _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
                | LBRACE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | LET ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | LPAREN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | MINUS ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | NOT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | REF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | STRING _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
                | THROW ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | TRY ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | TYPEOF ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | UNDEFINED ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (string))), _, (xs : (string list))) = _menhir_stack in
        let _v : (string list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 2011 "src/parser.ml"
         in
        _menhir_goto_nonempty_list_ident_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | DELETE ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | DEREF ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | FALSE ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | FUN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | ID _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                | IF ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | INT _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                | LBRACE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | LET ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | LPAREN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | MINUS ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | NOT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | REF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | STRING _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState145 _v
                | THROW ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | TRY ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | TYPEOF ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | UNDEFINED ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce69 : _menhir_env -> (('ttv_tail * _menhir_state * (Ast.expr)) * _menhir_state) * _menhir_state * (string) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (x : (string))) = _menhir_stack in
    let _v : (Ast.expr) = 
# 179 "src/parser.mly"
    ( make_get_field e1 (make_string x) )
# 2096 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_nonempty_list_simple_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr list) = 
# 223 "<standard.mly>"
    ( x :: xs )
# 2111 "src/parser.ml"
         in
        _menhir_goto_nonempty_list_simple_expr_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (es : (Ast.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 95 "src/parser.mly"
    ( make_app e es )
# 2122 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run86 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expr) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState86 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | FINALLY | IN | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (e2 : (Ast.expr))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 103 "src/parser.mly"
    ( make_update_field e1 e2 e3 )
# 2241 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = let bop = 
# 202 "src/parser.mly"
          ( BopTimes )
# 2257 "src/parser.ml"
         in
        
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2262 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 200 "src/parser.mly"
         ( BopPlus )
# 2282 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2287 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = let bop = 
# 204 "src/parser.mly"
        ( BopMod )
# 2303 "src/parser.ml"
         in
        
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2308 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
        let _v : (Ast.expr) = let bop = 
# 203 "src/parser.mly"
        ( BopDiv )
# 2318 "src/parser.ml"
         in
        
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2323 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | FINALLY | IN | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 109 "src/parser.mly"
    ( make_or e1 e2 )
# 2367 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 212 "src/parser.mly"
                  ( BopNeqStrict )
# 2397 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2402 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 201 "src/parser.mly"
          ( BopMinus )
# 2428 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2433 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 210 "src/parser.mly"
             ( BopNeq )
# 2463 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2468 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 205 "src/parser.mly"
       ( BopLt )
# 2498 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2503 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 206 "src/parser.mly"
        ( BopLeq )
# 2533 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2538 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 207 "src/parser.mly"
       ( BopGt )
# 2568 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2573 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 208 "src/parser.mly"
        ( BopGeq )
# 2603 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2608 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 211 "src/parser.mly"
               ( BopEqStrict )
# 2638 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2643 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | NOTEQUAL | NOTEQUALEQUAL | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let bop = 
# 209 "src/parser.mly"
          ( BopEq )
# 2673 "src/parser.ml"
             in
            
# 97 "src/parser.mly"
    ( make_binop bop e1 e2 )
# 2678 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | FINALLY | IN | OR | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 107 "src/parser.mly"
    ( make_and e1 e2 )
# 2726 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | FINALLY | IN | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 99 "src/parser.mly"
    ( make_assign e1 e2 )
# 2776 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 | MenhirState0 | MenhirState1 | MenhirState133 | MenhirState7 | MenhirState8 | MenhirState124 | MenhirState126 | MenhirState18 | MenhirState106 | MenhirState102 | MenhirState26 | MenhirState31 | MenhirState34 | MenhirState83 | MenhirState75 | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | FINALLY | IN | RBRACE | RBRACKET | RPAREN | THEN ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | FINALLY | IN | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (e1 : (Ast.expr))), _), _, (x2 : (
# 20 "src/parser.mly"
       (string)
# 2874 "src/parser.ml"
            ))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 105 "src/parser.mly"
    ( make_update_field e1 (make_string x2) e3 )
# 2879 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | DELETE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | DEREF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | FALSE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | LBRACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LET ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | MINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | THROW ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | TRY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | TYPEOF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | UNDEFINED ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | END | EOF | FINALLY | IN | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 113 "src/parser.mly"
    ( make_if_partial e1 e2 )
# 2982 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | FINALLY | IN | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e1 : (Ast.expr))), _, (e2 : (Ast.expr))), _, (e3 : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 111 "src/parser.mly"
    ( make_if e1 e2 e3 )
# 3034 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState104 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 101 "src/parser.mly"
    ( make_unop UopMinus e )
# 3150 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (f : (
# 20 "src/parser.mly"
       (string)
# 3201 "src/parser.ml"
            ))), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (string * Ast.expr) = 
# 184 "src/parser.mly"
    ( (f, e) )
# 3206 "src/parser.ml"
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
                | STRING _v ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (string * Ast.expr))) = _menhir_stack in
                let _v : ((string * Ast.expr) list) = 
# 241 "<standard.mly>"
    ( [ x ] )
# 3230 "src/parser.ml"
                 in
                _menhir_goto_separated_nonempty_list_COMMA_field_bind_ _menhir_env _menhir_stack _menhir_s _v
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            _menhir_run102 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | DOUBLE_SEMI | EOF ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | EQUALEQUAL ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | GEQ ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            _menhir_run106 _menhir_env (Obj.magic _menhir_stack)
        | LEQ ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUAL ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | NOTEQUALEQUAL ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | SEMI ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack)
        | DOUBLE_SEMI | EOF ->
            _menhir_reduce53 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_parse_phrase : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.phrase) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.phrase)) = _v in
    Obj.magic _1

and _menhir_goto_loption_separated_nonempty_list_COMMA_field_bind__ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((string * Ast.expr) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (xs : ((string * Ast.expr) list))) = _menhir_stack in
        (match try
          Some ((let fields = 
# 232 "<standard.mly>"
    ( xs )
# 3374 "src/parser.ml"
           in
          
# 172 "src/parser.mly"
    ( if fields |> List.map fst |> has_dups then
        (raise _eRR) (* duplicate fields *)
      else
        make_object fields )
# 3382 "src/parser.ml"
          ) : (Ast.expr))
        with
        | Error ->
            None with
        | Some _v ->
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | None ->
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "src/parser.mly"
       (string)
# 3402 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DELETE ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | DEREF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | FALSE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | FUN ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | ID _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | IF ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | INT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | LBRACE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LET ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | MINUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | NOT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | REF ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | STRING _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
        | THROW ->
            _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TRY ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | TYPEOF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | UNDEFINED ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | WHILE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce38 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 20 "src/parser.mly"
       (string)
# 3470 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (x : (
# 20 "src/parser.mly"
       (string)
# 3476 "src/parser.ml"
    ))) = _menhir_stack in
    let _v : (string) = 
# 189 "src/parser.mly"
    ( x )
# 3481 "src/parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState139 | MenhirState148 | MenhirState145 | MenhirState0 | MenhirState1 | MenhirState133 | MenhirState3 | MenhirState6 | MenhirState7 | MenhirState8 | MenhirState124 | MenhirState126 | MenhirState9 | MenhirState12 | MenhirState13 | MenhirState17 | MenhirState18 | MenhirState19 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState25 | MenhirState26 | MenhirState99 | MenhirState97 | MenhirState31 | MenhirState32 | MenhirState34 | MenhirState35 | MenhirState82 | MenhirState83 | MenhirState79 | MenhirState75 | MenhirState36 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
        let _v : (Ast.expr) = 
# 141 "src/parser.mly"
    ( make_var x )
# 3492 "src/parser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState86 | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT | LBRACKET ->
            _menhir_reduce69 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _), _, (x : (string))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 136 "src/parser.mly"
    ( make_delete_field e (make_string x) )
# 3511 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState142 | MenhirState22 | MenhirState94 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState94 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (string))) = _menhir_stack in
            let _v : (string list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 3533 "src/parser.ml"
             in
            _menhir_goto_nonempty_list_ident_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState33 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState33 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | DELETE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | DEREF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FALSE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LBRACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LET ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | MINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | NOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | THROW ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TRY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TYPEOF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | UNDEFINED ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | MenhirState139 | MenhirState148 | MenhirState145 | MenhirState0 | MenhirState133 | MenhirState1 | MenhirState7 | MenhirState126 | MenhirState124 | MenhirState8 | MenhirState12 | MenhirState18 | MenhirState19 | MenhirState106 | MenhirState104 | MenhirState102 | MenhirState25 | MenhirState99 | MenhirState97 | MenhirState26 | MenhirState31 | MenhirState83 | MenhirState79 | MenhirState75 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState39 | MenhirState36 | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | DEREF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState35 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState77 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | UPDATE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | BEGIN ->
                        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | DELETE ->
                        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | DEREF ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | FALSE ->
                        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | FUN ->
                        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | ID _v ->
                        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                    | IF ->
                        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | INT _v ->
                        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                    | LBRACE ->
                        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | LET ->
                        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | LPAREN ->
                        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | MINUS ->
                        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | NOT ->
                        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | REF ->
                        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | STRING _v ->
                        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
                    | THROW ->
                        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | TRUE ->
                        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | TRY ->
                        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | TYPEOF ->
                        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | UNDEFINED ->
                        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | WHILE ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
                | AND | ASSIGN | BEGIN | CATCH | COMMA | DEREF | DIV | DO | DONE | DOT | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FALSE | FINALLY | GEQ | GT | ID _ | IN | INT _ | LBRACE | LBRACKET | LEQ | LPAREN | LT | MINUS | MOD | NOT | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STRING _ | THEN | TIMES | TRUE | TYPEOF | UNDEFINED ->
                    _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
        | FALSE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | ID _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | INT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | LBRACE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | LBRACKET ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState35 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | DELETE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | DEREF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | FALSE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | LBRACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LET ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | MINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | NOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
            | THROW ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | TRY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | TYPEOF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | UNDEFINED ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | NOT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | STRING _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | TYPEOF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | UNDEFINED ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState35
        | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 93 "src/parser.mly"
    ( e )
# 3787 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
    | MenhirState82 | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DEREF ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | FALSE ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | ID _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | INT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | LBRACE ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LBRACKET ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | LPAREN ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | NOT ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | STRING _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState82 _v
        | TRUE ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | TYPEOF ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | UNDEFINED ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState82
        | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 221 "<standard.mly>"
    ( [ x ] )
# 3833 "src/parser.ml"
             in
            _menhir_goto_nonempty_list_simple_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | BEGIN | CATCH | COMMA | DEREF | DIV | DO | DONE | DOT | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FALSE | FINALLY | GEQ | GT | ID _ | IN | INT _ | LBRACE | LEQ | LPAREN | LT | MINUS | MOD | NOT | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STRING _ | THEN | TIMES | TRUE | TYPEOF | UNDEFINED ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 149 "src/parser.mly"
    ( make_deref e )
# 3851 "src/parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | LBRACKET ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState112
        | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 127 "src/parser.mly"
    ( make_ref e )
# 3875 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run86 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | LBRACKET ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack) MenhirState120
        | AND | ASSIGN | CATCH | COMMA | DIV | DO | DONE | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FINALLY | GEQ | GT | IN | LEQ | LT | MINUS | MOD | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | THEN | TIMES ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 125 "src/parser.mly"
    ( make_throw e)
# 3897 "src/parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | BEGIN | CATCH | COMMA | DEREF | DIV | DO | DONE | DOT | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FALSE | FINALLY | GEQ | GT | ID _ | IN | INT _ | LBRACE | LEQ | LPAREN | LT | MINUS | MOD | NOT | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STRING _ | THEN | TIMES | TRUE | TYPEOF | UNDEFINED ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let uop = 
# 193 "src/parser.mly"
        ( UopNot )
# 3915 "src/parser.ml"
             in
            
# 147 "src/parser.mly"
    ( make_unop uop e )
# 3920 "src/parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | ASSIGN | BEGIN | CATCH | COMMA | DEREF | DIV | DO | DONE | DOT | DOUBLE_SEMI | ELSE | END | EOF | EQUAL | EQUALEQUAL | FALSE | FINALLY | GEQ | GT | ID _ | IN | INT _ | LBRACE | LEQ | LPAREN | LT | MINUS | MOD | NOT | NOTEQUAL | NOTEQUALEQUAL | OR | PLUS | RBRACE | RBRACKET | RPAREN | SEMI | STRING _ | THEN | TIMES | TRUE | TYPEOF | UNDEFINED ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr) = let uop = 
# 194 "src/parser.mly"
           ( UopTypeof )
# 3940 "src/parser.ml"
             in
            
# 147 "src/parser.mly"
    ( make_unop uop e )
# 3945 "src/parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_DOUBLE_SEMI_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (unit option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (e : (Ast.expr))), _, _) = _menhir_stack in
            let _v : (Ast.phrase) = 
# 67 "src/parser.mly"
    ( Expr e )
# 3973 "src/parser.ml"
             in
            _menhir_goto_parse_phrase _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            let _v : (Ast.phrase) = 
# 71 "src/parser.mly"
    ( raise End_of_file )
# 3994 "src/parser.ml"
             in
            _menhir_goto_parse_phrase _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (d : (Ast.defn))), _, _) = _menhir_stack in
            let _v : (Ast.phrase) = 
# 69 "src/parser.mly"
    ( Defn d )
# 4015 "src/parser.ml"
             in
            _menhir_goto_parse_phrase _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (unit option) = 
# 114 "<standard.mly>"
    ( None )
# 4032 "src/parser.ml"
     in
    _menhir_goto_option_DOUBLE_SEMI_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState158 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState152 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState148 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState145 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState142 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState133 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState124 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState120 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState70 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState1 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState1 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState1
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState1

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 170 "src/parser.mly"
    ( make_undefined () )
# 4358 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 166 "src/parser.mly"
    ( make_bool true )
# 4457 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "src/parser.mly"
       (string)
# 4499 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 20 "src/parser.mly"
       (string)
# 4507 "src/parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 164 "src/parser.mly"
    ( make_string s )
# 4512 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | DELETE ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | DEREF ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | FALSE ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | FUN ->
                _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | IF ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | INT _v ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | LBRACE ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LET ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | LPAREN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | MINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | NOT ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | REF ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | STRING _v ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState104 _v
            | THROW ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | TRUE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | TRY ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | TYPEOF ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | UNDEFINED ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | WHILE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState104
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState104)
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
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

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | STRING _v ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | RBRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState10 in
        let _v : ((string * Ast.expr) list) = 
# 142 "<standard.mly>"
    ( [] )
# 4793 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_field_bind__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "src/parser.mly"
       (string)
# 4804 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 19 "src/parser.mly"
       (string)
# 4812 "src/parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 151 "src/parser.mly"
    ( (* The rather curious code below relies on the two's complement
         representation of integers. For any [s in 0..max_int], prefixing
         a minus sign then multiplying by [-1] leaves the integer unchanged.
         For any [s > max_int + 2] (where + is there defined on the
         unlimited-size natural numbers), [s] will be correctly
         rejected by [int_of_string]. That leaves only [s = max_int + 1]
         to deal with. That number prefixed by a minus sign is [min_int].
         And in two's complement, [-1 * min_int = min_int]. So that number
         will be parsed as [min_int], exactly as OCaml does. *)
      try make_int (-1 * int_of_string ("-" ^ s)) with
      | Failure s when s = "int_of_string" -> make_undefined ()
      | e -> raise e )
# 4828 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 20 "src/parser.mly"
       (string)
# 4888 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.expr) = 
# 168 "src/parser.mly"
    ( make_bool false )
# 4926 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run151 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x = () in
    let _v : (unit option) = 
# 116 "<standard.mly>"
    ( Some x )
# 4938 "src/parser.ml"
     in
    _menhir_goto_option_DOUBLE_SEMI_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

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

and parse_expression : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and parse_phrase : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.phrase) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | DELETE ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | DEREF ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | DOUBLE_SEMI ->
        _menhir_run151 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | FALSE ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | FUN ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | ID _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | IF ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | INT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | LBRACE ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | LET ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState139 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQUAL ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | BEGIN ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | DELETE ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | DEREF ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | FALSE ->
                    _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | FUN ->
                    _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | ID _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
                | IF ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | INT _v ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
                | LBRACE ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | LET ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | LPAREN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | MINUS ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | NOT ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | REF ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | STRING _v ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _v
                | THROW ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | TRUE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | TRY ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | TYPEOF ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | UNDEFINED ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | WHILE ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState148
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148)
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ID _v ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142)
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
    | LPAREN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | MINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | NOT ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | REF ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | STRING _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
    | THROW ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | TRUE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | TRY ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | TYPEOF ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | UNDEFINED ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | WHILE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | EOF ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState139
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)

# 269 "<standard.mly>"
  

# 5287 "src/parser.ml"
