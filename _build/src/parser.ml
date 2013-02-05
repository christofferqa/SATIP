exception Error

type token = 
  | WHILE
  | VAR
  | STAR
  | SEMICOLON
  | R_PAREN
  | R_BRACE
  | RETURN
  | PLUS
  | OUTPUT
  | NULL
  | MINUS
  | MALLOC
  | L_PAREN
  | L_BRACE
  | INTEGER_LITERAL of (string)
  | INPUT
  | IF
  | IDENTIFIER of (string)
  | GT
  | EQ
  | EOF
  | ELSE
  | DIV
  | COMMA
  | ASSIGN
  | AMP

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState106
  | MenhirState99
  | MenhirState90
  | MenhirState81
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState62
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState54
  | MenhirState46
  | MenhirState43
  | MenhirState41
  | MenhirState39
  | MenhirState36
  | MenhirState33
  | MenhirState32
  | MenhirState30
  | MenhirState26
  | MenhirState23
  | MenhirState20
  | MenhirState17
  | MenhirState16
  | MenhirState14
  | MenhirState13
  | MenhirState8
  | MenhirState5
  | MenhirState0

  
  let make_identifier pos i =
    { Ast.identifier_pos = pos;
		  Ast.identifier = i }

  let make_exp pos e =
    { Ast.exp_pos = pos;
		  Ast.exp = e }
	
  let make_stm pos s =
    { Ast.stm_pos = pos;
		  Ast.stm = s }

  let make_function pos decl =
    { Ast.function_decl_pos = pos;
		  Ast.function_decl = decl }

  let make_function_decl (name, formals, body) =
    { Ast.function_name    = name;
      Ast.function_formals = formals;
      Ast.function_body    = body }

  let make_program pos decl =
    let file_name = pos.Lexing.pos_fname in
    { Ast.program_name = file_name;
      Ast.program_decl = decl }
let _eRR =
  Error

let rec _menhir_goto_assignment : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stm) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.stm) =     ( _1 ) in
        _menhir_goto_statement_without_trailing_substatement _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_argument_list_nonempty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | AMP ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v _menhir_env._menhir_startp
        | INPUT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
        | INTEGER_LITERAL _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v _menhir_env._menhir_startp
        | L_PAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
        | MALLOC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
        | STAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46)
    | R_PAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.exp list) =      ( List.rev _1 ) in
        _menhir_goto_argument_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_equality_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | AMP ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp
        | INPUT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | INTEGER_LITERAL _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v _menhir_env._menhir_startp
        | L_PAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | MALLOC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | STAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | ASSIGN | COMMA | R_PAREN | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
        let _v : (Ast.exp) =     ( _1 ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState46 | MenhirState33 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (Ast.exp) =    ( _1 ) in
            (match _menhir_s with
            | MenhirState46 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _3 = _v in
                let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
                let _v : (Ast.exp list) =      ( _3 :: _1 ) in
                _menhir_goto_argument_list_nonempty _menhir_env _menhir_stack _menhir_s _v
            | MenhirState33 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = _v in
                let _v : (Ast.exp list) =      ( [_1] ) in
                _menhir_goto_argument_list_nonempty _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | MenhirState20 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | R_PAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | L_PAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
                | ASSIGN | COMMA | DIV | EQ | GT | MINUS | PLUS | R_PAREN | SEMICOLON | STAR ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2) = _menhir_stack in
                    let _startpos = _startpos__1_ in
                    let _v : (Ast.exp) =      ( _2 ) in
                    _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState16 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | R_PAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | L_BRACE ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState58
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState66 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ASSIGN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | AMP ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                | IDENTIFIER _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp
                | INPUT ->
                    _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                | INTEGER_LITERAL _v ->
                    _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v _menhir_env._menhir_startp
                | L_PAREN ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                | MALLOC ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                | NULL ->
                    _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                | STAR ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_startp
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState68 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _startpos__1_), _, _2), _, _4) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.stm) =     ( make_stm _startpos (Ast.PointerAssignment(_2,_4)) ) in
            _menhir_goto_assignment _menhir_env _menhir_stack _menhir_s _v
        | MenhirState70 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.stm) =     ( make_stm _startpos (Ast.Output(_2)) ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
                let _v : (Ast.stm) =     ( _1 ) in
                _menhir_goto_statement_without_trailing_substatement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState73 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | R_PAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _tok = _menhir_discard _menhir_env in
                (match _tok with
                | L_BRACE ->
                    _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState75
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState90 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _, _3) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.stm) =     ( make_stm _startpos (Ast.VarAssignment(_1,_3)) ) in
            _menhir_goto_assignment _menhir_env _menhir_stack _menhir_s _v
        | MenhirState99 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | SEMICOLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _ = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((_menhir_stack, _startpos__1_), _, _2) = _menhir_stack in
                let _startpos = _startpos__1_ in
                let _v : (Ast.stm) =    ( make_stm _startpos (Ast.Return(_2)) ) in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | R_BRACE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _ = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((_menhir_stack, _menhir_s), _, _2), _3) = _menhir_stack in
                    let _v : (Ast.stm list) =     ( _2 @ [_3] ) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _3 = _v in
                    let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _2) = _menhir_stack in
                    let _startpos = _startpos__1_ in
                    let _v : (Ast.function_decl) =     ( make_function _startpos (make_function_decl (_1, _2, _3)) ) in
                    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IDENTIFIER _v ->
                        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState106 _v _menhir_env._menhir_startp
                    | EOF ->
                        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState106
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState106)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_goto_statement_list_nonempty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stm list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v _menhir_env._menhir_startp
    | IF ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | OUTPUT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | VAR ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | WHILE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_startp
    | RETURN | R_BRACE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.stm list) =     ( List.rev _1 ) in
        _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_goto_relational_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState99 | MenhirState90 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState16 | MenhirState46 | MenhirState33 | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | EQ | R_PAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =     ( _1 ) in
            _menhir_goto_equality_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | GT ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | EQ | R_PAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _, _3, _startpos__3_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =     ( make_exp _startpos (Ast.Binop(_1,Ast.Eq,_3)) ) in
            _menhir_goto_equality_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run43 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v _menhir_env._menhir_startp
    | IF ->
        _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_startp
    | OUTPUT ->
        _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_startp
    | VAR ->
        _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_startp
    | WHILE ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _menhir_env._menhir_startp
    | R_BRACE ->
        _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_goto_statement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stm) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.stm list) =     ( _2 :: _1 ) in
        _menhir_goto_statement_list_nonempty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _v : (Ast.stm list) =     ( [_1] ) in
        _menhir_goto_statement_list_nonempty _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_additive_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState99 | MenhirState16 | MenhirState90 | MenhirState73 | MenhirState70 | MenhirState66 | MenhirState68 | MenhirState20 | MenhirState33 | MenhirState46 | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | EQ | GT | R_PAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =     ( _1 ) in
            _menhir_goto_relational_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | EQ | GT | R_PAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _, _3, _startpos__3_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =     ( make_exp _startpos (Ast.Binop(_1,Ast.Gt,_3)) ) in
            _menhir_goto_relational_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.exp) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_goto_statement_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stm list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | R_BRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, _2) = _menhir_stack in
            let _v : (Ast.stm list) =    ( _2 ) in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState75 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | ELSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _tok = _menhir_discard _menhir_env in
                    (match _tok with
                    | L_BRACE ->
                        _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState77
                    | _ ->
                        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                        _menhir_env._menhir_shifted <- (-1);
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77)
                | IDENTIFIER _ | IF | OUTPUT | RETURN | R_BRACE | STAR | VAR | WHILE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _3), _, _5) = _menhir_stack in
                    let _startpos = _startpos__1_ in
                    let _v : (Ast.stm) =     ( make_stm _startpos (Ast.IfThen(_3,_5)) ) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _1 = _v in
                    let _v : (Ast.stm) =     ( _1 ) in
                    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState77 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _3), _, _5), _, _7) = _menhir_stack in
                let _startpos = _startpos__1_ in
                let _v : (Ast.stm) =     ( make_stm _startpos (Ast.IfThenElse(_3,_5,_7)) ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = _v in
                let _v : (Ast.stm) =     ( _1 ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | MenhirState58 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, _3), _, _5) = _menhir_stack in
                let _startpos = _startpos__1_ in
                let _v : (Ast.stm) =     ( make_stm _startpos (Ast.While(_3,_5)) ) in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _1 = _v in
                let _v : (Ast.stm) =     ( _1 ) in
                _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RETURN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | AMP ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
            | IDENTIFIER _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp
            | INPUT ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
            | INTEGER_LITERAL _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v _menhir_env._menhir_startp
            | L_PAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
            | MALLOC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
            | STAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_statement_without_trailing_substatement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.stm) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _v : (Ast.stm) =     ( _1 ) in
    _menhir_goto_statement _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_argument_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | R_PAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2) = _menhir_stack in
        let _v : (Ast.exp list) =    ( _2 ) in
        (match _menhir_s with
        | MenhirState32 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _2 = _v in
            let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =      ( make_exp _startpos (Ast.FunctionInvocation(_1, _2)) ) in
            _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | MenhirState54 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _4 = _v in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =      ( make_exp _startpos (Ast.PointerInvocation(_2, _4)) ) in
            _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            _menhir_fail ())
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_multiplicative_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState99 | MenhirState90 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState16 | MenhirState20 | MenhirState46 | MenhirState36 | MenhirState33 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | ASSIGN | COMMA | EQ | GT | MINUS | PLUS | R_PAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =     ( _1 ) in
            _menhir_goto_additive_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | ASSIGN | COMMA | EQ | GT | MINUS | PLUS | R_PAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _, _3, _startpos__3_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =     ( make_exp _startpos (Ast.Binop(_1,Ast.Plus,_3)) ) in
            _menhir_goto_additive_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_startp
        | ASSIGN | COMMA | EQ | GT | MINUS | PLUS | R_PAREN | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _, _3, _startpos__3_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =     ( make_exp _startpos (Ast.Binop(_1,Ast.Minus,_3)) ) in
            _menhir_goto_additive_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.stm list) =     ( [] ) in
    _menhir_goto_statement_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | L_PAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | AMP ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp
        | INPUT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | INTEGER_LITERAL _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v _menhir_env._menhir_startp
        | L_PAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | MALLOC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | STAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run60 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run66 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run70 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run72 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | L_PAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _menhir_stack = (_menhir_stack, _startpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | AMP ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp
        | INPUT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
        | INTEGER_LITERAL _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v _menhir_env._menhir_startp
        | L_PAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
        | MALLOC ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
        | NULL ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
        | STAR ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_function_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.function_decl list) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.program) =     ( make_program _startpos _1 ) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x, _startpos_x_), _, xs, _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _v : (Ast.function_decl list) =     ( x :: xs ) in
        _menhir_goto_list_function_declaration_ _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.exp) =      ( make_exp _startpos (Ast.Null) ) in
    _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.exp) =      ( make_exp _startpos (Ast.Malloc) ) in
    _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.exp) =      ( make_exp _startpos (Ast.IntConst _1) ) in
    _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run22 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.exp) =      ( make_exp _startpos (Ast.Input) ) in
    _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v _menhir_env._menhir_startp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_goto_local_variable_declaration_list_nonempty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.identifier list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | SEMICOLON ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.identifier list) =     ( List.rev _1 ) in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (Ast.stm) =    ( make_stm _startpos (Ast.LocalDecl(_2)) ) in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (Ast.stm) =    ( _1 ) in
            _menhir_goto_statement_without_trailing_substatement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_primary_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.exp) =     ( _1 ) in
    _menhir_goto_pointer_expression _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | AMP ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp
    | INPUT ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | INTEGER_LITERAL _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp
    | L_PAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | MALLOC ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | NULL ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | STAR ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
    | R_PAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState33 in
        let _v : (Ast.exp list) =      ( [] ) in
        _menhir_goto_argument_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_goto_pointer_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.exp) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState99 | MenhirState90 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState16 | MenhirState20 | MenhirState46 | MenhirState43 | MenhirState39 | MenhirState36 | MenhirState33 | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = _v in
        let _startpos__1_ = _startpos in
        let _startpos = _startpos__1_ in
        let _v : (Ast.exp) =     ( _1 ) in
        _menhir_goto_multiplicative_expression _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let _startpos__3_ = _startpos in
        let ((_menhir_stack, _menhir_s, _1, _startpos__1_), _startpos__2_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (Ast.exp) =     ( make_exp _startpos (Ast.Binop(_1,Ast.Times,_3)) ) in
        _menhir_goto_multiplicative_expression _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _3 = _v in
        let _startpos__3_ = _startpos in
        let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (Ast.exp) =     ( make_exp _startpos (Ast.Binop(_1,Ast.Divide,_3)) ) in
        _menhir_goto_multiplicative_expression _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _2 = _v in
        let _startpos__2_ = _startpos in
        let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (Ast.exp) =    ( make_exp _startpos (Ast.Unop(Ast.Dereference, _2)) ) in
        _menhir_goto_pointer_expression _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_formal_parameter_list_nonempty : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.identifier list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | IDENTIFIER _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v _menhir_env._menhir_startp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8)
    | R_PAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
        let _v : (Ast.identifier list) =     ( List.rev _1 ) in
        _menhir_goto_formal_parameter_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_formal_parameter_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.identifier list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | R_PAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _startpos__1_), _, _2) = _menhir_stack in
        let _v : (Ast.identifier list) =     ( _2 ) in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | L_BRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState13 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp
            | IF ->
                _menhir_run72 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
            | OUTPUT ->
                _menhir_run70 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
            | STAR ->
                _menhir_run66 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
            | VAR ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
            | WHILE ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
            | RETURN ->
                _menhir_reduce56 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState106 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState99 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState90 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
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
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _startpos = _menhir_env._menhir_startp in
    let _v : (Ast.function_decl list) =     ( [] ) in
    _menhir_goto_list_function_declaration_ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = _v in
    let _startpos__1_ = _startpos in
    let _startpos = _startpos__1_ in
    let _v : (Ast.identifier) =    (make_identifier _startpos _1) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState106 | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | L_PAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | IDENTIFIER _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_startp
            | R_PAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState5 in
                let _v : (Ast.identifier list) =     ( [] ) in
                _menhir_goto_formal_parameter_list _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
        let _v : (Ast.identifier) =     ( _1 ) in
        (match _menhir_s with
        | MenhirState8 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _3 = _v in
            let (_menhir_stack, _menhir_s, _1) = _menhir_stack in
            let _v : (Ast.identifier list) =     ( _3 :: _1 ) in
            _menhir_goto_formal_parameter_list_nonempty _menhir_env _menhir_stack _menhir_s _v
        | MenhirState5 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            let _v : (Ast.identifier list) =     ( [_1] ) in
            _menhir_goto_formal_parameter_list_nonempty _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            _menhir_fail ())
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, _2, _startpos__2_) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (Ast.exp) =    ( make_exp _startpos (Ast.Unop(Ast.Pointer, make_exp _startpos (Ast.Var(_2)))) ) in
        _menhir_goto_pointer_expression _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState99 | MenhirState90 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState16 | MenhirState17 | MenhirState20 | MenhirState26 | MenhirState46 | MenhirState43 | MenhirState39 | MenhirState41 | MenhirState36 | MenhirState33 | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | L_PAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_startp
        | ASSIGN | COMMA | DIV | EQ | GT | MINUS | PLUS | R_PAREN | SEMICOLON | STAR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _v : (Ast.exp) =      ( make_exp _startpos (Ast.Var(_1)) ) in
            _menhir_goto_primary_expression _menhir_env _menhir_stack _menhir_s _v _startpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _1), _, _3, _startpos__3_) = _menhir_stack in
        let _v : (Ast.identifier list) =     ( _3 :: _1 ) in
        _menhir_goto_local_variable_declaration_list_nonempty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _1, _startpos__1_) = _menhir_stack in
        let _v : (Ast.identifier list) =     ( [_1] ) in
        _menhir_goto_local_variable_declaration_list_nonempty _menhir_env _menhir_stack _menhir_s _v
    | MenhirState14 | MenhirState59 | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | AMP ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
            | IDENTIFIER _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp
            | INPUT ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
            | INTEGER_LITERAL _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v _menhir_env._menhir_startp
            | L_PAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
            | MALLOC ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
            | NULL ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
            | STAR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _menhir_env._menhir_startp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and goal : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = 1073741823;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENTIFIER _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v _menhir_env._menhir_startp
    | EOF ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



