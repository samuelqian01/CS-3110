
(* The type of tokens. *)

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
  | STRING of (string)
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
  | INT of (string)
  | IN
  | IF
  | ID of (string)
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

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse_phrase: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.phrase)

val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
