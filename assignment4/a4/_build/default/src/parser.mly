(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

(* Acknowledgement:  this parser is adapted from the OCaml 4.04 parser
 *  [https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly],
 *  written by Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *  and distributed under the GNU Lesser General Public License version 2.1. *)

%{
open Ast
open Ast_factory

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Stdlib.compare lst)
%}

%token <string> INT
%token <string> ID STRING
%token PLUS MINUS TIMES DIV MOD AND OR
       LT LEQ GT GEQ EQUAL NOTEQUAL EQUALEQUAL NOTEQUALEQUAL
       NOT TYPEOF
%token LPAREN RPAREN SEMI DOUBLE_SEMI ARROW DEREF ASSIGN UPDATE LBRACE RBRACE
       COLON COMMA LBRACKET RBRACKET DOT
%token TRUE FALSE UNDEFINED
%token LET IN IF THEN ELSE BEGIN END THROW TRY CATCH HANDLE FINALLY FUN REF
       WHILE DO DONE DELETE
%token EOF

(* The entries commented out below are unnecessary, but indicate
   the correct place in the precedence table for those tokens
   should they ever become necessary. *)

(* %nonassoc IN *)
%nonassoc below_SEMI
%nonassoc SEMI
(* %nonassoc LET *)
(* %nonassoc CATCH *)
%nonassoc HANDLE
%nonassoc FINALLY
%nonassoc THEN
%nonassoc ELSE
%nonassoc UPDATE
%nonassoc ASSIGN
(* %right ARROW *)
%right OR
%right AND
%left EQUAL NOTEQUAL EQUALEQUAL NOTEQUALEQUAL LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc DOT
%nonassoc NOT TYPEOF DEREF LBRACKET
(* %nonassoc BEGIN FALSE LPAREN TRUE UNDEFINED *)

%start <Ast.expr> parse_expression
%start <Ast.phrase> parse_phrase

%%

parse_expression:
  | e = seq_expr; EOF
        { e }

parse_phrase:
  | e = seq_expr; DOUBLE_SEMI?; EOF
    { Expr e }
  | d = defn; DOUBLE_SEMI?; EOF
    { Defn d }
  | DOUBLE_SEMI?; EOF
    { raise End_of_file }
  ;

defn:
  | LET; x = ID; EQUAL; e = seq_expr
    { make_let_defn x e }
  | LET; f = ID;
    LPAREN; xs = nonempty_list(ident); RPAREN;
    EQUAL; e = seq_expr
    { make_let_fn_defn f xs e }
  ;

seq_expr:
  | e = expr; %prec below_SEMI
    { e }
  | e = expr; SEMI
    { e }
  | e = expr; SEMI; s = seq_expr;
    { make_seq e s }

expr:
  | e = simple_expr
    { e }
  | e = simple_expr; es = nonempty_list(simple_expr)
    { make_app e es }
  | e1 = expr; bop = binop; e2 = expr
    { make_binop bop e1 e2 }
  | e1 = expr; ASSIGN; e2 = expr
    { make_assign e1 e2 }
  | MINUS; e = expr
    { make_unop UopMinus e }
  | e1 = simple_expr; LBRACKET e2 = seq_expr; RBRACKET; UPDATE; e3 = expr
    { make_update_field e1 e2 e3 }
  | e1 = simple_expr; DOT x2 = ID; UPDATE; e3 = expr
    { make_update_field e1 (make_string x2) e3 }
  | e1 = expr; AND; e2 = expr
    { make_and e1 e2 }
  | e1 = expr; OR; e2 = expr
    { make_or e1 e2 }
  | IF; e1 = seq_expr; THEN; e2 = expr; ELSE; e3 = expr
    { make_if e1 e2 e3 }
  | IF; e1 = seq_expr; THEN; e2 = expr
    { make_if_partial e1 e2 }
  | LET; x = ID; EQUAL; e1 = expr; IN; e2 = seq_expr
    { make_let x e1 e2 }
  | LET; f = ID; LPAREN; xs = nonempty_list(ident); RPAREN; EQUAL;
    e1 = expr; IN; e2 = seq_expr
    { make_let_fn f xs e1 e2 }
  | TRY; e1 = seq_expr; CATCH; x = ID; HANDLE; e2 = seq_expr
    { make_try e1 x e2 }
  | TRY; e1 = seq_expr; CATCH; x = ID; HANDLE; e2 = seq_expr;
    FINALLY; e3 = seq_expr
    { make_try_finally e1 x e2 e3 }
  | THROW; e = simple_expr
    { make_throw e}
  | REF; e = simple_expr
    { make_ref e }
  | FUN; LPAREN; xs = nonempty_list(ident); RPAREN; ARROW; e = seq_expr
    { if has_dups xs then $syntaxerror (* duplicate argument names *) else
        make_fun xs e }
  | WHILE; e1 = seq_expr; DO; e2 = seq_expr; DONE
    { make_while e1 e2 }
  | DELETE e1 = simple_expr; LBRACKET; e2 = seq_expr; RBRACKET
    { make_delete_field e1 e2 }
  | DELETE e = simple_expr; DOT; x = ident
    { make_delete_field e (make_string x) }
  ;

simple_expr:
  | x = ident
    { make_var x }
  | LPAREN; e = seq_expr; RPAREN
    { e }
  | BEGIN; e = seq_expr; END
    { e }
  | uop = unop; e = simple_expr
    { make_unop uop e }
  | DEREF; e = simple_expr
    { make_deref e }
  | s = INT
    { (* The rather curious code below relies on the two's complement
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
      | e -> raise e }
  | s = STRING
    { make_string s }
  | TRUE
    { make_bool true }
  | FALSE
    { make_bool false }
  | UNDEFINED
    { make_undefined () }
  | LBRACE; fields = separated_list(COMMA, field_bind); RBRACE
    { if fields |> List.map fst |> has_dups then
        $syntaxerror (* duplicate fields *)
      else
        make_object fields }
  | e1 = simple_expr; LBRACKET e2 = seq_expr; RBRACKET
    { make_get_field e1 e2 }
  | e1 = simple_expr; DOT; x = ident
    { make_get_field e1 (make_string x) }
  ;

field_bind:
  | f = STRING; COLON; e = expr
    { (f, e) }
  ;

ident:
  | x = ID
    { x }
  ;

%inline unop:
  | NOT { UopNot }
  | TYPEOF { UopTypeof }
  (* MINUS requires special handling because it can occur as a
     a unary or a binary operator. See the [expr] production rule
     for its use as a unary operator. *)

%inline binop:
  | PLUS { BopPlus }
  | MINUS { BopMinus }
  | TIMES { BopTimes }
  | DIV { BopDiv }
  | MOD { BopMod }
  | LT { BopLt }
  | LEQ { BopLeq }
  | GT { BopGt }
  | GEQ { BopGeq }
  | EQUAL { BopEq }
  | NOTEQUAL { BopNeq }
  | EQUALEQUAL { BopEqStrict }
  | NOTEQUALEQUAL { BopNeqStrict }
  ;
