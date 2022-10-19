open Ast

let make_let_defn x e = Letpartial (x, e)

let make_let_fn_defn f x e =
  raise (Failure "Unimplemented: Ast_factory.make_let_fn_defn")

let make_seq e1 e2 =
  raise (Failure "Unimplemented: Ast_factory.make_seq")

let make_app e es = Funapp (e, es)

let make_unop uop e = EUnop (uop, e)

let make_binop bop e1 e2 = EBnop (bop, e1, e2)

let make_and e1 e2 = EScircuit (SAnd, e1, e2)

let make_or e1 e2 = EScircuit (SOr, e1, e2)

let make_if e1 e2 e3 = EIf (Ifthenelse, e1, e2, e3)

let make_if_partial e1 e2 = EIf (Ifthen, e1, e2, Undefined)

let make_let x e1 e2 = Let (x, e1, e2)

let make_let_fn f x e1 e2 =
  raise (Failure "Unimplemented: Ast_factory.make_let_fn")

let make_try e1 x e2 =
  raise (Failure "Unimplemented: Ast_factory.make_try")

let make_try_finally e1 x e2 e3 =
  raise (Failure "Unimplemented: Ast_factory.make_try_finally")

let make_throw e =
  raise (Failure "Unimplemented: Ast_factory.make_throw")

let make_ref e = raise (Failure "Unimplemented: Ast_factory.make_ref")

let make_deref e =
  raise (Failure "Unimplemented: Ast_factory.make_deref")

let make_assign e1 e2 =
  raise (Failure "Unimplemented: Ast_factory.make_assign")

let make_fun xs e = Fun (xs, e)

let make_while e1 e2 =
  raise (Failure "Unimplemented: Ast_factory.make_while")

let make_delete_field e1 e2 =
  raise (Failure "Unimplemented: Ast_factory.make_delete_field")

let make_var x = Var x

let make_int i = EInt i

let make_string s = EString s

let make_bool b = EBool b

let make_undefined () = Undefined

let make_object fields =
  raise (Failure "Unimplemented: Ast_factory.make_object")

let make_get_field e1 e2 =
  raise (Failure "Unimplemented: Ast_factory.make_get_field")

let make_update_field e1 e2 e3 =
  raise (Failure "Unimplemented: Ast_factory.make_update_field")
