open Ast

exception Unbound

exception Notfun

type value =
  | VBool of bool
  | VInt of int
  | VString of string
  | VUndefined
  | Closure of env * expr * id list

and result =
  | RValue of value
  | RExcn of value

and env = (id * value) list

and state = unit

let initial_env : (id * value) list = []

let initial_state = ()

let string_of_value v =
  match v with
  | VBool b -> string_of_bool b
  | VInt i -> string_of_int i
  | VString s -> "\"" ^ String.escaped s ^ "\""
  | VUndefined -> "undefined"
  | Closure (_, _, _) -> "<function>"

let string_of_result = function
  | RValue v -> string_of_value v
  | RExcn v -> "Exception: " ^ string_of_value v

let string_of_env env =
  raise (Failure "Unimplemented: Eval.string_of_env")

let string_of_state st =
  raise (Failure "Unimplemented: Eval.string_of_state")

let value_to_bool v =
  match v with
  | EInt i -> if i = 0 then VBool false else VBool true
  | EString s -> if s = "" then VBool false else VBool true
  | EBool b -> VBool b
  | Undefined -> VBool false
  | Fun (a, b) -> VBool true
  | _ -> VUndefined

let value_to_int v =
  match v with
  | EInt i -> VInt i
  | EBool b -> if b then VInt 1 else VInt 0
  | Undefined -> VUndefined
  | EString s -> (
      try VInt (int_of_string s) with
      | Failure _ -> VUndefined)
  | _ -> VUndefined

let value_to_string v =
  match v with
  | EInt i -> VString (string_of_int i)
  | EBool b -> if b then VString "true" else VString "false"
  | Undefined -> VString "undefined"
  | EString s -> VString s
  | _ -> VString "undefined"

let value_to_expression v =
  match v with
  | VInt i -> EInt i
  | VString s -> EString s
  | VBool b -> EBool b
  | VUndefined -> Undefined
  | Closure (_, a, b) -> Fun (b, a)

let value_to_primitive v =
  match v with
  | EInt i -> EInt i
  | EString s -> EString s
  | EBool b -> EBool b
  | Undefined -> Undefined
  | _ -> Undefined

let vs_to_s v =
  match v with
  | VString s -> s
  | _ -> raise (Failure "wrong conversion")

let vi_to_i v =
  match v with
  | VInt i -> i
  | _ -> raise (Failure "wrong conversion")

let vb_to_b v =
  match v with
  | VBool b -> b
  | _ -> raise (Failure "wrong conversion")

let expression_to_value e =
  match e with
  | EInt i -> VInt i
  | EString s -> VString s
  | EBool b -> VBool b
  | Undefined -> VUndefined
  | Fun (a, b) -> Closure ([], b, a) (**may require a fix*)
  | _ -> VUndefined

let tuple_to_exp tuple =
  match tuple with
  | RValue (VBool b), st -> EBool b
  | RValue (VString s), st -> EString s
  | RValue (VInt i), st -> EInt i
  | RValue VUndefined, st -> Undefined
  | RValue (Closure (_, a, b)), st -> Fun (b, a)
  | RExcn _, _ -> raise Unbound

let tuple_to_exp_divmod tuple = 
  match tuple with
  | RValue (VBool b), st -> EBool b
  | RValue (VString s), st -> EString s
  | RValue (VInt i), st -> EInt i
  | RValue VUndefined, st -> Undefined
  | RValue (Closure (_, a, b)), st -> Fun (b, a)
  | RExcn _, _ -> raise Division_by_zero

let tuple_to_val tuple =
  match tuple with
  | RValue (VBool b), st -> VBool b
  | RValue (VString s), st -> VString s
  | RValue (VInt i), st -> VInt i
  | RValue VUndefined, st -> VUndefined
  | RValue (Closure (a, b, c)), st -> Closure (a, b, c)
  | RExcn _, _ -> raise Unbound

let rec subst e v x =
  match e with
  | Var y -> if x = y then v else e
  | EBool _ -> e
  | EInt _ -> e
  | EString _ -> e
  | Undefined -> e
  | EUnop (uop, e1) -> EUnop (uop, subst e1 v x)
  | EBnop (bop, e1, e2) -> EBnop (bop, subst e1 v x, subst e2 v x)
  | EScircuit (scir, e1, e2) ->
      EScircuit (scir, subst e1 v x, subst e2 v x)
  | EIf (ifs, e1, e2, e3) ->
      EIf (ifs, subst e1 v x, subst e2 v x, subst e3 v x)
  | Let (id, e1, e2) ->
      let e1' = subst e1 v x in
      if x = id then Let (id, e1', e2) else Let (id, e1', subst e2 v x)
  | Fun (_, _) -> e
  | Funapp (_, _) -> e

let rec eval_expr (e, env, st) =
  try
    match e with
    | EBool b -> (RValue (VBool b), st)
    | EInt i -> (RValue (VInt i), st)
    | EString s -> (RValue (VString s), st)
    | Undefined -> (RValue VUndefined, st)
    | EUnop (unop, e) -> (RValue (unop_expr_helper unop e env st), st)
    | EBnop (bop, e1, e2) -> (RValue (binop_expr_helper bop e1 e2 env st), st)
    | EScircuit (scir, e1, e2) ->
        (RValue (scircuit_expr_helper scir e1 e2 env st), st)
    | EIf (ifs, e1, e2, e3) ->
        (RValue (if_helper ifs e1 e2 e3 env st), st)
    | Var x -> var_helper x env st
    | Let (id, e1, e2) ->
        let_e2_evaluate id e1 e2 (let_e1_evaluate id e1 env st) st
    | Fun (ids, e1) -> RValue(fun_expr_helper ids e1 env st), st
    | Funapp (expr, expr_list) -> fun_app_helper expr expr_list env st
  with
  | Unbound -> (RExcn (VString "Error: Unbound variable"), st)
  | Notfun -> (RExcn (VString "Error: Not a function"), st)
  | Division_by_zero -> (RExcn (VString "Error: Division by zero"), st)

and unop_expr_helper unop e env st =
  match unop with
  | UopMinus -> (
      match eval_expr (e, env, st) |> tuple_to_exp with
      | EInt i -> VInt (i * -1)
      | EBool b -> (
          match value_to_int (EBool b) with
          | VInt num -> VInt (-1 * num)
          | _ -> raise (Failure "impossible"))
      | Fun (_, _) -> VUndefined
      | _ -> VUndefined)
  | UopNot -> (
      match eval_expr (e, env, st) |> tuple_to_exp with
      | EInt i -> (
          match value_to_bool (EInt i) with
          | VBool tf -> if tf = true then VBool false else VBool true
          | _ -> raise (Failure "impossible"))
      | EBool b -> if b = true then VBool false else VBool true
      | EString s -> (
          match value_to_bool (EString s) with
          | VBool tf -> if tf = true then VBool false else VBool true
          | _ -> raise (Failure "impossible"))
      | Fun (_,_) -> VBool false 
      | _ -> VUndefined)
  | UopTypeof -> (
      match eval_expr (e, env, st) |> tuple_to_exp with
      | Fun (_, _) -> VString "function"
      | Undefined -> VString "undefined"
      | EBool _ -> VString "bool"
      | EInt _ -> VString "int"
      | EString _ -> VString "string"
      | _ -> raise (Failure "to do if doing extern, loc, closure, etc.")
      )

and binop_expr_helper bop e1 e2 env st =
  match bop with
  | BopMinus -> (
      let v1int =
        value_to_int (eval_expr (e1, env, st) |> tuple_to_exp)
      in
      let v2int =
        value_to_int (eval_expr (e2, env, st) |> tuple_to_exp)
      in
      match (v1int, v2int) with
      | VInt i1, VInt i2 -> VInt (i1 - i2)
      | _, _ -> VUndefined)
  | BopPlus -> (
      let v1prim =
        value_to_primitive (eval_expr (e1, env, st) |> tuple_to_exp)
      in
      let v2prim =
        value_to_primitive (eval_expr (e2, env, st) |> tuple_to_exp)
      in
      match (v1prim, v2prim) with
      | EString s1, notundef ->
          VString (s1 ^ (value_to_string notundef |> vs_to_s))
      | notundef, EString s2 ->
          VString ((value_to_string notundef |> vs_to_s) ^ s2)
      | a, b -> (
          match (a |> value_to_int, b |> value_to_int) with
          | VUndefined, _
          | _, VUndefined ->
              VUndefined
          | c, d -> VInt (vi_to_i c + vi_to_i d)))
  | BopTimes -> (
      let v1int =
        value_to_int (eval_expr (e1, env, st) |> tuple_to_exp)
      in
      let v2int =
        value_to_int (eval_expr (e2, env, st) |> tuple_to_exp)
      in
      match (v1int, v2int) with
      | VInt i1, VInt i2 -> VInt (i1 * i2)
      | _, _ -> VUndefined)
  | BopDiv -> (
      let v1int =
        value_to_int (eval_expr (e1, env, st) |> tuple_to_exp_divmod)
      in
      let v2int =
        value_to_int (eval_expr (e2, env, st) |> tuple_to_exp_divmod)
      in
      match (v1int, v2int) with
      | VInt i1, VInt i2 ->
          if i2 <> 0 then VInt (i1 / i2) else raise Division_by_zero
      | _, _ -> VUndefined)
  | BopMod -> (
      let v1int =
        value_to_int (eval_expr (e1, env, st) |> tuple_to_exp_divmod)
      in
      let v2int =
        value_to_int (eval_expr (e2, env, st) |> tuple_to_exp_divmod)
      in
      match (v1int, v2int) with
      | VInt i1, VInt i2 ->
          if i2 <> 0 then VInt (i1 mod i2) else raise Division_by_zero
      | _, _ -> VUndefined)
  | BopLt -> (
      let v1prim =
        value_to_primitive (eval_expr (e1, env, st) |> tuple_to_exp)
      in
      let v2prim =
        value_to_primitive (eval_expr (e2, env, st) |> tuple_to_exp)
      in
      match (v1prim, v2prim) with
      | EString s1, EString s2 -> VBool (s1 < s2)
      | a, b -> (
          match (value_to_int a, value_to_int b) with
          | VUndefined, _
          | _, VUndefined ->
              VUndefined
          | c, d -> VBool (c < d)))
  | BopLeq -> (
      let v1prim =
        value_to_primitive (eval_expr (e1, env, st) |> tuple_to_exp)
      in
      let v2prim =
        value_to_primitive (eval_expr (e2, env, st) |> tuple_to_exp)
      in
      match (v1prim, v2prim) with
      | EString s1, EString s2 -> VBool (s1 <= s2)
      | a, b -> (
          match (value_to_int a, value_to_int b) with
          | VUndefined, _
          | _, VUndefined ->
              VUndefined
          | c, d -> VBool (c <= d)))
  | BopGt -> (
      let v1prim =
        value_to_primitive (eval_expr (e1, env, st) |> tuple_to_exp)
      in
      let v2prim =
        value_to_primitive (eval_expr (e2, env, st) |> tuple_to_exp)
      in
      match (v1prim, v2prim) with
      | EString s1, EString s2 -> VBool (s1 > s2)
      | a, b -> (
          match (value_to_int a, value_to_int b) with
          | VUndefined, _
          | _, VUndefined ->
              VUndefined
          | c, d -> VBool (c > d)))
  | BopGeq -> (
      let v1prim =
        value_to_primitive (eval_expr (e1, env, st) |> tuple_to_exp)
      in
      let v2prim =
        value_to_primitive (eval_expr (e2, env, st) |> tuple_to_exp)
      in
      match (v1prim, v2prim) with
      | EString s1, EString s2 -> VBool (s1 >= s2)
      | a, b -> (
          match (value_to_int a, value_to_int b) with
          | VUndefined, _
          | _, VUndefined ->
              VUndefined
          | c, d -> VBool (c >= d)))
  | BopEq -> (
      match
        ( eval_expr (e1, env, st) |> tuple_to_exp,
          eval_expr (e2, env, st) |> tuple_to_exp )
      with
      | Undefined, Undefined -> VBool true
      | EBool b1, EBool b2 -> VBool (b1 = b2)
      | EString s1, EString s2 -> VBool (s1 = s2)
      | EInt i1, EInt i2 -> VBool (i1 = i2)
      | EString s1, EInt i1 ->
          binop_expr_helper bop
            (EString s1 |> value_to_int |> value_to_expression)
            (EInt i1) env st
      | EInt i1, EString s1 ->
          binop_expr_helper bop (EInt i1)
            (EString s1 |> value_to_int |> value_to_expression)
            env st
      | EBool b1, d ->
          binop_expr_helper bop
            (EBool b1 |> value_to_int |> value_to_expression)
            d env st
      | c, EBool b1 ->
          binop_expr_helper bop c
            (EBool b1 |> value_to_int |> value_to_expression)
            env st
      | _ -> VBool false)
  | BopNeq ->
      unop_expr_helper UopNot
        (binop_expr_helper BopEq
           (eval_expr (e1, env, st) |> tuple_to_exp)
           (eval_expr (e2, env, st) |> tuple_to_exp)
           env st
        |> value_to_expression)
        env st
  | BopEqStrict -> (
      match
        ( eval_expr (e1, env, st) |> tuple_to_exp,
          eval_expr (e2, env, st) |> tuple_to_exp )
      with
      | Undefined, Undefined -> VBool true
      | EBool b1, EBool b2 -> VBool (b1 = b2)
      | EString s1, EString s2 -> VBool (s1 = s2)
      | EInt i1, EInt i2 -> VBool (i1 = i2)
      | _ -> VBool false)
  | BopNeqStrict ->
      unop_expr_helper UopNot
        (binop_expr_helper BopEqStrict
           (eval_expr (e1, env, st) |> tuple_to_exp)
           (eval_expr (e2, env, st) |> tuple_to_exp)
           env st
        |> value_to_expression)
        env st

and scircuit_expr_helper scir e1 e2 env st =
  match scir with
  | SAnd ->
      if
        eval_expr (e1, env, st)
        |> tuple_to_exp |> value_to_bool = VBool true
      then eval_expr (e2, env, st) |> tuple_to_exp |> value_to_bool
      else eval_expr (e1, env, st) |> tuple_to_exp |> value_to_bool
  | SOr ->
      if
        eval_expr (e1, env, st)
        |> tuple_to_exp |> value_to_bool = VBool true
      then eval_expr (e1, env, st) |> tuple_to_exp |> value_to_bool
      else eval_expr (e2, env, st) |> tuple_to_exp |> value_to_bool

and if_helper ifs e1 e2 e3 env st =
  match ifs with
  | Ifthenelse ->
      if
        eval_expr (e1, env, st)
        |> tuple_to_exp |> value_to_bool = VBool true
      then
        eval_expr (e2, env, st) |> tuple_to_exp |> expression_to_value
      else
        eval_expr (e3, env, st) |> tuple_to_exp |> expression_to_value
  | Ifthen ->
      if
        eval_expr (e1, env, st)
        |> tuple_to_exp |> value_to_bool = VBool true
      then
        eval_expr (e2, env, st) |> tuple_to_exp |> expression_to_value
      else VUndefined

and let_e1_evaluate id e1 env st =
  (id, eval_expr (e1, env, st) |> tuple_to_val) :: initial_env

and let_e2_evaluate id e1 e2 env st =
  if List.mem_assoc id env then
    ( RValue
        (subst
           (eval_expr (e2, env, st) |> tuple_to_exp)
           (List.assoc id env |> value_to_expression)
           id
        |> expression_to_value),
      st )
  else eval_expr (e2, env, st)

and var_helper x env st =
  match List.mem_assoc x env with
  | true -> (RValue (List.assoc x env), st)
  | false -> (RExcn (VString "Unbound variable"), st)

and def_env ids e = 
  List.map (fun idelement -> (idelement, VUndefined)) ids

and fun_expr_helper ids e env st = 
  Closure (def_env ids e, e, ids)

and fun_app_helper expr explist env st = 
  match eval_expr (expr, env, st) with
   | RValue (Closure (defenv, e1, _)), st -> 
    let arguments = 
      List.map (fun element -> eval_expr (element, env, st) 
        |> tuple_to_val) explist in 
    let defenv_bindings = List.map (fun element -> fst element) defenv in 
    let update_def = 
      List.combine defenv_bindings arguments in eval_expr (e1, update_def, st)
   | _ -> raise Notfun

let rec eval_defn (d, env, st) =
  match d with
  | Letpartial (id, expr) ->
      ( eval_expr (Var id, letpartial_env_modifier d env st, st) |> fst,
        letpartial_env_modifier d env st,
        st )

and letpartial_env_modifier d env st =
  match d with
  | Letpartial (id, expr) ->
      (id, eval_expr (expr, env, st) |> tuple_to_val) :: initial_env

let eval_phrase (p, env, st) =
  match p with
  | Expr c ->
      let r, st' = eval_expr (c, env, st) in
      (r, env, st')
  | Defn d -> eval_defn (d, env, st)

let eval_expr_init e = eval_expr (e, initial_env, initial_state)
