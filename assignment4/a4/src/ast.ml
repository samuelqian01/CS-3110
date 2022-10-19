(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, unop, binop) are used by the parser.  You do not want to
   change them.
 ******************************************************************************)

type id = string

type unop =
  | UopMinus
  | UopNot
  | UopTypeof

type binop =
  | BopPlus
  | BopMinus
  | BopTimes
  | BopDiv
  | BopMod
  | BopLt
  | BopLeq
  | BopGt
  | BopGeq
  | BopEq
  | BopNeq
  | BopEqStrict
  | BopNeqStrict

type scircuit =
  | SAnd
  | SOr

type ifcontrol =
  | Ifthen
  | Ifthenelse

(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr =
  | EString of string
  | EBool of bool
  | EInt of int
  | Undefined
  | EUnop of unop * expr
  | EBnop of binop * expr * expr
  | EScircuit of scircuit * expr * expr
  | EIf of ifcontrol * expr * expr * expr
  | Var of id
  | Let of id * expr * expr
  | Fun of id list * expr
  | Funapp of expr * expr list

(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement
   this type however you wish.  There is only one kind of
   definition---the let [rec] definition---so this type can be quite
   simple.
 ******************************************************************************)

type defn = Letpartial of id * expr

(******************************************************************************
   [phrase] is the type of the AST for phrases. It is used by the
   parser.  You do not want to change it.
 ******************************************************************************)

type phrase =
  | Expr of expr
  | Defn of defn
