
module StatementType

open ReadOp
open Expres
open Addres

type statement =
    | Assignment of Token * expr
    | NameLabel of string
    | NumericLabel of int16
    | Eos of char
    | Comment of string
    (* oplineType *)
    | Str of string
    | Expr of expr
    | ByteExpr of expr []
    | Even
    | If of expr
    | EndIf
    | Global of string []
    | Text
    | Data
    | Bss
    | Sob of expr * expr
    | Common of string * expr
    | ExprOp of string * expr
    | SingleOp of string * addr
    | DoubleOp of string * addr * addr

