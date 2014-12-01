
module StatementType

open ReadOp
open Expres
open Addres

type statement =
    | Assignment of string * expr
    | NameLabel of string
    | NumericLabel of int16
    | Eos of char
    | Comment of string
    (* oplineType *)
    | Str of string
    | Byte of expr list
    | Even
    | If of expr
    | EndIf
    | Global of string list
    | Text
    | Data
    | Bss
    | Sob of expr * expr
    | Common of string * expr
    | Expr of expr
    | ExprOp of string * expr
    | SingleOp of string * addr
    | DoubleOp of string * addr * addr

