namespace V6as

open Addres
open ExpressionType

module StatementType =

    type statement =
        | Assignment of string * expression
        | NameLabel of string
        | NumericLabel of int16
        | Eos of char
        | Comment of string
        (* oplineType *)
        | Str of string
        | Byte of expression list
        | Even
        | If of expression
        | EndIf
        | Global of string list
        | Text
        | Data
        | Bss
        | Sob of expression * expression
        | Common of string * expression
        | Expr of expression
        | ExprOp of string * expression
        | SingleOp of string * addr
        | DoubleOp of string * addr * addr

