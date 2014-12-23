namespace V6as

open Addres
open ExpressionType
open ConditionCode

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
        | Common of string * expression
        | Expr of expression
        | ExprOp of string * expression
        | SingleOp of string * addr
        | DoubleOp of string * addr * addr
        | FlagClear of condFlag
        | FlagSet of condFlag

