
module Assem

open System
open Symtab
open ReadOp
open Expres
open StatementType
open Opline

let SyntaxError = "X syntax error"
let AssignError = ". illegal assignment to \".\""
let SymDefError = "M multiply defined symbol as label"
let LocalLabelError = "F error in local (\"f\" or \"b\") type symbol"

let assem src =
    let isEosOrComment = function
        | Token_Term _
        | Token_Comment _
            -> true
        | _
            -> false

    let checkDotAssign lhs rhsT =
        match lhs with
        | Token_Symbol "." ->
            if rhsT <> TypeText && rhsT <> TypeData && rhsT <> TypeBss then
                failwith AssignError
        | _ ->
            ()

    let isSymbol = function
        | Token_Symbol _ -> true
        | _              -> false

    let isBuiltinSymbol = function
        | Token_Symbol str -> symType str <> 0
        | _                -> false

    let localLabelNumberCheck token =
        let charToInt c = int c - int '0'
        let num = token |> function
            | Token_Octal s ->
                Seq.fold (fun acc c -> acc * 8 + charToInt c) 0 s
            | Token_Decimal s ->
                Int32.Parse s
            | Token_SChar c ->
                charToInt c
            | Token_DChar (c1, c2) ->
                (charToInt c1) <<< 8 + (charToInt c2)
            | _ ->
                0xFFFFFFFF
        if num > 9 then
            failwith LocalLabelError

    let rec readStatement src =
        let fst, rest = readOp src
        if not (isEosOrComment fst) then
            let snd, rest' = readOp rest
            match snd with
            | Token_Meta '=' ->
                if not (isSymbol fst) then
                    failwith SyntaxError
                let rhs, rhsT, rest'' = expres rest'
                checkDotAssign fst rhsT |> ignore
                [Assignment (fst, rhs)], rest''
            | Token_Meta ':' ->
                match fst with
                | Token_Symbol _ ->
                    if isBuiltinSymbol fst then
                        failwith SymDefError
                    let subseqSt, rest'' = readStatement rest'
                    Label fst::subseqSt, rest''
                | Token_Octal _
                | Token_Decimal _
                | Token_SChar _
                | Token_DChar (_, _) ->
                    localLabelNumberCheck fst |> ignore
                    let subseqSt, rest'' = readStatement rest'
                    Label fst::subseqSt, rest''
                | _ ->
                    failwith SyntaxError
            | _ ->
                let statement, rest = opline src
                [statement], rest
        else
            [], src

    let readEosOrComment src =
        let op, rest = readOp src
        match op with
        | Token_Term EOT ->
            Eos EOT, ""
        | Token_Term '\n' ->
            Eos '\n', rest
        | Token_Term ';' ->
            Eos ';', rest
        | Token_Comment text ->
            Comment text, ""
        | _ ->
            failwith SyntaxError


    let rec readSt src =
        match src with
        | "" ->
            []
        | _ ->
            let statement, rest = readStatement src
            let eos, rest' = readEosOrComment rest
            statement @ eos :: readSt rest'

    readSt src

