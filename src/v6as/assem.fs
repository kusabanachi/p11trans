namespace V6as

open System
open Symtab
open ReadOp
open Expres
open StatementType
open Opline

module Assem =

    let SyntaxError = "X syntax error"
    let AssignError = ". illegal assignment to \".\""
    let SymDefError = "M multiply defined symbol as label"
    let LocalLabelError = "F error in local (\"f\" or \"b\") type symbol"

    let assem src =
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

        let isBuiltinSymbol symStr =
            symType symStr <> 0s

        let localLabelNumberCheck num =
            if num > 9s then
                failwith LocalLabelError

        let rec readStatement src =
            let fst, rest = readOp src
            match fst with
            | Token_Term _
            | Token_Comment _ ->
                [], src
            | _ ->
                let snd, rest' = readOp rest
                match snd with
                | Token_Meta '=' ->
                    match fst with
                    | Token_Symbol symStr ->
                        let rhs, (rhsT, _), rest'' = expres rest'
                        checkDotAssign fst rhsT |> ignore
                        [Assignment (symStr, rhs)], rest''
                    | _ ->
                        failwith SyntaxError
                | Token_Meta ':' ->
                    match fst with
                    | Token_Symbol symStr ->
                        if isBuiltinSymbol symStr then
                            failwith SymDefError
                        let subseqSt, rest'' = readStatement rest'
                        NameLabel symStr :: subseqSt, rest''
                    | Token_Octal num
                    | Token_Decimal num
                    | Token_SChar (_, num)
                    | Token_DChar (_, _, num) ->
                        localLabelNumberCheck num |> ignore
                        let subseqSt, rest'' = readStatement rest'
                        NumericLabel num :: subseqSt, rest''
                    | _ ->
                        failwith SyntaxError
                | _ ->
                    let statement, rest = opline src
                    [statement], rest

        let readComment src =
            let op, rest = readOp src
            match op with
            | Token_Comment text ->
                [Comment text], rest
            | _ ->
                [], src

        let readEos src =
            let op, rest = readOp src
            match op with
            | Token_Term EOT ->
                Eos EOT, ""
            | Token_Term '\n' ->
                Eos '\n', rest
            | Token_Term ';' ->
                Eos ';', rest
            | _ ->
                failwith SyntaxError

        let rec readAssem src =
            match src with
            | "" ->
                []
            | _ ->
                let statement, rest = readStatement src
                let comment, rest'  = readComment rest
                let eos, rest''     = readEos rest'
                statement @ comment @ eos :: readAssem rest''

        readAssem src

