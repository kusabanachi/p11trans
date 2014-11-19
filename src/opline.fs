
module Opline

open Symtab
open ReadOp
open Expres
open Addres
open StatementType


let AddressError = "A error in address"
let SyntaxError = "X syntax error"

let opline src =

    let readComma src =
        match readOp src with
        | Token_Meta ',', rest -> rest
        | _ -> failwith AddressError

    let token, rest = readOp src
    match token with
    | Token_String s ->
        printfn "%A" s
        printfn "%A" token
        Str s, rest
    | Token_Symbol sym ->
        match symType sym with
        | 5s | 7s | 10s | 11s | 12s | 24s -> (* double *)
            let lhs, rest' = addres rest
            let rest'' = readComma rest'
            let rhs, rest''' = addres rest''
            DoubleOp (sym, lhs, rhs), rest'''
        | 13s ->  (* single *)
            let addr, rest' = addres rest
            SingleOp (sym, addr), rest'
        | 14s -> (* .byte *)
            let rec readByteExpr acc s =
                let e, _, r = expres s
                let acc' = e::acc
                match readOp r with
                | Token_Meta ',', r' -> readByteExpr acc' r'
                | _ -> acc', r

            let revList, rest' = readByteExpr [] rest
            let byteArray = List.rev revList |> List.toArray
            ByteExpr byteArray, rest'
        | 16s -> (* .even *)
            Even, rest
        | 17s -> (* .if *)
            let expr, _, rest' = expres rest
            If expr, rest'
        | 18s -> (* .endif *)
            EndIf, rest
        | 19s -> (* .global *)
            let rec readGlobalStr acc s =
                let token, r = readOp s
                match token with
                | Token_Symbol gsym ->
                     let acc' = gsym::acc
                     match readOp r with
                     | Token_Meta ',', r' -> readGlobalStr acc' r'
                     | _                  -> acc', r
                | _                       -> acc, s

            let revList, rest' = readGlobalStr [] rest
            let gsymArray = List.rev revList |> List.toArray
            Global gsymArray, rest'
        | 21s -> (* .text *)
            Text, rest
        | 22s -> (* .data *)
            Data, rest
        | 23s -> (* .bss *)
            Bss, rest
        | 25s -> (* sob *)
            let e1, _, rest' = expres rest
            let rest'' = readComma rest'
            let e2, _, rest''' = expres rest''
            Sob (e1, e2), rest'''
        | 26s -> (* .comm *)
            match readOp rest with
            | Token_Symbol sym, rest' ->
                match readOp rest' with
                | Token_Meta ',', rest'' ->
                    let expr, _, rest''' = expres rest''
                    Common (sym, expr), rest'''
                | _ -> failwith SyntaxError
            | _ -> failwith SyntaxError
        | 29s | 30s (* jbr, jeq, etc *)
        | 6s | 8s | 9s | 27s | 28s (* branch, rts, sys, estimated text, estimated data *)->
            let expr, _, rest' = expres rest
            ExprOp (sym, expr), rest'
        | _ ->
            let e, _, rest = expres src
            Expr e, rest
    | _ ->
        let e, _, rest = expres src
        Expr e, rest

