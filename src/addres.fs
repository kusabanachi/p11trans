
module Addres

open Symtab
open ReadOp
open Expres

type reg =
    | R0 | R1 | R2 | R3 | R4 | R5 | SP | PC

type addr =
    | Reg of expr
    | IncDfr of expr
    | DecDfr of expr
    | IdxDfr of expr * expr
    | Dfr of expr
    | IncDDfr of expr
    | DecDDfr of expr
    | DDfr of expr
    | IdxDDfr of expr * expr
    | Rel of expr
    | Imm of expr
    | RelDfr of expr
    | Abs of expr

let AddressError = "A error in address"
let ParenthesesError = ") Parentheses error"
let IndirectionError = "* indirection(*) used illegally"

let rec addres src =

    let checkReg eType eVal =
        if eVal > 7 || (eType <> 1 && eType < 4) then
            failwith AddressError
        else
            ()

    let readReg src =
        let expr, (eType, eVal), rest = expres src
        checkReg eType eVal |> ignore
        expr, rest

    let readClosing src =
        let closing, rest = readOp src
        match closing with
        | Token_Meta ')' -> rest
        | _ -> failwith ParenthesesError

    let token, rest = readOp src
    let nextToken, nextTokenRest = readOp rest
    match token with
    | Token_Meta '(' ->
        let reg, rest' = readReg rest
        let rest'' = readClosing rest'
        match readOp rest'' with
        | Token_Meta '+', rest''' ->
            IncDfr reg, rest'''
        | _ ->
            Dfr reg, rest''
    | Token_Meta '-' when nextToken = Token_Meta '(' ->
        let reg, rest' = readReg nextTokenRest
        let rest'' = readClosing rest'
        DecDfr reg, rest''
    | Token_Meta '$' ->
        let expr, _, rest' = expres rest
        Imm expr, rest'
    | Token_Meta '*' ->
        if nextToken = Token_Meta '*' then
            failwith IndirectionError

        let addr, rest' = addres rest
        (match addr with
         | Reg    expr       -> Dfr     expr
         | IncDfr expr       -> IncDDfr expr
         | DecDfr expr       -> DecDDfr expr
         | IdxDfr(idx, expr) -> IdxDDfr(idx, expr)
         | Dfr    expr       -> DDfr    expr
         | Rel    expr       -> RelDfr  expr
         | Imm    expr       -> Abs     expr
         | _ -> failwith IndirectionError
         , rest')
    | _ ->
        let expr, (eType, eVal), rest = expres src
        match readOp rest with
        | Token_Meta '(', rest' ->
            let reg, rest'' = readReg rest'
            let rest''' = readClosing rest''
            IdxDfr (expr, reg), rest'''
        | _ when eType = TypeRegister ->
            checkReg eType eVal |> ignore
            Reg expr, rest
        | _ ->
            Rel expr, rest

