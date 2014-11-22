
module Addres

open Symtab
open ReadOp
open Expres

type reg =
    | R0 | R1 | R2 | R3 | R4 | R5 | SP | PC

type addr =
    | Reg of reg
    | IncDfr of reg
    | DecDfr of reg
    | IdxDfr of reg * expr
    | Dfr of reg
    | IncDDfr of reg
    | DecDDfr of reg
    | DDfr of reg
    | IdxDDfr of reg * expr
    | Rel of expr
    | Imm of expr
    | RelDfr of expr
    | Abs of expr

let AddressError = "A error in address"
let ParenthesesError = ") Parentheses error"
let IndirectionError = "* indirection(*) used illegally"

let rec addres src =

    let toRegister eType eVal =
        if eType <> 1s && eType < 4s then
            failwith AddressError
        match eVal with
        | 0s -> R0
        | 1s -> R1
        | 2s -> R2
        | 3s -> R3
        | 4s -> R4
        | 5s -> R5
        | 6s -> SP
        | 7s -> PC
        | _  -> failwith AddressError

    let readReg src =
        let _, (eType, eVal), rest = expres src
        toRegister eType eVal, rest

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
         | Reg    reg        -> Dfr     reg
         | IncDfr reg        -> IncDDfr reg
         | DecDfr reg        -> DecDDfr reg
         | IdxDfr(reg, expr) -> IdxDDfr(reg, expr)
         | Dfr    reg        -> DDfr    reg
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
            IdxDfr (reg, expr), rest'''
        | _ when eType = TypeRegister ->
            let reg = toRegister eType eVal
            Reg reg, rest
        | _ ->
            Rel expr, rest

