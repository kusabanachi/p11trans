
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

    let readReg src =
        let regExp, t, rest = expres src
        if t <> 1 && t < 4 then
            failwith AddressError
        else
            regExp, rest

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
        let exp, _, rest' = expres rest
        Imm exp, rest'
    | Token_Meta '*' ->
        if nextToken = Token_Meta '*' then
            failwith IndirectionError

        let addr, rest' = addres rest
        (match addr with
         | Reg    exp       -> Dfr     exp
         | IncDfr exp       -> IncDDfr exp
         | DecDfr exp       -> DecDDfr exp
         | IdxDfr(idx, exp) -> IdxDDfr(idx, exp)
         | Dfr    exp       -> DDfr    exp
         | Rel    exp       -> RelDfr  exp
         | Imm    exp       -> Abs     exp
         | _ -> failwith IndirectionError
         , rest')
    | _ ->
        let exp, expT, rest = expres src
        match readOp rest with
        | Token_Meta '(', rest' ->
            let reg, rest'' = readReg rest'
            let rest''' = readClosing rest''
            IdxDfr (exp, reg), rest'''
        | _ when expT = TypeRegister ->
            Reg exp, rest
        | _ ->
            Rel exp, rest

