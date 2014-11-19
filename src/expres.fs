
module Expres

open ReadOp
open Symtab

type expr =
    | Expr_Op of char * expr * expr
    | Expr_Sym of string
    | Expr_Lbl of string
    | Expr_Dec of int16
    | Expr_Oct of int16
    | Expr_DChar of char * char
    | Expr_SChar of char
    | Expr_Group of expr

let ExpressionError = "E error in expression"
let ParenthesesError = "] parentheses error"

#if RECURSIVE_DESCENT
// expr  ::= factor expr' | operator factor expr'
// expr' ::= operator factor expr' | None

let rec expres src =

    let operatorMark = function
        | '^' | '|' | '+' | '-' | '*' | '&' | '%' | '!'
        | '<' | '>' | '%' | '/' -> true
        | _ -> false
    let readOperatorMark s =
        match readOp s with
        | Token_Fixor, rest
        | Token_Escp '%', rest ->
            Some('|', rest)
        | Token_Meta char, rest when operatorMark char ->
            Some(char, rest)
        | Token_Escp char, rest when operatorMark char ->
            Some(char, rest)
        | _ ->
            None

    let readFactor src =
        match readOp src with
            | Token_Symbol s, rest ->
                Some( Expr_Sym s, rest )
            | Token_LocalLabel s, rest ->
                Some( Expr_Lbl s, rest )
            | Token_Decimal s, rest ->
                Some( Expr_Dec s, rest )
            | Token_Octal s, rest ->
                Some( Expr_Oct s, rest )
            | Token_Meta '[', rest ->
                let ex, rest' = expres rest
                match readOp rest' with
                | Token_Meta ']', rest'' -> Some( Expr_Group ex, rest'' )
                | _ -> failwith ParenthesesError
            | _ -> None

    let readExpr src =
        let rec readExpr' lhs src =
            let rec readOperator s =
                match readOperatorMark s with
                | Some('+', rest) ->
                    readOperator rest
                | Some(x) ->
                    x
                | _ ->
                    '+', s
            let opr, rest = readOperator src
            match readFactor rest with
            | Some(rhs, rest') ->
                let lhs' = Expr_Op (opr, lhs, rhs)
                readExpr' lhs' rest'
            | _ ->
                lhs, rest

        match readFactor src with
        | Some(lhs, rest) ->
            readExpr' lhs rest
        | _ ->
            match readOperatorMark src with
            | Some(opr, rest) ->
                match readFactor rest with
                | Some(factor, rest') ->
                    let lhs = Expr_Op (opr, EDec "0"B, factor)
                    readExpr' lhs rest'
                | _ ->
                    failwith ExpressionError
            | _ ->
                failwith ExpressionError

    let result, rest = readExpr src
    // 数式の後に続く字句がオペレータの場合、オペレータが連続したのでエラー
    if (readOperatorMark rest).IsSome then
        failwith ExpressionError
    result, rest

#else
let rec expres src =

    let opr = ref '+'
    let opfound = ref 0
    let oprfound = ref 0
    let exprT = ref TypeAbsolute
    let exprVal = ref 0s

    let (|Sym|_|) = function
        | Token_Symbol s, rest -> Some( Expr_Sym s, (symType s, symVal s), rest )
        | _ -> None
    let (|Lbl|_|) = function
        | Token_LocalLabel s, rest -> Some( Expr_Lbl s, (TypeText, 0s), rest )
        | _ -> None
    let (|Dec|_|) = function
        | Token_Decimal n, rest -> Some( Expr_Dec n, (TypeAbsolute, n), rest )
        | _ -> None
    let (|Oct|_|) = function
        | Token_Octal n, rest -> Some( Expr_Oct n, (TypeAbsolute, n), rest )
        | _ -> None
    let (|DChar|_|) = function
        | Token_DChar (c1, c2, n), rest -> Some( Expr_DChar(c1, c2), (TypeAbsolute, n), rest )
        | _ -> None
    let (|SChar|_|) = function
        | Token_SChar (c, n), rest -> Some( Expr_SChar c, (TypeAbsolute, n), rest )
        | _ -> None
    let (|Grp|_|) = function
        | Token_Meta '[', rest ->
            let ex, t_v, rest' = expres rest
            match readOp rest' with
            | Token_Meta ']', rest'' -> Some( Expr_Group ex, t_v, rest'' )
            | _ -> failwith ParenthesesError
        | _ -> None

    let operatorMark = function
        | '^' | '<' | '>' | '|' | '+'
        | '-' | '*' | '/' | '&' | '%' | '!' -> true
        | _ -> false
    let (|Operater|_|) = function
        | Token_Fixor, rest
        | Token_Escp '%', rest ->
            Some('|', rest)
        | Token_Escp char, rest when operatorMark char ->
            Some(char, rest)
        | Token_Meta char, rest when operatorMark char ->
            Some(char, rest)
        | _ ->
            None

    let combine rhsT =
        if !exprT = 0s && rhsT = 0s then
            0s
        elif !opr = '^' then
            rhsT
        elif !opr = '-' && !exprT = rhsT then
            TypeAbsolute
        elif !exprT >= rhsT then
            !exprT
        else
            rhsT

    let calc rhsV =
        let lShift lhs rhs =
            if rhs >= 0s then
                lhs <<< int rhs
            else
                lhs >>> int -rhs

        let rShift (lhs:int16) (rhs:int16):int16 =
            if rhs >= 0s then
                lhs >>> int rhs
            else
                int16 (int lhs &&& 0xfffe) <<< int -rhs

        match !opr with
        | '<' -> lShift !exprVal rhsV
        | '>' -> rShift !exprVal rhsV
        | '|' -> !exprVal ||| rhsV
        | '+' -> !exprVal  +  rhsV
        | '-' -> !exprVal  -  rhsV
        | '*' -> !exprVal  *  rhsV
        | '/' -> !exprVal  /  rhsV
        | '&' -> !exprVal &&& rhsV
        | '%' -> !exprVal  %  rhsV
        | '!' -> !exprVal  + (~~~ rhsV)
        | _   -> !exprVal

    let rec expres' lhs src =
        match readOp src with
        | Sym (operand, (opType, opVal), rest)
        | Lbl (operand, (opType, opVal), rest)
        | Dec (operand, (opType, opVal), rest)
        | Oct (operand, (opType, opVal), rest)
        | DChar (operand, (opType, opVal), rest)
        | SChar (operand, (opType, opVal), rest)
        | Grp (operand, (opType, opVal), rest) ->
            opfound := !opfound + 1
            exprT := combine opType
            exprVal := calc opVal
            if !opfound = 1 && !oprfound = 0 then
                expres' operand rest
            else
                let lhs' = Expr_Op(!opr, lhs, operand)
                opr := '+'
                expres' lhs' rest
        | Operater (operator, rest) ->
            if !opr <> '+' then
                failwith ExpressionError
            else
                opr := operator
                oprfound := 1
                expres' lhs rest
        | _ ->
            lhs, (!exprT, !exprVal), src

    let result = expres' (Expr_Dec 0s) src
    if !opfound = 0 then
        failwith ExpressionError
    result

#endif