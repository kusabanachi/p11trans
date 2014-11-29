namespace Ack_i86

module Express =

    open System
    open Expres

    let private oprPrior = function
        | '*' -> 5
        | '/' -> 5
        | '%' -> 5
        | '+' -> 4
        | '-' -> 4
        | '!' -> 4
        | '<' -> 3
        | '>' -> 3
        | '&' -> 2
        | '|' -> 1
        |  x  -> failwithf "Unknown Operator \"%c\"" x

    let rec private exprPrior = function
        | Expr_Op ('^', lhs, _) -> exprPrior lhs
        | Expr_Op (opr,   _, _) -> oprPrior opr
        | _                     -> 0xFFFF


    let rec expr = function
        | Expr_Op (operator, lhs, rhs) ->
            opStr operator lhs rhs
        | Expr_Sym sStr ->
            sStr
        | Expr_Lbl lStr ->
            lStr
        | Expr_Dec num ->
            Convert.ToString num
        | Expr_Oct num ->
            if num < 10s then
                Convert.ToString num
            else
                "0" + Convert.ToString (num, 8)
        | Expr_DChar (c1, c2) ->
            sprintf "[\'%c\'<<8]|\'%c\'" c1 c2
        | Expr_SChar c ->
            sprintf "\'%c\'" c
        | Expr_Group e  ->
            "[" + expr e + "]"

    and private opStr opr lhs rhs =
        if opr = '^' then
            expr lhs
        else
            let opratorStr =
                match opr with
                | '!' -> "+~"
                | '<' -> "<<"
                | '>' -> ">>"
                | _   -> opr.ToString()
            let lhsStr =
                if exprPrior lhs < oprPrior opr then
                    "[" + expr lhs + "]"
                else
                    expr lhs
            let rhsStr =
                expr rhs

            lhsStr + opratorStr + rhsStr


