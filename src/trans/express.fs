namespace Ack_i86

open System
open ExpressionType

module Express =

    let (|Keyword|_|) = function
        | ".."     ->  Some 0s
        | "exit"   ->  Some 0o1s
        | "fork"   ->  Some 0o2s
        | "read"   ->  Some 0o3s
        | "write"  ->  Some 0o4s
        | "open"   ->  Some 0o5s
        | "close"  ->  Some 0o6s
        | "wait"   ->  Some 0o7s
        | "creat"  ->  Some 0o10s
        | "link"   ->  Some 0o11s
        | "unlink" ->  Some 0o12s
        | "exec"   ->  Some 0o13s
        | "chdir"  ->  Some 0o14s
        | "time"   ->  Some 0o15s
        | "makdir" ->  Some 0o16s
        | "chmod"  ->  Some 0o17s
        | "chown"  ->  Some 0o20s
        | "break"  ->  Some 0o21s
        | "stat"   ->  Some 0o22s
        | "seek"   ->  Some 0o23s
        | "tell"   ->  Some 0o24s
        | "mount"  ->  Some 0o25s
        | "umount" ->  Some 0o26s
        | "setuid" ->  Some 0o27s
        | "getuid" ->  Some 0o30s
        | "stime"  ->  Some 0o31s
        | "fstat"  ->  Some 0o34s
        | "mdate"  ->  Some 0o36s
        | "stty"   ->  Some 0o37s
        | "gtty"   ->  Some 0o40s
        | "nice"   ->  Some 0o42s
        | "signal" ->  Some 0o60s
        | _        ->  None


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
            match sStr with
            | Keyword num -> Convert.ToString num
            | _           -> sStr
        | Expr_Lbl lStr ->
            lStr
        | Expr_Dec num ->
            Convert.ToString num
        | Expr_Oct num ->
            if abs num < 10s then
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


