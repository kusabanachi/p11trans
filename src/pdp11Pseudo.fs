module p11trans.pdp11Pseudo

(* read pdp11's pseudo-operation assembly codes *)

open p11trans.utility
open p11trans.intermediate


// convert string to expression.
// string -> expr option
let toExpr str = Some(Expr(str))


// convert string to list of expressions of pseudo's arg part.
// string -> expr list option
let toExprList (str:string) =
    let exprStrs = Array.toList( str.Split([|','|]) )
    if exprStrs.Length > 0 then
        Some( List.map Expr exprStrs )
    else
        None


// convert string to name and expression of pseudo's arg part.
// string -> (string * expr) option
let toNameExpr str =
    maybe {
        let! (name, arg2) = getTwoArgs str
        let! expr = toExpr arg2
        return (name, expr)
    }


// adapt arguments to function.
// 'a option -> ('a -> 'b) -> 'b option
let withArgs args code = adaptArgs code args


// convert pseudo string to intermediate pseudo expression.
// string -> string -> pseudoOp option
let getPseudo pseudoStr argStr=
    match pseudoStr with
    | "byte"   -> Byte     |> withArgs (toExprList argStr)
    | "even"   -> Even     |> Some
    | "if"     -> If       |> withArgs (toExpr argStr)
    | "endif"  -> Endif    |> Some
    | "globl"  -> Global   |> withArgs (Some(argStr))
    | "text"   -> SectText |> Some
    | "data"   -> SectData |> Some
    | "bss"    -> SectBss  |> Some
    | "comm"   -> Comm     |> withArgs (toNameExpr argStr)
    | _ -> None
