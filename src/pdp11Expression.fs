module p11trans.pdp11Expr

(* read pdp11's expression assembly codes *)

open p11trans.utility
open p11trans.intermediate


let notEscaped (str:string) index =
    if index = 0 then
        true
    elif str.[index-1] <> '\\' then
        true
    else
        false


let charConstant


// convert expression string to intermediate code.
// string -> expr option
let getExpression (src:string) =
    let quote = [| '\''; '\"' |]
    let index = src.IndexOfAny(quote, 0)
    if index >= 0 && notEscaped src index then
        dPrint index
    dPrint "aaa"
    Some(Expr(src))

