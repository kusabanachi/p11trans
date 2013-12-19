module p11trans.ackPseudo

(* translate intermediate expressions
   to ACK i8086 pseudo-operation assembly code text *)

open p11trans.intermediate


// get a byte sized numbers text.
// expr list -> string
let getBytes valExprs =
    let bytesList = List.map (fun (Expr(expr)) -> expr) valExprs
    String.concat ", " bytesList


// get ACK i8086 pseudo-operation text.
// pseudoOp -> string
let getPseudoText = function
    | Byte(valExprs) ->
        ".data1 " + getBytes valExprs
    | Even ->
        ".align 2"
    | If(expr) ->
        ""
    | Endif ->
        ""
    | Global(names) ->
        ".extern " + names
    | SectText ->
        ".sect .text"
    | SectData ->
        ".sect .data"
    | SectBss ->
        ".sect .bss"
    | Comm(str, expr) ->
        let eStr = expr |> function | Expr(e) -> e
        ".comm " + str + ", " + eStr

