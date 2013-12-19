module p11trans.ackExpression

(* translate intermediate expressions
   to ACK i8086 expression assembly code text *)

open p11trans.intermediate
open p11trans.utility


// convert octal number string to hex number string
// string -> string 
let octStrToHexStr octStr =
    let octNum = int octStr
    if octNum > 9 then
        let rec octToDec = function
            | 0 -> 0
            | x -> x % 10 + octToDec(x / 10) * 8
        sprintf "0x%x" (octToDec octNum)
    else
        octStr


// find octal number and convert it.
// string -> string
let rec transOctalNumber str =
    let (checkedStr, leftStr) =
        match str with
        | RegexMatch "^([0-9]+)\.(.*?)$" rMatch ->
            (rMatch.Groups.[1].Value, rMatch.Groups.[2].Value)
        | RegexMatch "^([0-9]+)$" rMatch
        | RegexMatch "^([0-9]+)([\s\+\-\*&\|%!\^].*?)$" rMatch ->
            (octStrToHexStr rMatch.Groups.[1].Value,
             rMatch.Groups.[2].Value)
        | RegexMatch "^([^0-9]+)(.*?)$" rMatch ->
            (rMatch.Groups.[1].Value, rMatch.Groups.[2].Value)
        | _ ->
            (str, "")

    if isBlankString leftStr then
        checkedStr
    else
        checkedStr + (transOctalNumber leftStr)


// if is there relocation counter, convert to zero character.
// string -> string
let removeRelocationCounter (str:string) = 
    str.Replace("..", "0")


// get ACK i8086 expression text.
// expr -> string
let getExpressionStr = function
    | Expr(str) ->
        str
        |> transOctalNumber
        |> removeRelocationCounter


