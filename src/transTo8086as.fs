module p11trans.transTo8086as

(* translate intermediate expressions
   to i8086 assembly code text *)

open p11trans.utility
open p11trans.intermediate
open p11trans.ackExpression
open p11trans.ackTempMemText


// concatenate statements string and comment string
// string -> string option -> string
let concatComment stStr comment =
    match (stStr, comment) with
    | (statement, None) ->
        statement
    | ("", Some(commStr)) ->
        sprintf "!%s" commStr
    | (statement, Some(commStr)) ->
        sprintf "%-28s !%s" statement commStr


// get i8086 assign text.
// symbol -> expr -> string
let getAssignText (Symbol(symStr)) expr =
    let exprStr = getExpressionStr expr
    if symStr <> "." then
        symStr + " = " + exprStr
    else
        let locCountPtn1 = "^\.\s*\+\s*(.+?)$" // . + xxxx
        let locCountPtn2 = "^(.+?)\s*\+\s*\.$" // xxxx + .
        match exprStr with
        | RegexMatch locCountPtn1 rMatch | RegexMatch locCountPtn2 rMatch ->
            let offset = rMatch.Groups.[1].Value
            ".space " + offset
        | _ ->
            failwithf "assigning location counter with absolute value"


// get i8086 string text.
// string -> string
let getStringText str = ".ascii \"" + str + "\""


// get i8086 expression text.
// expr -> string
let getExpressionText expr = ".data2 " + getExpressionStr expr


// get i8086 statement text.
// statementElement option -> string
let getStatementText = function
    | Some( Assign(sym, expr) ) ->
        getAssignText sym expr
    | Some( String(str) ) ->
        getStringText str
    | Some( Instruction(code) ) ->
        ackInstruction.getInstructionText code
    | Some( Pseudo(pseudoOp) ) ->
        ackPseudo.getPseudoText pseudoOp
    | Some( Expression(expr) ) ->
        getExpressionText expr
    | None -> ""


// translate a string of statement part to i8086 string.
// index is order of statement in a text line. it have an effect on indent.
// int -> string * statementElement option -> string
let transStatement index ((label:string), statement) =
    let statementText = getStatementText statement
    let labelText = label
    let isPseudoTextWithoutLabel =
        statementText.StartsWith(".") &&
        not (statementText.StartsWith(".data")) &&
        isBlankString label

    if isPseudoTextWithoutLabel then
        statementText
    elif not (isBlankString statementText) then
        if index = 0 then
            sprintf "%-8s %s" label statementText
        else
            sprintf "%s%s" label statementText 
    else
        label


// translate a line of intermediate expression to i8086 assembly code.
// (string * statementElement option) list * string option -> string
let transOneLine (exComment, comment) =
    let stList = List.mapi transStatement exComment
    let stTexts = String.concat ";  " stList
    concatComment stTexts comment


// translate some lines of intermediate expression to i8086 assembly code.
// ((string * statementElement option) list * string option) list -> string list
let transTo8086Asm intermediate =
    let outTextLines = List.map transOneLine intermediate
    resolveTempMemData outTextLines intermediate


