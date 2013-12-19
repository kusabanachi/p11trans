module p11trans.readPdp11as

(* read pdp11's assembly code text
   and convert to intermediate expressions *)

open p11trans.utility
open p11trans.intermediate


// clip comment from a text line. get comment and rest of text.
// string -> string option * string
let clipComment str =
    // todo treat escape sequence
    let pattern = "^(.*?)/(.*?)\s*$"    // xxx / xxx

    match str with
    | RegexMatch pattern rMatch ->
        let commStr = rMatch.Groups.[2].Value
        let rest = rMatch.Groups.[1].Value
        (Some(commStr), rest)
    | _ ->
        (None, str) 


// clip labels from statement string. get labels and rest of text.
// string -> string * string
let clipLabels str =
    let pattern = "^\s*(.+:)\s*(.*?)\s*$" // xxx: xxx

    match str with
    | RegexMatch pattern rMatch ->
        let labels = rMatch.Groups.[1].Value
        let exLabel = rMatch.Groups.[2].Value
        (labels, exLabel)
    | _ ->
        ("", str)


// read assignment statement. if it's matched, return Some(Assign(xx)).
// string -> statementElement option
let readAssignment str =
    let pattern = "^\s*(\S+)\s*=\s*(\S+)\s*$" // xxx = xxx

    match str with
    | RegexMatch pattern rMatch ->
        let lhs = rMatch.Groups.[1].Value
        let rhs = rMatch.Groups.[2].Value
        let rest = rMatch.Groups.[3].Value.Trim()
        Some( Assign(Symbol(lhs), Expr(rhs)) )
    | _ ->
        None


// read string statement. if it's matched, return Some(String(xx)).
// string -> statementElement option
let readString str =
    let pattern = "^\s*<(.*[^\\\])>\s*$" // <xxx>

    match str with
    | RegexMatch pattern rMatch ->
        let contents = rMatch.Groups.[1].Value
        Some( String(contents) )
    | _ ->
        None


// read instruction statement. if it's matched, return Some(Instruction(xx)).
// string -> statementElement option
let readInstruction str =
    let pattern = "^\s*([a-z]+)\s*(.*?)\s*$" // xxx xxxxxx

    match str with
    | RegexMatch pattern rMatch ->
        let code = rMatch.Groups.[1].Value
        let operands = rMatch.Groups.[2].Value
        match pdp11Instruction.getInstruction code operands with
        | Some instruction -> Some( Instruction(instruction) )
        | _ -> None
    | _ -> None


// read Pseudo-operation. if it's matched, return Some(Pseudo(xx)).
// string -> statementElement option
let readPseudo str =
    let pattern = "^\s*\.(\S*)\s*(.*?)\s*$" // .xxx xxx

    match str with
    | RegexMatch pattern rMatch ->
        let code = rMatch.Groups.[1].Value
        let args = rMatch.Groups.[2].Value
        match pdp11Pseudo.getPseudo code args with
        | Some pseudo -> Some( Pseudo(pseudo) )
        | _ -> None
    | _ -> None


// read expression. if it's matched, return Some(Expression(xx)).
// string -> statementElement option
let readExpression str =
    let pattern = "^\s*(\S+)\s*$" // xxx

    match str with
    | RegexMatch pattern rMatch ->
        let exprStr = rMatch.Groups.[1].Value
        Some( Expression(Expr(exprStr)) )
    | _ -> None


// read one statement.
// string -> string * statementElement option
let readStatement src =
    let ordo taskA taskB = if taskA <> None then taskA else taskB
    let (labels, exLabel) = clipLabels src
    let statement = 
        readAssignment exLabel
        |> ordo <| readString exLabel
        |> ordo <| readInstruction exLabel
        |> ordo <| readPseudo exLabel
        |> ordo <| readExpression exLabel
    (labels, statement)


// split a line into some statements.
// string -> string * string list
let splitIntoStatements (lineStr:string) =
    let statements = lineStr.Split([|';'|])
    Array.toList(statements)


// read a line of pdp11 assembly code.
// output is "(label * 'Some statementElement') list * 'Some comment'"
// string -> (string * statementElement option) list * string option
let readOneLine lineStr =
    let (commentStr, exCommentStr) = clipComment lineStr
    let statementStrs = splitIntoStatements exCommentStr
    let statements = List.map readStatement statementStrs
    (statements, commentStr)


// read some lines of pdp11 assembly code text
// string list -> ((string * statementElement option) list * string option) list
let readPdp11as asm =
    List.map readOneLine asm


