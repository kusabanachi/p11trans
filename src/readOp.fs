
module ReadOp

open System

[<Literal>]
let EOT = '\x04'

let StringTermError = "> string not terminated properly"
let GarbageCharError = "G garbage (unknown) character"
let LocalLabelError = "F error in local (\"f\" or \"b\") type symbol"


type Token =
    | Token_Garb
    | Token_Fixor
    | Token_Escp of char
    | Token_Meta of char
    | Token_Term of char
    | Token_DChar of char * char
    | Token_SChar of char
    | Token_Comment of string
    | Token_String of string
    | Token_LocalLabel of string
    | Token_Octal of string
    | Token_Decimal of string
    | Token_Symbol of string

let readFirstCharOfString src =
    if String.IsNullOrEmpty src then
        failwith StringTermError
    let c, rest = src.[0], src.[1..]
    if  c = EOT || c = '\n' then
        failwith StringTermError
    elif c = '\\' then
        if String.IsNullOrEmpty rest then
            failwith StringTermError
        else
            let c, rest = rest.[0], rest.[1..]
            (match c with
             | 'n' -> '\n'
             | 't' -> '\t'
             | 'e' -> EOT
             | '0' -> '\000'
             | 'r' -> '\r'
             | 'a' -> '\x06'
             | 'p' -> '\x1b'
             | _   -> c
             , rest)
    else
        c, rest


let (|Garb|_|) (src:string) =
    let head = src.[0]
    if '\x00' <= head && head <= '\x03' ||
       '\x05' <= head && head <= '\x08' ||
       '\x0b' <= head && head <= '\x0c' ||
       '\x0e' <= head && head <= '\x1f' ||
       head = '#' || head = '>' || head = '?' ||
       head = '@' || head = '`' || head = '{' ||
       head = '}' || head = '\x7f' then
        failwith GarbageCharError
        Some( Token_Garb, src )
    else None

let (|Fixor|_|) (src:string) =
    match src.[0] with
    | '|' -> Some( Token_Fixor, src.[1..] )
    | _ -> None

let (|Escp|_|) (src:string) =
    match src.[0] with
    | '\\' ->
        if src.Length >= 2 then
            match src.[1] with
            | '/' | '<' | '>' | '%' ->
                Some( Token_Escp src.[1], src.[2..] )
            | _ ->
                Some( Token_Escp '\\', src.[2..])
        else
            Some( Token_Escp '\\', "" )
    | _ -> None

let (|Skip|_|) (src:string) =
    match src.[0] with
    | '\t' | '\r' | ' ' -> Some( src.[1..] )
    | _ -> None

let (|Meta|_|) (src:string) =
    match src.[0] with
    | '!' | '$' | '%' | '&' | '('
    | ')' | '*' | '+' | ',' | '-'
    | ':' | '=' | '[' | ']' | '^' -> Some( Token_Meta src.[0], src.[1..] )
    | _ -> None

let (|Term|_|) (src:string) =
    match src.[0] with
    | EOT | '\n' | ';' -> Some( Token_Term src.[0], src.[1..] )
    | _ -> None

let (|DQuote|_|) (src:string) =
    match src.[0] with
    | '\"' ->
        let fst, rest = readFirstCharOfString src.[1..]
        let snd, rest = readFirstCharOfString rest
        Some( Token_DChar (fst, snd), rest)
    | _ -> None

let (|SQuote|_|) (src:string) =
    match src.[0] with
    | '\'' ->
        let c, rest = readFirstCharOfString src.[1..]
        Some( Token_SChar c, rest)
    | _ -> None

let (|Slash|_|) (src:string) =
    match src.[0] with
    | '/' -> Some( Token_Comment src.[1..], "" )
    | _ -> None

let (|Lt|_|) (src:string) =
    match src.[0] with
    | '<' ->
        let rec strAndRest str src =
            if String.IsNullOrEmpty src then
                failwith StringTermError
            else
                let c, rest = readFirstCharOfString src
                if c = '>' then
                    str, rest
                else
                    strAndRest (str + string c) rest
        let str, rest = strAndRest "" src.[1..]
        Some( Token_String str, rest )
    | _ -> None

let (|Number|_|) (src:string) =
    if Char.IsDigit src.[0] then
        let rec numAndRest numAcc src =
            if String.IsNullOrEmpty src || not (Char.IsDigit src.[0]) then
                numAcc, src
            else
                numAndRest (numAcc + src.[0..0]) src.[1..]
        let numStr, rest = numAndRest "" src
        match rest with
        | "" ->
            Some( Token_Octal numStr, rest)
        | _ when rest.[0] = 'f' || rest.[0] = 'b' ->
            let value = Int32.Parse numStr
            if 0 <= value && value <= 9 then
                let labelStr = string value + rest.[0..0]
                Some( Token_LocalLabel labelStr, rest.[1..])
            else
                failwith LocalLabelError
        | _ when rest.[0] = '.' ->
            Some( Token_Decimal numStr, rest.[1..])
        | _ ->
            Some( Token_Octal numStr, rest )
    else
        None

let (|Ascii|_|) src =
    let isCharOfSymbol c =
        'a' <= c && c <= 'z' ||
        'A' <= c && c <= 'Z' ||
        c = '.' || c = '_' || c = '~' ||
        '0' <= c && c <= '9'
    let rec symbolAndRest symAcc rest =
        if String.IsNullOrEmpty rest || not (isCharOfSymbol rest.[0]) then
            symAcc, rest
        else
            symbolAndRest (symAcc + rest.[0..0]) rest.[1..]
    let symName, rest = symbolAndRest "" src
    let symName = symName |> function
        | s when s.[0] = '~' -> s.[1..]
        | _ -> symName
    let symName = symName |> function
        | s when s.Length > 8 -> s.[..7]
        | _ -> symName
    Some( Token_Symbol symName, rest )

let rec readOp = function
    | ""        -> ( Token_Term EOT, "" )
    | Skip rest -> readOp rest
    | Garb   op -> op
    | Fixor  op -> op
    | Escp   op -> op
    | Meta   op -> op
    | Term   op -> op
    | DQuote op -> op
    | SQuote op -> op
    | Slash  op -> op
    | Lt     op -> op
    | Number op -> op
    | Ascii  op -> op
    | x -> failwith <| "Unknown Token: " + x

