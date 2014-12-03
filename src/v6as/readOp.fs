namespace V6as

open System

module ReadOp =

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
        | Token_DChar of char * char * int16
        | Token_SChar of char * int16
        | Token_Comment of string
        | Token_String of string
        | Token_LocalLabel of string
        | Token_Octal of int16
        | Token_Decimal of int16
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
            let snd, rest' = readFirstCharOfString rest
            let num = (int16 fst <<< 8) + int16 snd
            Some( Token_DChar (fst, snd, num), rest')
        | _ -> None

    let (|SQuote|_|) (src:string) =
        match src.[0] with
        | '\'' ->
            let c, rest = readFirstCharOfString src.[1..]
            let num = int16 c
            Some( Token_SChar (c, num), rest)
        | _ -> None

    let (|Slash|_|) (src:string) =
        match src.[0] with
        | '/' ->
            let text = src.[1..]
            let len =
                match text.IndexOfAny([| EOT; '\n' |]) with
                | -1 -> text.Length
                | i  -> i
            Some( Token_Comment text.[..len - 1], text.[len..] )
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
        let octNum = Seq.fold (fun acc c -> acc * 8s + int16 c - int16 '0') 0s
        let decNum = Seq.fold (fun acc c -> acc * 10s + int16 c - int16 '0') 0s

        if Char.IsDigit src.[0] then
            let rec digestDigits acc src =
                if String.IsNullOrEmpty src || not (Char.IsDigit src.[0]) then
                    acc, src
                else
                    digestDigits (acc + src.[0..0]) src.[1..]
            let numStr, rest = digestDigits "" src
            match rest with
            | "" ->
                let num = octNum numStr
                Some( Token_Octal num, rest)
            | _ when rest.[0] = 'f' || rest.[0] = 'b' ->
                let num = Int32.Parse numStr
                if 0 <= num && num <= 9 then
                    let labelStr = string num + rest.[0..0]
                    Some( Token_LocalLabel labelStr, rest.[1..])
                else
                    failwith LocalLabelError
            | _ when rest.[0] = '.' ->
                let num = decNum numStr
                Some( Token_Decimal num, rest.[1..])
            | _ ->
                let num = octNum numStr
                Some( Token_Octal num, rest )
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

