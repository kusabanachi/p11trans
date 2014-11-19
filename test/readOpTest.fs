namespace test

open System
open NUnit.Framework
open ReadOp


[<TestFixture>]
type ReadOpTest() =

    static member GarbageTestData =
        [|
            [| "\x0C_" |]
            [| "# 55" |]
            [| "\babc" |]
            [| ">qq" |]
            [| "? " |]
            [| "@ t" |]
            [| "` e" |]
            [| "{ s" |]
            [| "} t" |]
            [| "\x7Ft" |]
        |]

    [<TestCaseSource("GarbageTestData")>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.ReadGarbage src =
        readOp src |> ignore


    static member FixorTestData =
        [|
            [| ( "|hoge", (Token_Fixor, "hoge") ) |]
            [| ( "| 255.", (Token_Fixor, " 255.") ) |]
            [| ( "| a\r\n", (Token_Fixor, " a\r\n") ) |]
        |]

    [<TestCaseSource("FixorTestData")>]
    member x.ReadFixor data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member EscapeTestData =
        [|
            [| ( "\\/ H", (Token_Escp '/', " H") ) |]
            [| ( "\\< Oc", (Token_Escp '<', " Oc") ) |]
            [| ( "\\> G", (Token_Escp '>', " G") ) |]
            [| ( "\\% EE", (Token_Escp '%', " EE") ) |]
            [| ( "\\ABC", (Token_Escp '\\', "BC") ) |]
        |]

    [<TestCaseSource("EscapeTestData")>]
    member x.ReadEscapedChar data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member SkipTestData =
        [|
            [| ( "\t\<O", (Token_Escp '<', "O") ) |]
            [| ( "\r", (Token_Term EOT, "") ) |]
            [| ( " \t\r\r\t |\r", (Token_Fixor, "\r") ) |]
        |]

    [<TestCaseSource("SkipTestData")>]
    member x.ReadSkipChar data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member MetaCharTestData =
        [|
            [| ( "!", (Token_Meta '!', "") ) |]
            [| ( "$a", (Token_Meta '$', "a") ) |]
            [| ( "%&", (Token_Meta '%', "&") ) |]
            [| ( "&\r", (Token_Meta '&', "\r") ) |]
            [| ( "(  ", (Token_Meta '(', "  ") ) |]
            [| ( ")|||", (Token_Meta ')', "|||") ) |]
            [| ( "**", (Token_Meta '*', "*") ) |]
            [| ( "+*", (Token_Meta '+', "*") ) |]
            [| ( ",,,", (Token_Meta ',', ",,") ) |]
            [| ( "-^", (Token_Meta '-', "^") ) |]
            [| ( ":?@", (Token_Meta ':', "?@") ) |]
            [| ( "=\n", (Token_Meta '=', "\n") ) |]
            [| ( "[;", (Token_Meta '[', ";") ) |]
            [| ( "][][]", (Token_Meta ']', "[][]") ) |]
            [| ( "^777", (Token_Meta '^', "777") ) |]
        |]

    [<TestCaseSource("MetaCharTestData")>]
    member x.ReadMetaChar data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member TermTestData =
        [|
            [| ( "\n\n\n", (Token_Term '\n', "\n\n") ) |]
            [| ( ";mov ", (Token_Term ';', "mov ") ) |]
            [| ( "\x04k", (Token_Term EOT, "k") ) |]
            [| ( "", (Token_Term EOT, "") ) |]
        |]

    [<TestCaseSource("TermTestData")>]
    member x.ReadTerminationChar data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member dqNum a b = (int16 a <<< 8) + int16 b

    static member DQuoteTestData =
        [|
            [| ( "\"okok ", (Token_DChar ('o', 'k', ReadOpTest.dqNum 'o' 'k'), "ok ") ) |]
            [| ( "\"\r\t\r\t", (Token_DChar ('\r', '\t', ReadOpTest.dqNum '\r' '\t'), "\r\t") ) |]
        |]

    [<TestCaseSource("DQuoteTestData")>]
    member x.ReadDoubleCharConstant data =
        let src, expected = data
        Assert.True(readOp src = expected)

    [<Test>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.ReadDoubleCharConstantAndThrowException() =
        let src = "\"\r\n"
        readOp src |> ignore


    static member SQuoteTestData =
        [|
            [| ( "\'okok ", (Token_SChar ('o', int16 'o'), "kok ") ) |]
            [| ( "\'\r\t\r\t", (Token_SChar ('\r', int16 '\r'), "\t\r\t") ) |]
        |]

    [<TestCaseSource("SQuoteTestData")>]
    member x.ReadSingleCharConstant data =
        let src, expected = data
        Assert.True(readOp src = expected)

    [<Test>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.ReadSingleCharConstantAndThrowException() =
        let src = "\'\n"
        readOp src |> ignore


    static member SlashTestData =
        [|
            [| ( "/ This is a comment", (Token_Comment " This is a comment", "") ) |]
            [| ( "/", (Token_Comment "", "") ) |]
        |]

    [<TestCaseSource("SlashTestData")>]
    member x.ReadComment data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member LtTestData =
        [|
            [| ( "<thisisastring>and", (Token_String "thisisastring", "and") ) |]
            [| ( "<>oooo", (Token_String "", "oooo") ) |]
            [| ( "<\\n\\t\\e\\0\\r\\a\\p\\\\>", (Token_String "\x0A\x09\x04\x00\x0d\x06\x1b\\", "") ) |]
        |]

    [<TestCaseSource("LtTestData")>]
    member x.ReadString data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member NumberTestData =
        [|
            [| ( "198*40", (Token_Octal 144s, "*40") ) |]
            [| ( "22.(", (Token_Decimal 22s, "(") ) |]
            [| ( "700001", (Token_Octal 0x8001s, "") ) |]
            [| ( "65536.", (Token_Decimal 0s, "") ) |]
            [| ( "8b; 6b", (Token_LocalLabel "8b", "; 6b") ) |]
            [| ( "0f+2", (Token_LocalLabel "0f", "+2") ) |]
            [| ( "003b", (Token_LocalLabel "3b", "") ) |]
        |]

    [<TestCaseSource("NumberTestData")>]
    member x.ReadNumber data =
        let src, expected = data
        Assert.True(readOp src = expected)


    static member LocalLabelErrorTestData =
        [|
            [| "14b" |]
            [| "22f; /" |]
        |]

    [<TestCaseSource("LocalLabelErrorTestData")>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.ReadNumberAndThrowException src =
        readOp src |> ignore


    static member SymbolTestData =
        [|
            [| ( "konichiwa!", (Token_Symbol "konichiw", "!") ) |]
            [| ( "~m~m~m#m", (Token_Symbol "m~m~m", "#m") ) |]
            [| ( "...2.", (Token_Symbol "...2.", "") ) |]
            [| ( "ja_JP.UTF-8", (Token_Symbol "ja_JP.UT", "-8") ) |]
        |]

    [<TestCaseSource("SymbolTestData")>]
    member x.ReadSymbol data =
        let src, expected = data
        Assert.True(readOp src = expected)



