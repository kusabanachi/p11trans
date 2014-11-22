namespace test

open NUnit.Framework
open ReadOp
open Expres
open Addres
open Assem
open StatementType


[<TestFixture>]
type AssemTest() =

    static member AssignmentTestData =
        [|
            [| ( "signal = 48.\n", [Assignment (Token_Symbol "signal", Expr_Dec 48s); Eos '\n'] ) |]
            [| ( ".=.+2;", [Assignment (Token_Symbol ".", Expr_Op ('+', Expr_Sym ".", Expr_Oct 2s)); Eos ';'] ) |]
        |]

    [<TestCaseSource("AssignmentTestData")>]
    member x.AssignmentTest data =
        let src, expected = data
        let actual = assem src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "assem %A -> %A" src actual)


    static member LabelTestData =
        [|
            [| ( "pof: .=.+1", [Label (Token_Symbol "pof"); Assignment (Token_Symbol ".", Expr_Op ('+', Expr_Sym ".", Expr_Oct 1s)); Eos EOT] ) |]
            [| ( "9: sys seek", [Label (Token_Octal 9s); ExprOp ("sys", Expr_Sym "seek"); Eos EOT] ) |]
            [| ( "2.:", [Label (Token_Decimal 2s); Eos EOT] ) |]
            [| ( "11:", [Label (Token_Octal 9s); Eos EOT] ) |]
            [| ( "\'\\a:", [Label (Token_SChar ('\006', 6s)); Eos EOT] ) |]
            [| ( "\"\\0\\0:", [Label (Token_DChar ('\000', '\000', 0s)); Eos EOT] ) |]
            [| ( "hoge: 2.: fuga:", [Label (Token_Symbol "hoge"); Label (Token_Decimal 2s); Label (Token_Symbol "fuga"); Eos EOT] ) |]
        |]

    [<TestCaseSource("LabelTestData")>]
    member x.LabelTest data =
        let src, expected = data
        let actual = assem src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "assem %A -> %A" src actual)


    static member OplineTestData =
        [|
            [| ( "mov 3, r2", [DoubleOp ("mov", Rel (Expr_Oct 3s), Reg R2); Eos EOT] ) |]
            [| ( ".text", [Text; Eos EOT] ) |]
            [| ( "<tstf\0\0\0\0>;", [Str "tstf\x00\x00\x00\x00"; Eos ';'] ) |]
            [| ( "02020", [Expr (Expr_Oct 0o2020s); Eos EOT] ) |]
        |]

    [<TestCaseSource("OplineTestData")>]
    member x.OplineStatementTest data =
        let src, expected = data
        let actual = assem src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "assem %A -> %A" src actual)


    static member EosTestData =
        [|
            [| ( "\x04 mov 3, r2", [Eos EOT] ) |]
            [| ( "\n mov 3, r2", [Eos '\n'; DoubleOp ("mov", Rel (Expr_Oct 3s), Reg R2); Eos EOT] ) |]
            [| ( "; mov 3, r2", [Eos ';'; DoubleOp ("mov", Rel (Expr_Oct 3s), Reg R2); Eos EOT] ) |]
        |]

    [<TestCaseSource("EosTestData")>]
    member x.EosTest data =
        let src, expected = data
        let actual = assem src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "assem %A -> %A" src actual)


    static member CommentTestData =
        [|
            [| ( "/ This is a great comment.", [Comment " This is a great comment."; Eos EOT] ) |]
            [| ( "/ hoge\n", [Comment " hoge"; Eos '\n'] ) |]
        |]

    [<TestCaseSource("CommentTestData")>]
    member x.CommentTest data =
        let src, expected = data
        let actual = assem src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "assem %A -> %A" src actual)


    static member AssemErrorTestData =
        [|
            [| ".=2" |]
            [| "<string>=1" |]
            [| "signal:" |]
            [| "12:" |]
            [| "<string>:" |]
        |]

    [<TestCaseSource("AssemErrorTestData")>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.invalidAssem src =
        assem src |> ignore

