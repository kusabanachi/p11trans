namespace test

open NUnit.Framework
open ExpressionType
open V6as.Addres
open V6as.Opline
open V6as.StatementType


[<TestFixture>]
type OplineTest() =

    static member OplineTestData =
        [|
            [| ( "mov 3, r2", (DoubleOp("mov", Rel(Expr_Oct 3s), Reg R2), "") ) |]
            [| ( "ror 6(r3)", (SingleOp("ror", IdxDfr(R3, Expr_Oct 6s)), "") ) |]
            [| ( ".byte 7, 9, hello;", (Byte [Expr_Oct 7s; Expr_Oct 9s; Expr_Sym "hello"], ";") ) |]
            [| ( ".even 7, 9, hello;", (Even, " 7, 9, hello;") ) |]
            [| ( ".if debug", (If (Expr_Sym "debug"), "") ) |]
            [| ( ".endif debug", (EndIf, " debug") ) |]
            [| ( ".globl putchar, getchar, allocate", (Global ["putchar"; "getchar"; "allocate"], "") ) |]
            [| ( ".text hoge", (Text, " hoge") ) |]
            [| ( ".data", (Data, "") ) |]
            [| ( ".bss", (Bss, "") ) |]
            [| ( "sob r0,2b", (DoubleOp("sob", Reg R0, Rel(Expr_Lbl "2b")), "") ) |]
            [| ( ".comm _errno,2", (Common("_errno", Expr_Oct 2s), "") ) |]
            [| ( "sys exec; 2f; 1f", (ExprOp("sys", Expr_Sym "exec"), "; 2f; 1f") ) |]
            [| ( "sys exec; 2f; 1f", (ExprOp("sys", Expr_Sym "exec"), "; 2f; 1f") ) |]
            [| ( "<tstf\0\0\0\0>;", (Str "tstf\x00\x00\x00\x00", ";") ) |]
            [| ( "02020, /* [ */", (Expr (Expr_Oct 0o2020s), ", /* [ */") ) |]
            [| ( "..;512.", (Expr (Expr_Sym ".."), ";512.") ) |]
        |]

    [<TestCaseSource("OplineTestData")>]
    member x.ValidOpline data =
        let src, expected = data
        let actual = opline src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "opline %A -> %A" src actual)


    static member OplineErrorTestData =
        [|
            [| "mov 3 r2" |]
            [| "sob r0 2b" |]
            [| ".comm 1, 2" |]
            [| ".comm _errno 2" |]
        |]

    [<TestCaseSource("OplineErrorTestData")>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.invalidOpline src =
        opline src |> ignore

