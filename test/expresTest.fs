namespace test

open System
open NUnit.Framework
open ReadOp
open Symtab
open Expres


[<TestFixture>]
type ExpresTest() =

    static member ValidExpressTestData =
        [|
            [| ( "3(r0)", (Expr_Oct "3", TypeAbsolute, "(r0)") ) |]
            [| ( "3+3.;", (Expr_Op ('+' ,Expr_Oct "3", Expr_Dec "3"), TypeAbsolute, ";") ) |]
            [| ( "1 * 91. - 2f",
                 (Expr_Op ('-', Expr_Op ('*', Expr_Oct "1", Expr_Dec "91"), Expr_Lbl "2f"), TypeText, "") ) |]
            [| ( "1f\/1", (Expr_Op ('/' , Expr_Lbl "1f", Expr_Oct "1"), TypeText, "") ) |]
            [| ( "1&~zzz", (Expr_Op ('&', Expr_Oct "1", Expr_Sym "zzz"), TypeAbsolute, "") ) |]
            [| ( "1|1", (Expr_Op ('|', Expr_Oct "1", Expr_Oct "1"), TypeAbsolute, "") ) |]
            [| ( "1\>1", (Expr_Op ('>', Expr_Oct "1", Expr_Oct "1"), TypeAbsolute, "") ) |]
            [| ( "1\<1", (Expr_Op ('<', Expr_Oct "1", Expr_Oct "1"), TypeAbsolute, "") ) |]
            [| ( "1%1", (Expr_Op ('%', Expr_Oct "1", Expr_Oct "1"), TypeAbsolute, "") ) |]
            [| ( "1\%1", (Expr_Op ('|', Expr_Oct "1", Expr_Oct "1"), TypeAbsolute, "") ) |]
            [| ( "1!1", (Expr_Op ('!', Expr_Oct "1", Expr_Oct "1"), TypeAbsolute, "") ) |]
            [| ( "1^1", (Expr_Op ('^', Expr_Oct "1", Expr_Oct "1"), TypeAbsolute, "") ) |]
            [| ( "[0] \n", (Expr_Group (Expr_Oct "0"), TypeAbsolute, " \n") ) |]
            [| ( "1+[2*2]",
                 (Expr_Op ('+', Expr_Oct "1", Expr_Group (Expr_Op ('*', Expr_Oct "2", Expr_Oct "2"))), TypeAbsolute, "") ) |]
            [| ( "5 6 5", (Expr_Op ('+', Expr_Op ('+', Expr_Oct "5", Expr_Oct "6"), Expr_Oct "5"), TypeAbsolute, "") ) |]
            [| ( "[1 1]", (Expr_Group (Expr_Op ('+', Expr_Oct "1", Expr_Oct "1")), TypeAbsolute, "") ) |]
            [| ( "[2][3*2]",
                 (Expr_Op ('+', Expr_Group (Expr_Oct "2"), Expr_Group (Expr_Op ('*', Expr_Oct "3", Expr_Oct "2"))), TypeAbsolute, "") ) |]
            [| ( "3[2*[3 2]]",
                 (Expr_Op ('+', Expr_Oct "3", Expr_Group (Expr_Op ('*', Expr_Oct "2", Expr_Group (Expr_Op ('+', Expr_Oct "3", Expr_Oct "2"))))),
                  TypeAbsolute,
                  "") ) |]
            [| ( "% 5", (Expr_Op ('%', Expr_Dec "0", Expr_Oct "5"), TypeAbsolute, "") ) |]
            [| ( "+ 9.", (Expr_Op ('+', Expr_Dec "0", Expr_Dec "9"), TypeAbsolute, "") ) |]
            [| ( "3+++++5", (Expr_Op ('+', Expr_Oct "3", Expr_Oct "5"), TypeAbsolute, "") ) |]
            [| ( "1+*3", (Expr_Op ('*', Expr_Oct "1", Expr_Oct "3"), TypeAbsolute, "") ) |]
            [| ( "4 5-", (Expr_Op ('+', Expr_Oct "4", Expr_Oct "5"), TypeAbsolute, "") ) |]
        |]

    [<TestCaseSource("ValidExpressTestData")>]
    member x.ValidExpression data =
        let src, expected = data
        let actual = expres src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "expres %A -> %A" src actual)

    static member ExpresErrorTestData =
        [|
            [| "3**3" |]
            [| "3%+3" |]
            [| "" |]
            [| "*" |]
            [| "[]" |]
            [| "3+2 [9-1" |]
        |]

    [<TestCaseSource("ExpresErrorTestData")>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.invalidExpression src =
        expres src |> ignore

