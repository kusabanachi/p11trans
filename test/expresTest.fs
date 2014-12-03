namespace test

open System
open NUnit.Framework
open V6as.ReadOp
open V6as.Symtab
open V6as.Expres


[<TestFixture>]
type ExpresTest() =

    static member ValidExpressTestData =
        [|
            [| ( "3(r0)", (Expr_Oct 3s, (TypeAbsolute, 3s), "(r0)") ) |]
            [| ( "3+3.;", (Expr_Op ('+', Expr_Oct 3s, Expr_Dec 3s), (TypeAbsolute, 6s), ";") ) |]
            [| ( "'a - \"bc",
                 (Expr_Op ('-', Expr_SChar 'a', Expr_DChar ('b', 'c')),
                  (TypeAbsolute, int16 'a' - (int16 'b' * 256s + int16 'c')), "") ) |]
            [| ( "1 * 91. - 2f",
                 (Expr_Op ('-', Expr_Op ('*', Expr_Oct 1s, Expr_Dec 91s), Expr_Lbl "2f"), (TypeText, 91s), "") ) |]
            [| ( "1f\/1", (Expr_Op ('/' , Expr_Lbl "1f", Expr_Oct 1s), (TypeText, 0s), "") ) |]
            [| ( "1&~zzz", (Expr_Op ('&', Expr_Oct 1s, Expr_Sym "zzz"), (TypeAbsolute, 0s), "") ) |]
            [| ( "1|1", (Expr_Op ('|', Expr_Oct 1s, Expr_Oct 1s), (TypeAbsolute, 1s), "") ) |]
            [| ( "1\>1", (Expr_Op ('>', Expr_Oct 1s, Expr_Oct 1s), (TypeAbsolute, 0s), "") ) |]
            [| ( "1\<1", (Expr_Op ('<', Expr_Oct 1s, Expr_Oct 1s), (TypeAbsolute, 2s), "") ) |]
            [| ( "1%1", (Expr_Op ('%', Expr_Oct 1s, Expr_Oct 1s), (TypeAbsolute, 0s), "") ) |]
            [| ( "1\%1", (Expr_Op ('|', Expr_Oct 1s, Expr_Oct 1s), (TypeAbsolute, 1s), "") ) |]
            [| ( "1!0", (Expr_Op ('!', Expr_Oct 1s, Expr_Oct 0s), (TypeAbsolute, 0s), "") ) |]
            [| ( "1^1", (Expr_Op ('^', Expr_Oct 1s, Expr_Oct 1s), (TypeAbsolute, 1s), "") ) |]
            [| ( "[0] \n", (Expr_Group (Expr_Oct 0s), (TypeAbsolute, 0s), " \n") ) |]
            [| ( "1+[2*2]",
                 (Expr_Op ('+', Expr_Oct 1s, Expr_Group (Expr_Op ('*', Expr_Oct 2s, Expr_Oct 2s))), (TypeAbsolute, 5s), "") ) |]
            [| ( "5 6 5", (Expr_Op ('+', Expr_Op ('+', Expr_Oct 5s, Expr_Oct 6s), Expr_Oct 5s), (TypeAbsolute, 16s), "") ) |]
            [| ( "[1 1]", (Expr_Group (Expr_Op ('+', Expr_Oct 1s, Expr_Oct 1s)), (TypeAbsolute, 2s), "") ) |]
            [| ( "[2][3*2]",
                 (Expr_Op ('+', Expr_Group (Expr_Oct 2s), Expr_Group (Expr_Op ('*', Expr_Oct 3s, Expr_Oct 2s))), (TypeAbsolute, 8s), "") ) |]
            [| ( "3[2*[3 2]]",
                 (Expr_Op ('+', Expr_Oct 3s, Expr_Group (Expr_Op ('*', Expr_Oct 2s, Expr_Group (Expr_Op ('+', Expr_Oct 3s, Expr_Oct 2s))))),
                  (TypeAbsolute, 13s),
                  "") ) |]
            [| ( "% 5", (Expr_Op ('%', Expr_Dec 0s, Expr_Oct 5s), (TypeAbsolute, 0s), "") ) |]
            [| ( "+ 9.", (Expr_Op ('+', Expr_Dec 0s, Expr_Dec 9s), (TypeAbsolute, 9s), "") ) |]
            [| ( "3+++++5", (Expr_Op ('+', Expr_Oct 3s, Expr_Oct 5s), (TypeAbsolute, 8s), "") ) |]
            [| ( "1+*3", (Expr_Op ('*', Expr_Oct 1s, Expr_Oct 3s), (TypeAbsolute, 3s), "") ) |]
            [| ( "4 5-", (Expr_Op ('+', Expr_Oct 4s, Expr_Oct 5s), (TypeAbsolute, 9s), "") ) |]
            [| ( "177775 + 4", (Expr_Op ('+', Expr_Oct -3s, Expr_Oct 4s), (TypeAbsolute, 1s), "") ) |]
            [| ( "65534. + 3", (Expr_Op ('+', Expr_Dec -2s, Expr_Oct 3s), (TypeAbsolute, 1s), "") ) |]
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

    static member ShiftByMinusTestData =
        [|
            [| ( "15.\>[-2]",
                 (Expr_Op ('>', Expr_Dec 15s, Expr_Group (Expr_Op ('-', Expr_Dec 0s, Expr_Oct 2s))),
                  (TypeAbsolute, 0x38s), "") ) |]
            [| ( "15.\<[-2]",
                 (Expr_Op ('<', Expr_Dec 15s, Expr_Group (Expr_Op ('-', Expr_Dec 0s, Expr_Oct 2s))),
                  (TypeAbsolute, 3s), "") ) |]
        |]

    [<TestCaseSource("ShiftByMinusTestData")>]
    member x.ShiftByMinusExpression data =
        let src, expected = data
        let actual = expres src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "expres %A -> %A" src actual)

