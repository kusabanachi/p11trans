namespace test

open System
open NUnit.Framework
open Expres
open Addres


[<TestFixture>]
type AddresTest() =

    static member AddresTestData =
        [|
            [| ( "2.+r0", (Reg R2, "") ) |]
            [| ( "(r1+3)+aa", (IncDfr R4, "aa") ) |]
            [| ( "-(r2)aa", (DecDfr R2, "aa") ) |]
            [| ( "-16(r3)", (IdxDfr (R3, Expr_Op ('-', Expr_Dec 0s, Expr_Oct 0o16s)), "") ) |]
            [| ( "(r0)aa", (Dfr R0, "aa") ) |]
            [| ( "*r0, aa", (Dfr R0, ", aa") ) |]
            [| ( "*(r1)+", (IncDDfr R1, "") ) |]
            [| ( "*-(r1)", (DecDDfr R1, "") ) |]
            [| ( "*(r1)", (DDfr R1, "") ) |]
            [| ( "*9(r1)", (IdxDDfr (R1, Expr_Oct 9s), "") ) |]
            [| ( "-12.", (Rel (Expr_Op ('-', Expr_Dec 0s, Expr_Dec 12s)), "") ) |]
            [| ( "$-1.", (Imm (Expr_Op ('-', Expr_Dec 0s, Expr_Dec 1s)), "") ) |]
            [| ( "*-1.", (RelDfr (Expr_Op ('-', Expr_Dec 0s, Expr_Dec 1s)), "") ) |]
            [| ( "*$-1.", (Abs (Expr_Op ('-', Expr_Dec 0s, Expr_Dec 1s)), "") ) |]
        |]

    [<TestCaseSource("AddresTestData")>]
    member x.ValidAddres data =
        let src, expected = data
        let actual = addres src
        let compare = actual = expected
        Assert.IsTrue(compare, sprintf "addres %A -> %A" src actual)


    static member AddresErrorTestData =
        [|
            [| "(r2,)" |]
            [| "-(sp" |]
            [| "**3" |]
            [| "*(r2,)" |]
            [| "(r5+3)" |]
            [| "-(r5+3)" |]
            [| "r5+3" |]
        |]

    [<TestCaseSource("AddresErrorTestData")>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.invalidAddress src =
        addres src |> ignore


