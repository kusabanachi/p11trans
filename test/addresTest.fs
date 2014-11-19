namespace test

open System
open NUnit.Framework
open Expres
open Addres


[<TestFixture>]
type AddresTest() =

    static member AddresTestData =
        [|
            [| ( "2.+r0", (Reg (Expr_Op ('+', Expr_Dec 2, Expr_Sym "r0")), "") ) |]
            [| ( "(r1+3)+aa", (IncDfr (Expr_Op ('+', Expr_Sym "r1", Expr_Oct 3)), "aa") ) |]
            [| ( "-(r2)aa", (DecDfr (Expr_Sym "r2"), "aa") ) |]
            [| ( "-16(r3)", (IdxDfr (Expr_Op ('-', Expr_Dec 0, Expr_Oct 0o16), Expr_Sym "r3"), "") ) |]
            [| ( "(r0)aa", (Dfr (Expr_Sym "r0"), "aa") ) |]
            [| ( "*r0, aa", (Dfr (Expr_Sym "r0"), ", aa") ) |]
            [| ( "*(r1)+", (IncDDfr (Expr_Sym "r1"), "") ) |]
            [| ( "*-(r1)", (DecDDfr (Expr_Sym "r1"), "") ) |]
            [| ( "*(r1)", (DDfr (Expr_Sym "r1"), "") ) |]
            [| ( "*9(r1)", (IdxDDfr (Expr_Oct 9, Expr_Sym "r1"), "") ) |]
            [| ( "-12.", (Rel (Expr_Op ('-', Expr_Dec 0, Expr_Dec 12)), "") ) |]
            [| ( "$-1.", (Imm (Expr_Op ('-', Expr_Dec 0, Expr_Dec 1)), "") ) |]
            [| ( "*-1.", (RelDfr (Expr_Op ('-', Expr_Dec 0, Expr_Dec 1)), "") ) |]
            [| ( "*$-1.", (Abs (Expr_Op ('-', Expr_Dec 0, Expr_Dec 1)), "") ) |]
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
            [| "(r5+3)" |]
            [| "-(sp" |]
            [| "**3" |]
            [| "*(r2,)" |]
        |]

    [<TestCaseSource("AddresErrorTestData")>]
    [<ExpectedException(typeof<System.Exception>)>]
    member x.invalidAddress src =
        addres src |> ignore


