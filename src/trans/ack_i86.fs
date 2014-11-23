namespace Ack_i86

module Ack_i86_trans =

    open StatementType
    open Instruction
    open Expres

    let ack_i86_asm pdp11as =
        let singleOp code addr =
            let immVal num = Addres.Imm (Expr_Oct (int16 num))
            match code with
            | "clr" -> addType "and" addr (immVal 0)
            | "com" -> addType "xor" addr (immVal 0xffff)
            | "adc" -> addType "adc" addr (immVal 0)
            | "sbc" -> addType "sbb" addr (immVal 0)
            | "ror" -> addType "rcr" addr (immVal 1)
            | "rol" -> addType "rcl" addr (immVal 1)
            | "asr" -> addType "sar" addr (immVal 1)
            | "asl" -> addType "sal" addr (immVal 1)
            | _ -> ""

        let doubleOp code dest src =
            match code with
            | "mov" -> movType "mov" dest src
            | "bis" -> addType "or" dest src
            | "add" -> addType "add" dest src
            | "sub" -> addType "sub" dest src
            | _ -> ""

        let transStatement = function
            | SingleOp (code, addr)      -> singleOp code addr
            | DoubleOp (code, src, dest) -> doubleOp code dest src
            | _ -> ""

        List.map transStatement pdp11as

