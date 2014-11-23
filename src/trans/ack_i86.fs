namespace Ack_i86

module Ack_i86_trans =

    open StatementType
    open Instruction

    let ack_i86_asm pdp11as =
        let doubleOp code dest src =
            match code with
            | "mov" -> movType code dest src
            | _ -> ""

        let transStatement = function
            | DoubleOp (code, src, dest) -> doubleOp code dest src
            | _ -> ""

        List.map transStatement pdp11as

