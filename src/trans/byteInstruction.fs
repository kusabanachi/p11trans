namespace Ack_i86

open Address
open ByteInstructionAsm
open Instruction

module ByteInstruction =

    let andbType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if dest = Reg AX then
            let code1, src =
                if src.isByteAccessible then
                    "", src
                else
                    moveRefOrVal utilReg src
            let code2 = binaryCalc code dest src
            let code3 = signExtend
            code1 +!!+ code2 +!!+ code3
        elif dest.isRegValue then
            let code1, src =
                if src.isByteAccessible
                        && src <> Reg AX
                        && not (usingSameReg (src, dest)) then
                    "", src
                else
                    moveRefOrVal utilReg src
            let code2 = exchangeVal (Reg AX) dest
            let code3 = binaryCalc code (Reg AX) src
            let code4 = signExtend
            let code5 = exchangeVal dest (Reg AX)
            code1 +!!+ code2 +!!+ code3 +!!+ code4 +!!+ code5
        elif dest.isAccessible then
            let code1, src =
                if src.isByteAccessible && not src.isMemory then
                    "", src
                else
                    moveVal utilReg src
            let code2 = binaryCalc code dest src
            code1 +!!+ code2
        elif src.isByteAccessible
                && not src.isMemory
                && not (destAddrAffectSrcVal (src, dest)) then
            let code1, dest = moveRef utilReg dest
            let code2       = binaryCalc code dest src
            code1 +!!+ code2
        else
            let code1, src  = moveValToMem tempMem src
            let code2, dest = moveRef utilReg dest
            let code3       = exchangeVal (Reg AX) dest
            let code4       = binaryCalc code (Reg AX) src
            let code5       = exchangeVal dest (Reg AX)
            code1 +!!+ code2 +!!+ code3 +!!+ code4 +!!+ code5


