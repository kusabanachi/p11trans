namespace Ack_i86

open Address
open ByteInstructionAsm
open Instruction

module ByteInstruction =

    let private findFreeReg (a1:addr) (a2:addr) =
        let notUsing reg =
            not (a1.isUsing reg || a2.isUsing reg)
        if notUsing AX then
            AX
        elif notUsing CX then
            CX
        else
            DX


    let andbType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if dest.isByteAccessible then
            let code1, src =
                if not dest.isMemory && not src.isByteAccessible then
                    moveRefOrVal utilReg src
                elif dest.isMemory && src.isMemory
                    || not src.isByteAccessible then
                    moveVal utilReg src
                else
                    "", src
            let code2 = binaryCalc code dest src
            code1 +!!+ code2
        elif not dest.isMemory then
            let dReg = dest.getRegister
            let code1, src  =
                if src.isByteAccessible
                        && not (srcAddrAffectDestVal (src, dest)) then
                    "", src
                else
                    moveValToMem tempMem src
            let code2, dest = moveVal utilReg dest
            let code3       = binaryCalc code dest src
            let code4, _    = moveVal dReg dest
            code1 +!!+ code2 +!!+ code3 +!!+ code4
        elif src.isByteAccessible
                && not src.isMemory
                && not (destAddrAffectSrcVal (src, dest)) then
            let code1, dest = moveRef utilReg dest
            let code2       = binaryCalc code dest src
            code1 +!!+ code2
        else
            let saveReg     = findFreeReg src dest
            let code1       = storeRegVal saveReg
            let code2, src  = moveVal saveReg src
            let code3, dest = moveRef utilReg dest
            let code4       = binaryCalc code dest src
            let code5       = restoreRegVal saveReg
            code1 +!!+ code2 +!!+ code3 +!!+ code4 +!!+ code5


    let movbType code i_dest i_src =
        let dest = i86Addr i_dest
        let src = i86Addr i_src

        match dest with
        | Reg AX ->
            let code1, src =
                if src.isByteAccessible then
                    "", src
                else
                    moveRefOrVal utilReg src
            let code2 = binaryCalc code dest src
            let code3 = signExtend
            code1 +!!+ code2 +!!+ code3
        | Reg dReg ->
            let code1, src =
                if src = Reg dReg then
                    "", Reg AX
                elif src.isUsing AX
                        && (swapReg src dReg).isByteAccessible then
                    "", swapReg src dReg
                elif src.isByteAccessible
                        && not (src.isUsing dReg)
                        && not (src.isUsing AX) then
                    "", src
                else
                    moveRefOrVal utilReg src
            let code2 = exchangeVal (Reg AX) dest
            let code3 = binaryCalc code (Reg AX) src
            let code4 = signExtend
            let code5 = exchangeVal dest (Reg AX)
            code1 +!!+ code2 +!!+ code3 +!!+ code4 +!!+ code5
        | _ ->
            andbType code i_dest i_src


    let cmpbType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src
        let binaryCalc' d s =
            if code <> "cmpb" then
                binaryCalc code d s
            else
                binaryCalc code s d
        let destIsAccessible =
            dest.isByteAccessible
                && not (dest.isImmediate && code <> "cmpb")
        let srcIsAccessible =
            src.isByteAccessible
                && not (src.isImmediate && code = "cmpb")

        if destIsAccessible then
            let code1, src =
                if not dest.isMemory && not srcIsAccessible then
                    moveRefOrVal utilReg src
                elif dest.isMemory && src.isMemory
                        || not srcIsAccessible then
                    moveVal utilReg src
                else
                    "", src
            let code2 = binaryCalc' dest src
            code1 +!!+ code2
        elif srcIsAccessible
                && not (srcAddrAffectDestVal (src, dest))
                && not (destAddrAffectSrcVal (src, dest)) then
            let code1, dest =
                if not src.isMemory then
                    moveRefOrVal utilReg dest
                else
                    moveVal utilReg dest
            let code2       = binaryCalc' dest src
            code1 +!!+ code2
        else
            let code1, src  = moveValToMem tempMem src
            let code2, dest = moveVal utilReg dest
            let code3       = binaryCalc' dest src
            code1 +!!+ code2 +!!+ code3


