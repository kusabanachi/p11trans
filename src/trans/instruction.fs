namespace Ack_i86

module Instruction =

    open Address
    open Expres
    open InstructionAsm

    let private findFreeReg (a1:addr) (a2:addr) =
        let notUsing reg =
            not (a1.isUsing reg || a2.isUsing reg)
        if notUsing SI then
            SI
        elif notUsing DI then
            DI
        else
            BP


    let movType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if dest = DecDfr SP then
            pushVal src
        elif src = IncDfr SP then
            popValTo dest
        elif dest = Dfr (SP, None) then
            let code1 = incrementReg SP 2
            let code2 = pushVal src
            code1 +!!+ code2
        elif src.isMemory && dest.isMemory then
            if dest.isAccessible then
                let code1, src = moveVal utilReg src
                let code2 = binaryCalc code dest src
                code1 +!!+ code2
            elif not (dest.isUsing SP) then
                let code1 = pushVal src
                let code2 = popValTo dest
                code1 +!!+ code2
            else
                let saveReg = findFreeReg src dest
                let code1       = storeRegVal saveReg
                let code2, src  = moveVal saveReg src
                let code3, dest = moveRef utilReg dest
                let code4       = binaryCalc code dest src
                let code5       = restoreRegVal saveReg
                code1 +!!+ code2 +!!+ code3 +!!+ code4 +!!+ code5
        elif not dest.isAccessible then
            let code1, dest = moveRef utilReg dest
            let code2 = binaryCalc code dest src
            code1 +!!+ code2
        elif not src.isAccessible then
            let code1, src = moveRef utilReg src
            let code2 = binaryCalc code dest src
            code1 +!!+ code2
        else
            binaryCalc code dest src


