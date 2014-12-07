namespace Ack_i86

module Instruction =

    open Address
    open V6as.Expres
    open WordInstructionAsm

    let private findFreeReg (a1:addr) (a2:addr) =
        let notUsing reg =
            not (a1.isUsing reg || a2.isUsing reg)
        if notUsing SI then
            SI
        elif notUsing DI then
            DI
        else
            BP

    let private destAddrAffectSrcVal = function
        | Reg srcReg, IncDfr destReg
        | Reg srcReg, DecDfr destReg
        | Reg srcReg, IncDDfr destReg
        | Reg srcReg, DecDDfr destReg
            when srcReg = destReg -> true
        | _                       -> false


    let movType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if dest = DecDfr SP then
            pushVal src
        elif src = IncDfr SP && not (dest.isUsing SP) then
            popValTo dest
        elif dest = dfr SP then
            if not (src.isUsing SP) then
                let code1 = popValTo (Reg utilReg)
                let code2 = pushVal src
                code1 +!!+ code2
            else
                let code1, src = moveVal utilReg src
                let code2 = popValTo (namedMem tempMem)
                let code3 = pushVal src
                code1 +!!+ code2 +!!+ code3
        elif src.isMemory && dest.isMemory ||
             destAddrAffectSrcVal (src, dest) then
            if dest.isAccessible then
                let code1, src = moveVal utilReg src
                let code2      = binaryCalc code dest src
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
            let code2       = binaryCalc code dest src
            code1 +!!+ code2
        elif not src.isAccessible then
            let src =
                match src, dest with
                | IncDfr  srcReg,  Reg destReg
                    when srcReg = destReg -> dfr srcReg
                | IncDDfr srcReg,  Reg destReg
                    when srcReg = destReg -> ddfr srcReg
                | _                       -> src
            let code1, src = moveRef utilReg src
            let code2      = binaryCalc code dest src
            code1 +!!+ code2
        else
            let src =
                match src, dest with
                | IncDfr srcReg,  Reg destReg
                    when srcReg = destReg -> dfr srcReg
                | _                       -> src
            binaryCalc code dest src


    let addType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if src.isMemory && dest.isMemory ||
             destAddrAffectSrcVal (src, dest) then
            if dest.isAccessible then
                let code1, src = moveVal utilReg src
                let code2      = binaryCalc code dest src
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
            let code2       = binaryCalc code dest src
            code1 +!!+ code2
        elif not src.isAccessible then
            let code1, src = moveRef utilReg src
            let code2      = binaryCalc code dest src
            code1 +!!+ code2
        else
            binaryCalc code dest src


    let cmpType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if src.isMemory && dest.isMemory then
            if dest.isAccessible then
                let code1, src = moveVal utilReg src
                let code2      = binaryCalc code dest src
                code1 +!!+ code2
            elif src.isAccessible then
                let code1, dest = moveVal utilReg dest
                let code2       = binaryCalc code dest src
                code1 +!!+ code2
            else
                let code1, src  = moveValToMem tempMem src
                let code2, dest = moveVal utilReg dest
                let code3       = binaryCalc code dest src
                code1 +!!+ code2 +!!+ code3
        elif not dest.isAccessible then
            let code1, dest = moveRef utilReg dest
            let code2       = binaryCalc code dest src
            code1 +!!+ code2
        elif not src.isAccessible then
            let code1, src = moveRef utilReg src
            let code2      = binaryCalc code dest src
            code1 +!!+ code2
        else
            binaryCalc code dest src


    let incType code addr =
        let addr = i86Addr addr

        if not addr.isAccessible then
            let code1, addr = moveRef utilReg addr
            let code2       = unaryCalc code addr
            code1 +!!+ code2
        else
            unaryCalc code addr


    let sysType = systemCall


    let jsrType dest reg =
        match i86Addr reg with
        | Reg IP ->
            incType "call" dest
        | reg ->
            let dest = i86Addr dest
            let code1, dest =
                if not dest.isAccessible then
                    moveRef utilReg dest
                else
                    resolveIncDec dest
            let code2 = pushVal reg
            let code3 = storePCtoRegAndJmpToDest reg dest
            code1 +!!+ code2 +!!+ code3


    let rtsType addr =
        match i86Addr addr with
        | Reg IP ->
            "ret"
        | reg ->
            let code1, _ = moveVal utilReg reg
            let code2    = popValTo reg
            let code3    = unaryCalc "jmp" (dfr utilReg)
            code1 +!!+ code2 +!!+ code3


