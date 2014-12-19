namespace Ack_i86

open Address
open V6as.Expres
open WordInstructionAsm
open ExpressionType

module Instruction =

    let private findFreeReg (a1:addr) (a2:addr) =
        let notUsing reg =
            not (a1.isUsing reg || a2.isUsing reg)
        if notUsing SI then
            SI
        elif notUsing DI then
            DI
        else
            BP

    let destAddrAffectSrcVal = function
        | src: addr, IncDfr destReg
        | src      , DecDfr destReg
        | src      , IncDDfr destReg
        | src      , DecDDfr destReg
            when src.isUsing destReg -> true
        | _                          -> false

    let srcAddrAffectDestVal = function
        | IncDfr srcReg,  dest: addr
        | DecDfr srcReg,  dest
        | IncDDfr srcReg, dest
        | DecDDfr srcReg, dest
            when dest.isUsing srcReg -> true
        | _                          -> false


    let addType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if dest.isAccessible then
            let code1, src =
                if not dest.isMemory && not src.isAccessible then
                    moveRefOrVal utilReg src
                elif dest.isMemory && src.isMemory
                        || destAddrAffectSrcVal (src, dest) then
                    moveVal utilReg src
                else
                    "", src
            let code2 = binaryCalc code dest src
            code1 +!!+ code2
        elif not src.isMemory
                 && not (destAddrAffectSrcVal (src, dest)) then
            let code1, dest = moveRef utilReg dest
            let code2       = binaryCalc code dest src
            code1 +!!+ code2
        else
            let saveReg = findFreeReg src dest
            let code1       = storeRegVal saveReg
            let code2, src  = moveVal saveReg src
            let code3, dest = moveRef utilReg dest
            let code4       = binaryCalc code dest src
            let code5       = restoreRegVal saveReg
            code1 +!!+ code2 +!!+ code3 +!!+ code4 +!!+ code5


    let movType code i_dest i_src =
        let dest = i86Addr i_dest
        let src = i86Addr i_src

        if dest = DecDfr SP then
            pushVal src
        elif src = IncDfr SP && not (dest.isUsing SP) then
            popValTo dest
        elif dest = dfr SP && not src.isImmediate then
            if not (src.isUsing SP) then
                let code1 = popValTo (Reg utilReg)
                let code2 = pushVal src
                code1 +!!+ code2
            else
                let code1, src = moveVal utilReg src
                let code2 = popValTo (namedMem tempMem)
                let code3 = pushVal src
                code1 +!!+ code2 +!!+ code3
        elif not dest.isMemory then
            let dReg = dest.getRegister
            let src =
                match src with
                | IncDfr sReg when sReg = dReg ->
                    dfr sReg
                | IncDDfr sReg when sReg = dReg ->
                    ddfr sReg
                | _ ->
                    src
            let code1, src =
                if not src.isAccessible then
                    moveRefOrVal utilReg src
                else
                    "", src
            let code2 = binaryCalc code dest src
            code1 +!!+ code2
        elif dest.isAccessible then
            addType code i_dest i_src
        elif not src.isMemory
                 && not (destAddrAffectSrcVal (src, dest)) then
            addType code i_dest i_src
        elif not (dest.isUsing SP) then
            let code1 = pushVal src
            let code2 = popValTo dest
            code1 +!!+ code2
        else
            addType code i_dest i_src


    let cmpType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src
        let binaryCalc' d s =
            if code <> "cmp" then
                binaryCalc code d s
            else
                binaryCalc code s d
        let destIsAccessible =
            dest.isAccessible
                && not (dest.isImmediate && code <> "cmp")
        let srcIsAccessible =
            src.isAccessible
                && not (src.isImmediate && code = "cmp")

        if destIsAccessible then
            let code1, src =
                if not dest.isMemory && not srcIsAccessible then
                    moveRefOrVal utilReg src
                elif dest.isMemory && src.isMemory
                        || not srcIsAccessible
                        || destAddrAffectSrcVal (src, dest) then
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


    let bicType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        if dest.isAccessible then
            let code1, src = moveVal utilReg src
            let code2      = invert src
            let code3      = binaryCalc "and" dest src
            code1 +!!+ code2 +!!+ code3
        else
            let saveReg = findFreeReg src dest
            let code1       = storeRegVal saveReg
            let code2, src  = moveVal saveReg src
            let code3       = invert src
            let code4, dest = moveRef utilReg dest
            let code5       = binaryCalc "and" dest src
            let code6       = restoreRegVal saveReg
            code1 +!!+ code2 +!!+ code3
              +!!+ code4 +!!+ code5 +!!+ code6


    let incType code addr =
        let addr = i86Addr addr

        if not addr.isAccessible then
            let code1, addr = moveRef utilReg addr
            let code2       = unaryCalc code addr
            code1 +!!+ code2
        else
            unaryCalc code addr


    let mulType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        match dest with
        | Reg AX ->
            let code1, src =
                if src.isImmediate || not src.isAccessible then
                    moveRefOrVal utilReg src
                else
                    "", src
            let code2 = unaryCalc "imul" src
            let code3 = exchangeVal (Reg AX) (Reg DX)
            code1 +!!+ code2 +!!+ code3
        | Reg DX ->
            let code1, src, axSv =
                if src.isImmediate then
                    let c11, axSv = moveVal utilReg (Reg AX)
                    let c12, _    = moveVal AX dest
                    let c13, src' = moveVal DX src
                    c11 +!!+ c12 +!!+ c13, src', axSv
                elif not src.isAccessible || src.isUsing AX then
                    match src with
                    | Reg    AX
                    | DecDfr AX
                    | Dfr   (AX, _) ->
                        let c11, src' = moveRefOrVal utilReg src
                        let c12, _    = moveVal AX dest
                        c11 +!!+ c12, src', (Reg utilReg)
                    | _ ->
                        let c11, src' = moveRef utilReg src
                        let c12, axSv = moveValToMem tempMem (Reg AX)
                        let c13, _    = moveVal AX dest
                        c11 +!!+ c12 +!!+ c13, src', axSv
                else
                    let c11, axSv = moveVal utilReg (Reg AX)
                    let c12, _    = moveVal AX dest
                    c11 +!!+ c12, src, axSv
            let code2    = unaryCalc "imul" src
            let code3, _ = moveVal DX (Reg AX)
            let code4, _ = moveVal AX axSv
            code1 +!!+ code2 +!!+ code3 +!!+ code4
        | Reg dReg when dReg = CX || dReg = DI ->
            let nextR = nextReg dReg

            let code1, src =
                if src = Reg dReg
                        || (src.isUsing AX && (swapReg src dReg).isAccessible) then
                    let c11, _ = moveVal nextR (Reg DX)
                    let c12    = exchangeVal (Reg AX) dest
                    let src' =
                        if src = Reg dReg then
                            Reg AX
                        else
                            swapReg src dReg
                    c11 +!!+ c12, src'
                elif src = Reg nextR then
                    let c11 = exchangeVal (Reg DX) (Reg nextR)
                    let c12 = exchangeVal (Reg AX) dest
                    c11 +!!+ c12, (Reg DX)
                elif not (src.isAccessible)
                        || src.isUsing dReg
                        || src.isUsing nextR
                        || src.isImmediate then
                    let c11, src' = moveRefOrVal utilReg src
                    let c12, _    = moveVal nextR (Reg DX)
                    let c13       = exchangeVal (Reg AX) dest
                    c11 +!!+ c12 +!!+ c13, src'
                else
                    let c11, _ = moveVal nextR (Reg DX)
                    let c12    = exchangeVal (Reg AX) dest
                    c11 +!!+ c12, src
            let code2 = unaryCalc "imul" src
            let code3 = exchangeVal (Reg dReg) (Reg DX)
            let code4 = exchangeVal (Reg nextR) (Reg AX)
            let code5 = exchangeVal (Reg AX) (Reg DX)
            code1 +!!+ code2 +!!+ code3 +!!+ code4 +!!+ code5
        | Reg dReg (* SI or BP *) ->
            let code1, src, dxSv =
                if src = Reg dReg
                        || (src.isUsing AX && (swapReg src dReg).isAccessible) then
                    let c11, dxSv = moveVal utilReg (Reg DX)
                    let c12       = exchangeVal (Reg AX) dest
                    let src' =
                        if src = Reg dReg then
                            Reg AX
                        else
                            swapReg src dReg
                    c11 +!!+ c12, src', dxSv
                elif src.isImmediate then
                    let c11, dxSv = moveVal utilReg (Reg DX)
                    let c12, src' = moveVal DX src
                    let c13       = exchangeVal (Reg AX) dest
                    c11 +!!+ c12 +!!+ c13, src', dxSv
                elif not (src.isAccessible)
                        || src.isUsing dReg then
                    match src with
                    | DecDfr DX
                    | Dfr   (DX, _) ->
                        let c11, src' = moveRef utilReg src
                        let c12       = exchangeVal (Reg AX) dest
                        c11 +!!+ c12, src', (Reg utilReg)
                    | _ ->
                        let c11, src' = moveRef utilReg src
                        let c12, dxSv = moveValToMem tempMem (Reg DX)
                        let c13       = exchangeVal (Reg AX) dest
                        c11 +!!+ c12 +!!+ c13, src', dxSv
                else
                    let c11, dxSv = moveVal utilReg (Reg DX)
                    let c12       = exchangeVal (Reg AX) dest
                    c11 +!!+ c12, src, dxSv
            let code2     = unaryCalc "imul" src
            let code3     = exchangeVal (Reg dReg) (Reg AX)
            let code4, _  = moveVal DX dxSv
            code1 +!!+ code2 +!!+ code3 +!!+ code4
        | _ ->
            failwithf "Invalid address"


    let divType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let conditionCheck, src, endLabel =
            let c1, src =
                if not src.isAccessible
                        || src.isIncrement
                        || src.isDecrement then
                    moveVal utilReg src
                else
                    "", src
            let c2, endL = divConditionCheck dest src
            c1 +!!+ c2, src, endL

        match dest with
        | Reg AX ->
            let code1, src =
                if src.isImmediate || not src.isAccessible then
                    moveRefOrVal utilReg src
                elif src = Reg AX then
                    "", Reg DX
                elif src = Reg DX then
                    "", Reg AX
                else
                    "", src
            let code2 = exchangeVal (Reg AX) (Reg DX)
            let code3 = unaryCalc "idiv" src
            conditionCheck
              +!!+ code1 +!!+ code2 +!!+ code3
              +!!+ Label.nameLabel endLabel
        | Reg dReg when dReg = CX || dReg = DI ->
            let nextR = nextReg dReg
            let swapCode addr reg =
                let swapped = swapReg addr reg
                if swapped.isAccessible then
                    "", swapped
                else
                    moveRefOrVal utilReg src

            let code1, src =
                if src.isUsing AX then
                    swapCode src nextR
                elif src.isUsing dReg then
                    swapCode src DX
                elif src.isUsing DX then
                    swapCode src dReg
                elif src.isUsing nextR then
                    swapCode src AX
                elif src.isImmediate || not src.isAccessible then
                    moveRefOrVal utilReg src
                else
                    "", src
            let code2 = exchangeVal (Reg DX) (Reg dReg)
            let code3 = exchangeVal (Reg AX) (Reg nextR)
            let code4 = unaryCalc "idiv" src
            let code5 = exchangeVal (Reg dReg) (Reg AX)
            let code6 = exchangeVal (Reg nextR) (Reg DX)
            let code7 = exchangeVal (Reg AX) (Reg DX)
            conditionCheck
              +!!+ code1 +!!+ code2 +!!+ code3 +!!+ code4
              +!!+ code5 +!!+ code6 +!!+ code7
              +!!+ Label.nameLabel endLabel
        | _ ->
            ""


    let ashType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let code1, cxSv =
            if src <> Reg CX then
                let c11, src = moveVal utilReg src
                let c12      = exchangeVal (Reg CX) src
                c11 +!!+ c12, Reg utilReg
            else
                moveVal utilReg (Reg CX)
        let dest = if dest = Reg CX then
                       cxSv
                   else
                       dest
        let code2    = shftLeftOrRight dest
        let code3, _ = moveVal CX cxSv
        code1 +!!+ code2 +!!+ code3


    let ashcType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let code1, cxSv =
            if src <> Reg CX then
                let c11, src = moveVal utilReg src
                let c12      = exchangeVal (Reg CX) src
                c11 +!!+ c12, Reg utilReg
            else
                moveVal utilReg (Reg CX)
        let dest, next =
            let dReg = dest.getRegister
            if dest = Reg CX then
                cxSv, Reg (nextReg dReg)
            elif dest = Reg DX then
                dest, cxSv
            else
                dest, Reg (nextReg dReg)
        let code2    = shftLeftOrRight32bit dest next
        let code3, _ = moveVal CX cxSv
        code1 +!!+ code2 +!!+ code3


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


    let sxtType addr =
        let addr = i86Addr addr

        let code1, addr =
            if not addr.isAccessible
                    || addr.isIncrement
                    || addr.isDecrement then
                moveRef utilReg addr
            else
                "", addr
        let code2 = fillWithNFlag addr
        code1 +!!+ code2


    let markType expr =
        let twofoldExpr = Expr_Op ('*', expr, Expr_Oct 2s)
        let regR5 = V6as.Addres.Reg V6as.Addres.R5

        let code1 = binaryCalc "add" (Reg SP) (Imm twofoldExpr)
        let code2 = rtsType regR5
        code1 +!!+ code2


    let sobType expr src =
        let src = i86Addr src
        let expr = i86Addr expr

        if src = Reg CX then
            unaryCalc "loop" expr
        else
            let code1 = unaryCalc "dec" src
            let code2 = unaryCalc "jne" expr
            code1 +!!+ code2


