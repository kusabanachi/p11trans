namespace Ack_i86

module InstructionAsm =

    open Address
    open Expres

    let tempMem = "tmpMem"

    let inline (+!!+) (i1:string) (i2:string) =
        if i1.Length <> 0 && i2.Length <> 0 then
            i1 + ";  " + i2
        else
            i1 + i2


    type instructionType = | Word | Byte

    type instructionAsm (itype: instructionType) =
        let movText (a1:addr) (a2:addr) =
            "mov " + a1.text + ", " + a2.text

        let leaText (a1:addr) (a2:addr) =
            "lea " + a1.text + ", " + a2.text

        let incDfrNum = function
            | SP ->       2
            | _  ->
                match itype with
                | Word -> 2
                | Byte -> 1

        let dfrAddr reg num =
            Dfr (reg, Some (Expr_Dec (int16 num)))

        let incText r1 r2 num =
            leaText (Reg r1) (dfrAddr r2 num)


        member this.moveRef (dReg:reg) sAddr =
            let midReg = if dReg.isMemoryAccessible then dReg else utilReg

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (Reg sReg)
                       +!!+ incText sReg sReg incNum,
                     Dfr (dReg, None))
                elif dReg.isMemoryAccessible then
                    (movText (Reg dReg) (Reg sReg)
                       +!!+ incText sReg dReg incNum,
                     Dfr (dReg, None))
                else
                    (movText (Reg dReg) (Reg sReg)
                       +!!+ movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg incNum,
                     Dfr (dReg, None))
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg dReg) (Reg sReg),
                     Dfr (dReg, None))
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg dReg) (Reg sReg),
                     Dfr (dReg, None))
            | Dfr (sReg, expr) ->
                (movText (Reg dReg) (Reg sReg),
                 Dfr (dReg, expr))
            | IncDDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (Dfr (sReg, None))
                       +!!+ incText sReg sReg incNum,
                     Dfr (dReg, None))
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg incNum
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Dfr (dReg, None))
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg dReg) (Dfr (sReg, None)),
                     Dfr (dReg, None))
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg dReg) (dfrAddr midReg decNum),
                     Dfr (dReg, None))
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (Dfr (sReg, expr)),
                     Dfr (dReg, None))
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ movText (Reg dReg) (Dfr (midReg, expr)),
                     Dfr (dReg, None))
            | Rel expr | Abs expr ->
                (movText (Reg dReg) (Imm expr),
                 Dfr (dReg, None))
            | RelDfr expr ->
                (movText (Reg dReg) (Rel expr),
                 Dfr (dReg, None))
            | _ ->
                failwithf "Invalid address"


        member this.moveVal (dReg:reg) sAddr =
            let midReg = if dReg.isMemoryAccessible then dReg else utilReg

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (Dfr (sReg, None))
                       +!!+ incText sReg sReg incNum,
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg incNum
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Reg dReg)
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg dReg) (Dfr (sReg, None)),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg dReg) (dfrAddr midReg decNum),
                     Reg dReg)
            | Dfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) sAddr,
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ movText (Reg dReg) (Dfr (midReg, expr)),
                     Reg dReg)
            | IncDDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg midReg) (Dfr (sReg, None))
                       +!!+ incText sReg sReg incNum
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg incNum
                       +!!+ movText (Reg midReg) (Dfr (midReg, None))
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Reg dReg)
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg midReg) (Dfr (sReg, None))
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg midReg) (dfrAddr midReg decNum)
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Reg dReg)
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg midReg) (Dfr (sReg, expr))
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ movText (Reg midReg) (Dfr (midReg, expr))
                       +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                     Reg dReg)
            | Reg _ | Rel _ | Abs _ | Imm _ ->
                (movText (Reg dReg) sAddr,
                 Reg dReg)
            | RelDfr expr ->
                (movText (Reg midReg) (Rel expr)
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)


        member this.moveValToMem symbol sAddr =
            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg utilReg) (Dfr (sReg, None))
                       +!!+ incText sReg sReg incNum
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg incNum
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg utilReg) (Dfr (sReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg decNum
                       +!!+ movText (Reg utilReg) (dfrAddr utilReg decNum)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | Dfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg utilReg) sAddr
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | IncDDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg utilReg) (Dfr (sReg, None))
                       +!!+ incText sReg sReg incNum
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg incNum
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg utilReg) (Dfr (sReg, None))
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg decNum
                       +!!+ movText (Reg utilReg) (dfrAddr utilReg decNum)
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg utilReg) (Dfr (sReg, expr))
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | Rel _ | Abs _ ->
                (movText (Reg utilReg) sAddr
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            | RelDfr expr ->
                (movText (Reg utilReg) (Rel expr)
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            | Reg _ | Imm _ ->
                (movText (Abs (Expr_Sym symbol)) sAddr,
                 Abs (Expr_Sym symbol))


        member this.pushVal sAddr =
            let pushText (a:addr) = "push " + a.text

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    pushText (Dfr (sReg, None))
                      +!!+ incText sReg sReg incNum
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg incNum
                      +!!+ pushText (Dfr (utilReg, None))
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    incText sReg sReg decNum
                      +!!+ pushText (Dfr (sReg, None))
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg decNum
                      +!!+ pushText (dfrAddr utilReg decNum)
            | Dfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    pushText sAddr
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ pushText (Dfr (utilReg, expr))
            | IncDDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    movText (Reg utilReg) (Dfr (sReg, None))
                      +!!+ pushText (Dfr (utilReg, None))
                      +!!+ incText sReg sReg incNum
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg incNum
                      +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                      +!!+ pushText (Dfr (utilReg, None))
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    incText sReg sReg decNum
                      +!!+ movText (Reg utilReg) (Dfr (sReg, None))
                      +!!+ pushText (Dfr (utilReg, None))
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg decNum
                      +!!+ movText (Reg utilReg) (dfrAddr utilReg decNum)
                      +!!+ pushText (Dfr (utilReg, None))
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    movText (Reg utilReg) sAddr
                      +!!+ pushText (Dfr (utilReg, None))
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                      +!!+ pushText (Dfr (utilReg, None))
            | Reg _ | Rel _ | Abs _ ->
                pushText sAddr
            | RelDfr expr ->
                movText (Reg utilReg) (Rel expr)
                  +!!+ pushText (Dfr (utilReg, None))
            | Imm _ ->
                movText (Reg utilReg) sAddr
                  +!!+ pushText (Dfr (utilReg, None))


        member this.popValTo dAddr =
            let popText (a:addr) = "pop " + a.text

            match dAddr with
            | IncDfr dReg ->
                let incNum = incDfrNum dReg
                if dReg.isMemoryAccessible then
                    popText (Dfr (dReg, None))
                      +!!+ incText dReg dReg incNum
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg incNum
                      +!!+ popText (Dfr (utilReg, None))
            | DecDfr dReg ->
                let decNum = -(incDfrNum dReg)
                if dReg.isMemoryAccessible then
                    incText dReg dReg decNum
                      +!!+ popText (Dfr (dReg, None))
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg decNum
                      +!!+ popText (dfrAddr utilReg decNum)
            | Dfr (dReg, expr) ->
                if dReg.isMemoryAccessible then
                    popText dAddr
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ popText (Dfr (utilReg, expr))
            | IncDDfr dReg ->
                let incNum = incDfrNum dReg
                if dReg.isMemoryAccessible then
                    movText (Reg utilReg) (Dfr (dReg, None))
                      +!!+ popText (Dfr (utilReg, None))
                      +!!+ incText dReg dReg incNum
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg incNum
                      +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                      +!!+ popText (Dfr (utilReg, None))
            | DecDDfr dReg ->
                let decNum = -(incDfrNum dReg)
                if dReg.isMemoryAccessible then
                    incText dReg dReg decNum
                      +!!+ movText (Reg utilReg) (Dfr (dReg, None))
                      +!!+ popText (Dfr (utilReg, None))
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg decNum
                      +!!+ movText (Reg utilReg) (dfrAddr utilReg decNum)
                      +!!+ popText (Dfr (utilReg, None))
            | DDfr (dReg, expr) ->
                if dReg.isMemoryAccessible then
                    movText (Reg utilReg) dAddr
                      +!!+ popText (Dfr (utilReg, None))
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                      +!!+ popText (Dfr (utilReg, None))
            | Reg _ | Rel _ | Abs _ ->
                popText dAddr
            | RelDfr expr ->
                movText (Reg utilReg) (Rel expr)
                  +!!+ popText (Dfr (utilReg, None))
            | _ ->
                failwithf "Invalid address: pop to %A" dAddr


        member this.incrementReg (reg:reg) num =
            if reg.isMemoryAccessible then
                leaText (Reg reg) (dfrAddr reg num)
            else
                movText (Reg utilReg) (Reg reg)
                  +!!+ leaText (Reg reg) (dfrAddr utilReg num)


        member this.binaryCalc codeStr (dAddr:addr) (sAddr:addr) =
            let codeText (a1:addr) (a2:addr) =
                match itype with
                | Word ->
                    codeStr + " " + a1.text + ", " + a2.text
                | Byte ->
                    codeStr + " " + a1.byteText + ", " + a2.byteText

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                codeText dAddr (Dfr (sReg, None))
                  +!!+ incText sReg sReg incNum
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                incText sReg sReg decNum
                  +!!+ codeText dAddr (Dfr (sReg, None))
            | _ ->
                match dAddr with
                | IncDfr dReg ->
                    let incNum = incDfrNum dReg
                    codeText (Dfr (dReg, None)) sAddr
                      +!!+ incText dReg dReg incNum
                | DecDfr dReg ->
                    let decNum = -(incDfrNum dReg)
                    incText dReg dReg decNum
                      +!!+ codeText (Dfr (dReg, None)) sAddr
                | _ ->
                    codeText dAddr sAddr


        member this.unaryCalc codeStr addr =
            let codeText (a:addr) =
                match itype with
                | Word ->
                    codeStr + " " + a.text
                | Byte ->
                    codeStr + " " + a.byteText

            match addr with
            | IncDfr reg ->
                let incNum = incDfrNum reg
                codeText (Dfr (reg, None))
                  +!!+ incText reg reg incNum
            | DecDfr reg ->
                let decNum = -(incDfrNum reg)
                incText reg reg decNum
                  +!!+ codeText (Dfr (reg, None))
            | _ ->
                codeText addr


        member this.storeRegVal reg =
            movText (Abs (Expr_Sym tempMem)) (Reg reg)

        member this.restoreRegVal reg =
            movText (Reg reg) (Abs (Expr_Sym tempMem))



module WordInstructionAsm =

    open InstructionAsm

    let tempMem = tempMem

    let inline (+!!+) i1 i2 = (+!!+) i1 i2


    let asm = instructionAsm (itype = Word)

    let moveRef dReg sAddr = asm.moveRef dReg sAddr

    let moveVal dReg sAddr = asm.moveVal dReg sAddr

    let moveValToMem symbol sAddr = asm.moveValToMem symbol sAddr

    let pushVal sAddr = asm.pushVal sAddr

    let popValTo dAddr = asm.popValTo dAddr

    let incrementReg reg num = asm.incrementReg reg num

    let binaryCalc codeStr dAddr sAddr = asm.binaryCalc codeStr dAddr sAddr

    let unaryCalc codeStr addr = asm.unaryCalc codeStr addr

    let storeRegVal reg = asm.storeRegVal reg

    let restoreRegVal reg = asm.restoreRegVal reg


