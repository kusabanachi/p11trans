namespace Ack_i86

open Address
open ExpressionType
open Eos

module InstructionAsm =

    let tempMem = "tmpMem"

    let inline (+!!+) (i1:string) (i2:string) =
        if i1.Length <> 0 && i2.Length <> 0 then
            i1 + (eos ';') + i2
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

        let incText r1 r2 num =
            leaText (Reg r1) (idfr r2 num)


        member this.moveRef (dReg:reg) sAddr =
            let midReg = if dReg.isMemoryAccessible then dReg else utilReg

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (Reg sReg)
                       +!!+ incText sReg sReg incNum,
                     dfr dReg)
                elif dReg.isMemoryAccessible then
                    (movText (Reg dReg) (Reg sReg)
                       +!!+ incText sReg dReg incNum,
                     dfr dReg)
                else
                    (movText (Reg dReg) (Reg sReg)
                       +!!+ movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg incNum,
                     dfr dReg)
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg dReg) (Reg sReg),
                     dfr dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg dReg) (Reg sReg),
                     dfr dReg)
            | Dfr (sReg, expr) ->
                (movText (Reg dReg) (Reg sReg),
                 Dfr (dReg, expr))
            | IncDDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (dfr sReg)
                       +!!+ incText sReg sReg incNum,
                     dfr dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg incNum
                       +!!+ movText (Reg dReg) (dfr midReg),
                     dfr dReg)
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg dReg) (dfr sReg),
                     dfr dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg dReg) (idfr midReg decNum),
                     dfr dReg)
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (Dfr (sReg, expr)),
                     dfr dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ movText (Reg dReg) (Dfr (midReg, expr)),
                     dfr dReg)
            | Rel expr | Abs expr ->
                (movText (Reg dReg) (Imm expr),
                 dfr dReg)
            | RelDfr expr ->
                (movText (Reg dReg) (Rel expr),
                 dfr dReg)
            | _ ->
                failwithf "Invalid address"


        member this.moveVal (dReg:reg) sAddr =
            let midReg = if dReg.isMemoryAccessible then dReg else utilReg

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg dReg) (dfr sReg)
                       +!!+ incText sReg sReg incNum,
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg incNum
                       +!!+ movText (Reg dReg) (dfr midReg),
                     Reg dReg)
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg dReg) (dfr sReg),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg dReg) (idfr midReg decNum),
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
                    (movText (Reg midReg) (dfr sReg)
                       +!!+ incText sReg sReg incNum
                       +!!+ movText (Reg dReg) (dfr midReg),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg incNum
                       +!!+ movText (Reg midReg) (dfr midReg)
                       +!!+ movText (Reg dReg) (dfr midReg),
                     Reg dReg)
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg midReg) (dfr sReg)
                       +!!+ movText (Reg dReg) (dfr midReg),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ incText sReg midReg decNum
                       +!!+ movText (Reg midReg) (idfr midReg decNum)
                       +!!+ movText (Reg dReg) (dfr midReg),
                     Reg dReg)
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg midReg) (Dfr (sReg, expr))
                       +!!+ movText (Reg dReg) (dfr midReg),
                     Reg dReg)
                else
                    (movText (Reg midReg) (Reg sReg)
                       +!!+ movText (Reg midReg) (Dfr (midReg, expr))
                       +!!+ movText (Reg dReg) (dfr midReg),
                     Reg dReg)
            | Reg _ | Rel _ | Abs _ | Imm _ ->
                (movText (Reg dReg) sAddr,
                 Reg dReg)
            | RelDfr expr ->
                (movText (Reg midReg) (Rel expr)
                   +!!+ movText (Reg dReg) (dfr midReg),
                 Reg dReg)


        member this.moveValToMem symbol sAddr =
            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    (movText (Reg utilReg) (dfr sReg)
                       +!!+ incText sReg sReg incNum
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg incNum
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg utilReg) (dfr sReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg decNum
                       +!!+ movText (Reg utilReg) (idfr utilReg decNum)
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
                    (movText (Reg utilReg) (dfr sReg)
                       +!!+ incText sReg sReg incNum
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg incNum
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    (incText sReg sReg decNum
                       +!!+ movText (Reg utilReg) (dfr sReg)
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ incText sReg utilReg decNum
                       +!!+ movText (Reg utilReg) (idfr utilReg decNum)
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    (movText (Reg utilReg) (Dfr (sReg, expr))
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
                else
                    (movText (Reg utilReg) (Reg sReg)
                       +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                       +!!+ movText (Reg utilReg) (dfr utilReg)
                       +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                     Abs (Expr_Sym symbol))
            | Rel _ | Abs _ ->
                (movText (Reg utilReg) sAddr
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            | RelDfr expr ->
                (movText (Reg utilReg) (Rel expr)
                   +!!+ movText (Reg utilReg) (dfr utilReg)
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
                    pushText (dfr sReg)
                      +!!+ incText sReg sReg incNum
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg incNum
                      +!!+ pushText (dfr utilReg)
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    incText sReg sReg decNum
                      +!!+ pushText (dfr sReg)
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg decNum
                      +!!+ pushText (idfr utilReg decNum)
            | Dfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    pushText sAddr
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ pushText (Dfr (utilReg, expr))
            | IncDDfr sReg ->
                let incNum = incDfrNum sReg
                if sReg.isMemoryAccessible then
                    movText (Reg utilReg) (dfr sReg)
                      +!!+ pushText (dfr utilReg)
                      +!!+ incText sReg sReg incNum
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg incNum
                      +!!+ movText (Reg utilReg) (dfr utilReg)
                      +!!+ pushText (dfr utilReg)
            | DecDDfr sReg ->
                let decNum = -(incDfrNum sReg)
                if sReg.isMemoryAccessible then
                    incText sReg sReg decNum
                      +!!+ movText (Reg utilReg) (dfr sReg)
                      +!!+ pushText (dfr utilReg)
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg decNum
                      +!!+ movText (Reg utilReg) (idfr utilReg decNum)
                      +!!+ pushText (dfr utilReg)
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    movText (Reg utilReg) sAddr
                      +!!+ pushText (dfr utilReg)
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                      +!!+ pushText (dfr utilReg)
            | Reg _ | Rel _ | Abs _ ->
                pushText sAddr
            | RelDfr expr ->
                movText (Reg utilReg) (Rel expr)
                  +!!+ pushText (dfr utilReg)
            | Imm _ ->
                movText (Reg utilReg) sAddr
                  +!!+ pushText (Reg utilReg)


        member this.popValTo dAddr =
            let popText (a:addr) = "pop " + a.text

            match dAddr with
            | IncDfr dReg ->
                let incNum = incDfrNum dReg
                if dReg.isMemoryAccessible then
                    popText (dfr dReg)
                      +!!+ incText dReg dReg incNum
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg incNum
                      +!!+ popText (dfr utilReg)
            | DecDfr dReg ->
                let decNum = -(incDfrNum dReg)
                if dReg.isMemoryAccessible then
                    incText dReg dReg decNum
                      +!!+ popText (dfr dReg)
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg decNum
                      +!!+ popText (idfr utilReg decNum)
            | Dfr (dReg, expr) ->
                if dReg.isMemoryAccessible then
                    popText dAddr
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ popText (Dfr (utilReg, expr))
            | IncDDfr dReg ->
                let incNum = incDfrNum dReg
                if dReg.isMemoryAccessible then
                    movText (Reg utilReg) (dfr dReg)
                      +!!+ popText (dfr utilReg)
                      +!!+ incText dReg dReg incNum
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg incNum
                      +!!+ movText (Reg utilReg) (dfr utilReg)
                      +!!+ popText (dfr utilReg)
            | DecDDfr dReg ->
                let decNum = -(incDfrNum dReg)
                if dReg.isMemoryAccessible then
                    incText dReg dReg decNum
                      +!!+ movText (Reg utilReg) (dfr dReg)
                      +!!+ popText (dfr utilReg)
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg decNum
                      +!!+ movText (Reg utilReg) (idfr utilReg decNum)
                      +!!+ popText (dfr utilReg)
            | DDfr (dReg, expr) ->
                if dReg.isMemoryAccessible then
                    movText (Reg utilReg) dAddr
                      +!!+ popText (dfr utilReg)
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                      +!!+ popText (dfr utilReg)
            | Reg _ | Rel _ | Abs _ ->
                popText dAddr
            | RelDfr expr ->
                movText (Reg utilReg) (Rel expr)
                  +!!+ popText (dfr utilReg)
            | _ ->
                failwithf "Invalid address: pop to %A" dAddr


        member this.incrementReg (reg:reg) num =
            if reg.isMemoryAccessible then
                leaText (Reg reg) (idfr reg num)
            else
                movText (Reg utilReg) (Reg reg)
                  +!!+ leaText (Reg reg) (idfr utilReg num)


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
                codeText dAddr (dfr sReg)
                  +!!+ incText sReg sReg incNum
            | DecDfr sReg ->
                let decNum = -(incDfrNum sReg)
                incText sReg sReg decNum
                  +!!+ codeText dAddr (dfr sReg)
            | _ ->
                match dAddr with
                | IncDfr dReg ->
                    let incNum = incDfrNum dReg
                    codeText (dfr dReg) sAddr
                      +!!+ incText dReg dReg incNum
                | DecDfr dReg ->
                    let decNum = -(incDfrNum dReg)
                    incText dReg dReg decNum
                      +!!+ codeText (dfr dReg) sAddr
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
                codeText (dfr reg)
                  +!!+ incText reg reg incNum
            | DecDfr reg ->
                let decNum = -(incDfrNum reg)
                incText reg reg decNum
                  +!!+ codeText (dfr reg)
            | _ ->
                codeText addr


        member this.resolveIncDec addr =
            match addr with
            | IncDfr reg ->
                let incNum = incDfrNum reg
                (movText (Reg utilReg) (Reg reg)
                   +!!+ incText reg reg incNum,
                 dfr utilReg)
            | DecDfr reg ->
                let decNum = -(incDfrNum reg)
                (incText reg reg decNum,
                 dfr reg)
            | _ ->
                ("", addr)


        member this.storeRegVal reg =
            movText (Abs (Expr_Sym tempMem)) (Reg reg)

        member this.restoreRegVal reg =
            movText (Reg reg) (Abs (Expr_Sym tempMem))

        member this.signExtend =
            "cbw"

        member this.exchangeVal (dAddr:addr) (sAddr:addr) =
            "xchg " + dAddr.text + ", " + sAddr.text

        member this.systemCall expr =
            "int 7" +!!+ Pseudo.data1 [expr]

        member this.storePCtoRegAndJmpToDest (reg:addr) (dest:addr) =
            "call 8f"
              +!!+ "8: pop " + reg.text
              +!!+ "add " + reg.text + ", #7"
              +!!+ "jmp " + dest.text
              +!!+ "nop"



module WordInstructionAsm =

    open InstructionAsm

    let tempMem = tempMem

    let inline (+!!+) i1 i2 = (+!!+) i1 i2


    let private asm = instructionAsm (itype = Word)

    let moveRef = asm.moveRef

    let moveVal = asm.moveVal

    let moveValToMem = asm.moveValToMem

    let pushVal = asm.pushVal

    let popValTo = asm.popValTo

    let incrementReg = asm.incrementReg

    let binaryCalc = asm.binaryCalc

    let unaryCalc = asm.unaryCalc

    let resolveIncDec = asm.resolveIncDec

    let storeRegVal = asm.storeRegVal

    let restoreRegVal = asm.restoreRegVal

    let signExtend = asm.signExtend

    let exchangeVal = asm.exchangeVal

    let systemCall = asm.systemCall

    let storePCtoRegAndJmpToDest = asm.storePCtoRegAndJmpToDest


module ByteInstructionAsm =

    open InstructionAsm

    let tempMem = tempMem

    let inline (+!!+) i1 i2 = (+!!+) i1 i2


    let private asm = instructionAsm (itype = Byte)

    let moveRef = asm.moveRef

    let moveVal = asm.moveVal

    let moveValToMem = asm.moveValToMem

    let pushVal = asm.pushVal

    let popValTo = asm.popValTo

    let incrementReg = asm.incrementReg

    let binaryCalc = asm.binaryCalc

    let unaryCalc = asm.unaryCalc

    let resolveIncDec = asm.resolveIncDec

    let storeRegVal = asm.storeRegVal

    let restoreRegVal = asm.restoreRegVal

    let signExtend = asm.signExtend

    let exchangeVal = asm.exchangeVal

    let systemCall = asm.systemCall

    let storePCtoRegAndJmpToDest = asm.storePCtoRegAndJmpToDest

