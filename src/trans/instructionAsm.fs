namespace Ack_i86

open Address
open ExpressionType
open Eos
open Label

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
            | IncDDfr _
            | DecDDfr _
            | DDfr _    -> 2
            | a when a.isUsing SP
                        -> 2
            | _  ->
                match itype with
                | Word  -> 2
                | Byte  -> 1

        let incText r1 r2 num =
            leaText (Reg r1) (idfr r2 num)


        let moveReferredVal (dReg:reg) ref =
            let midReg =
                if dReg.isMemoryAccessible then dReg else utilReg
            let dest = Reg dReg

            match ref with
            | IncDfr  sReg
            | IncDDfr sReg ->
                let incNum = incDfrNum ref
                if sReg = SP then
                    "pop " + dest.text
                elif sReg.isMemoryAccessible then
                    movText dest (dfr sReg)
                      +!!+ incText sReg sReg incNum
                else
                    movText (Reg midReg) (Reg sReg)
                      +!!+ incText sReg midReg incNum
                      +!!+ movText dest (dfr midReg)
            | DecDfr  sReg
            | DecDDfr sReg ->
                let decNum = -(incDfrNum ref)
                if sReg.isMemoryAccessible then
                    incText sReg sReg decNum
                      +!!+ movText dest (dfr sReg)
                else
                    movText (Reg midReg) (Reg sReg)
                      +!!+ incText sReg midReg decNum
                      +!!+ movText dest (idfr midReg decNum)
            | Dfr  (sReg, expr)
            | DDfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    movText dest (Dfr (sReg, expr))
                else
                    movText (Reg midReg) (Reg sReg)
                      +!!+ movText dest (Dfr (midReg, expr))
            | Reg _ | Rel _ | Abs _ | Imm _ ->
                movText dest ref
            | RelDfr expr ->
                movText dest (Rel expr)


        member this.moveRef (dReg:reg) sAddr =
            let midReg =
                if dReg.isMemoryAccessible then dReg else utilReg

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sAddr
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
                let decNum = -(incDfrNum sAddr)
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
            | Rel expr
            | Abs expr ->
                (movText (Reg dReg) (Imm expr),
                 dfr dReg)
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                (moveReferredVal dReg sAddr,
                 dfr dReg)
            | _ ->
                failwithf "Invalid address"


        member this.moveVal (dReg:reg) sAddr =
            let midReg =
                if dReg.isMemoryAccessible then dReg else utilReg

            match sAddr with
            | IncDfr _
            | DecDfr _
            | Dfr    _
            | Reg    _
            | Rel    _
            | Abs    _
            | Imm    _ ->
                (moveReferredVal dReg sAddr,
                 Reg dReg)
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                (moveReferredVal midReg sAddr
                   +!!+ movText (Reg dReg) (dfr midReg),
                 Reg dReg)


        member this.moveRefOrVal dReg sAddr =
            match sAddr with
            | Reg _
            | Imm _
            | IncDfr SP ->
                this.moveVal dReg sAddr
            | _ ->
                this.moveRef dReg sAddr


        member this.moveValToMem symbol sAddr =
            let destMem = Abs (Expr_Sym symbol)

            match sAddr with
            | IncDfr _
            | DecDfr _
            | Dfr    _
            | Rel    _
            | Abs    _ ->
                (moveReferredVal utilReg sAddr
                   +!!+ movText destMem (Reg utilReg),
                 destMem)
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                (moveReferredVal utilReg sAddr
                   +!!+ movText (Reg utilReg) (dfr utilReg)
                   +!!+ movText destMem (Reg utilReg),
                 destMem)
            | Reg _
            | Imm _ ->
                (movText destMem sAddr,
                 destMem)


        member this.pushVal sAddr =
            let pushText (a:addr) = "push " + a.text

            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sAddr
                if sReg.isMemoryAccessible then
                    pushText (dfr sReg)
                      +!!+ incText sReg sReg incNum
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg incNum
                      +!!+ pushText (dfr utilReg)
            | DecDfr sReg ->
                let decNum = -(incDfrNum sAddr)
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
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                moveReferredVal utilReg sAddr
                  +!!+ pushText (dfr utilReg)
            | Reg _
            | Rel _
            | Abs _ ->
                pushText sAddr
            | Imm _ ->
                movText (Reg utilReg) sAddr
                  +!!+ pushText (Reg utilReg)


        member this.popValTo dAddr =
            let popText (a:addr) = "pop " + a.text

            match dAddr with
            | IncDfr dReg ->
                let incNum = incDfrNum dAddr
                if dReg.isMemoryAccessible then
                    popText (dfr dReg)
                      +!!+ incText dReg dReg incNum
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg incNum
                      +!!+ popText (dfr utilReg)
            | DecDfr dReg ->
                let decNum = -(incDfrNum dAddr)
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
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                moveReferredVal utilReg dAddr
                  +!!+ popText (dfr utilReg)
            | Reg _
            | Rel _
            | Abs _ ->
                popText dAddr
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
                let incNum = incDfrNum sAddr
                codeText dAddr (dfr sReg)
                  +!!+ incText sReg sReg incNum
            | DecDfr sReg ->
                let decNum = -(incDfrNum sAddr)
                incText sReg sReg decNum
                  +!!+ codeText dAddr (dfr sReg)
            | _ ->
                match dAddr with
                | IncDfr dReg ->
                    let incNum = incDfrNum dAddr
                    codeText (dfr dReg) sAddr
                      +!!+ incText dReg dReg incNum
                | DecDfr dReg ->
                    let decNum = -(incDfrNum dAddr)
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
                let incNum = incDfrNum addr
                codeText (dfr reg)
                  +!!+ incText reg reg incNum
            | DecDfr reg ->
                let decNum = -(incDfrNum addr)
                incText reg reg decNum
                  +!!+ codeText (dfr reg)
            | _ ->
                codeText addr


        member this.resolveIncDec addr =
            match addr with
            | IncDfr reg ->
                let incNum = incDfrNum addr
                (movText (Reg utilReg) (Reg reg)
                   +!!+ incText reg reg incNum,
                 dfr utilReg)
            | DecDfr reg ->
                let decNum = -(incDfrNum addr)
                (incText reg reg decNum,
                 dfr reg)
            | _ ->
                ("", addr)


        member this.storeRegVal reg =
            movText (Abs (Expr_Sym tempMem)) (Reg reg)

        member this.restoreRegVal reg =
            movText (Reg reg) (Abs (Expr_Sym tempMem))

        member this.invert (addr:addr) =
            "not " + addr.text

        member this.signExtend =
            "cbw"

        member this.exchangeVal (dAddr:addr) (sAddr:addr) =
            "xchg " + dAddr.text + ", " + sAddr.text

        member this.systemCall expr =
            "int 7" +!!+ Pseudo.data1 [expr]

        member this.shftLeftOrShiftRight (dAddr:addr) =
            let label1 = uniqName "ash"
            let label2 = label1 + "e"
            "testb cl, #0x20"            +!!+
            "jne " + label1              +!!+  // jne .+11; nop
            "andb cl, #0x1f"             +!!+
            "sal " + dAddr.text + ", cl" +!!+
            "jmp " + label2              +!!+  // jmp .+10; nop
            nameLabel label1             +!!+
            "orb cl, #0xc0"              +!!+
            "negb cl"                    +!!+
            "sar " + dAddr.text + ", cl" +!!+
            nameLabel label2


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

    let moveRefOrVal = asm.moveRefOrVal

    let moveValToMem = asm.moveValToMem

    let pushVal = asm.pushVal

    let popValTo = asm.popValTo

    let incrementReg = asm.incrementReg

    let binaryCalc = asm.binaryCalc

    let unaryCalc = asm.unaryCalc

    let resolveIncDec = asm.resolveIncDec

    let storeRegVal = asm.storeRegVal

    let restoreRegVal = asm.restoreRegVal

    let invert = asm.invert

    let signExtend = asm.signExtend

    let exchangeVal = asm.exchangeVal

    let systemCall = asm.systemCall

    let shftLeftOrShiftRight = asm.shftLeftOrShiftRight

    let storePCtoRegAndJmpToDest = asm.storePCtoRegAndJmpToDest


module ByteInstructionAsm =

    open InstructionAsm

    let tempMem = tempMem

    let inline (+!!+) i1 i2 = (+!!+) i1 i2


    let private asm = instructionAsm (itype = Byte)

    let moveRef = asm.moveRef

    let moveVal = asm.moveVal

    let moveRefOrVal = asm.moveRefOrVal

    let moveValToMem = asm.moveValToMem

    let pushVal = asm.pushVal

    let popValTo = asm.popValTo

    let incrementReg = asm.incrementReg

    let binaryCalc = asm.binaryCalc

    let unaryCalc = asm.unaryCalc

    let resolveIncDec = asm.resolveIncDec

    let storeRegVal = asm.storeRegVal

    let restoreRegVal = asm.restoreRegVal

    let invert = asm.invert

    let signExtend = asm.signExtend

    let exchangeVal = asm.exchangeVal

    let systemCall = asm.systemCall

    let shftLeftOrShiftRight = asm.shftLeftOrShiftRight

    let storePCtoRegAndJmpToDest = asm.storePCtoRegAndJmpToDest

