namespace Ack_i86

open Address
open TransState
open ExpressionType
open Eos
open Label
open ConditionCode

module InstructionAsm =

    let tempMem = "tmpMem"

    let inline (+!!+) (i1:string) (i2:string) =
        if i1.Length <> 0 && i2.Length <> 0 then
            i1 + (eos ';') + i2
        else
            i1 + i2


    let private movText (a1:addr) (a2:addr) =
        "mov " + a1.text + ", " + a2.text

    let private leaText (a1:addr) (a2:addr) =
        "lea " + a1.text + ", " + a2.text

    let private incDfrNum addr state =
        match addr with
        | IncDDfr _
        | DecDDfr _
        | DDfr _    -> 2
        | a when a.isUsing SP
                    -> 2
        | _  ->
            match state.iType with
            | Word  -> 2
            | Byte  -> 1

    let private incText r1 r2 num =
        leaText (Reg r1) (idfr r2 num)


    let private moveReferredVal (dReg: reg) ref state =
        let midReg =
            if dReg.isMemoryAccessible then dReg else utilReg
        let dest = Reg dReg

        match ref with
        | IncDfr  sReg
        | IncDDfr sReg ->
            let incNum = incDfrNum ref state
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
            let decNum = -(incDfrNum ref state)
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


    let private flagBit (flag: condFlag) =
        Array.fold
            (fun acc (f, bit) ->
                 if flag.HasFlag f then
                     acc ||| bit
                 else
                     acc)
            0
            [|condFlag.Carry, 0x1; condFlag.Overflow, 0x800;
              condFlag.Zero, 0x40; condFlag.Negative, 0x80|]


    let private addressFromArg arg state =
        match arg with
        | ArgSrc       -> state.srcAddress
        | ArgDest      -> state.destAddress
        | ArgTempReg   -> Reg state.tempReg.Value
        | ArgTempMem   -> Abs (Expr_Sym tempMem)
        | ArgAddr addr -> addr
        | ArgReg reg   -> Reg reg


    let moveRef (dReg: reg) sArg state =
        let sAddr = addressFromArg sArg state
        let midReg =
            if dReg.isMemoryAccessible then dReg else utilReg

        let code, ref =
            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sAddr state
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
                let decNum = -(incDfrNum sAddr state)
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
                (moveReferredVal dReg sAddr state,
                 dfr dReg)
            | _ ->
                failwithf "Invalid address"

        let state' =
            match sArg with
            | ArgSrc  -> {state with srcAddress = ref}
            | ArgDest -> {state with destAddress = ref;
                                     destValue = ref}
            | _       -> state

        code, state'


    let moveVal (dReg: reg) sArg state =
        let sAddr = addressFromArg sArg state
        let midReg =
            if dReg.isMemoryAccessible then dReg else utilReg

        let code =
            match sAddr with
            | IncDfr _
            | DecDfr _
            | Dfr    _
            | Reg    _
            | Rel    _
            | Abs    _
            | Imm    _ ->
                moveReferredVal dReg sAddr state
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                moveReferredVal midReg sAddr state
                  +!!+ movText (Reg dReg) (dfr midReg)

        let state' =
            match sArg with
            | ArgSrc  -> {state with srcAddress = Reg dReg}
            | ArgDest -> {state with destAddress = Reg dReg;
                                     destValue = Reg dReg}
            | _       -> state

        code, state'


    let moveRefOrVal dReg sArg state =
        let sAddr = addressFromArg sArg state
        match sAddr with
        | Reg _
        | Imm _
        | IncDfr SP ->
            moveVal dReg sArg state
        | _ ->
            moveRef dReg sArg state


    let moveValToMem symbol sArg state =
        let sAddr = addressFromArg sArg state
        let destMem = Abs (Expr_Sym symbol)

        let code =
            match sAddr with
            | IncDfr _
            | DecDfr _
            | Dfr    _
            | Rel    _
            | Abs    _ ->
                moveReferredVal utilReg sAddr state
                  +!!+ movText destMem (Reg utilReg)
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                moveReferredVal utilReg sAddr state
                  +!!+ movText (Reg utilReg) (dfr utilReg)
                  +!!+ movText destMem (Reg utilReg)
            | Reg _
            | Imm _ ->
                movText destMem sAddr

        let state' =
            match sArg with
            | ArgSrc  -> {state with srcAddress = destMem}
            | ArgDest -> {state with destAddress = destMem;
                                     destValue = destMem}
            | _       -> state

        code, state'


    let pushVal sArg state =
        let sAddr = addressFromArg sArg state
        let pushText (a:addr) = "push " + a.text

        let code, latestSrc =
            match sAddr with
            | IncDfr sReg ->
                let incNum = incDfrNum sAddr state
                if sReg.isMemoryAccessible then
                    pushText (dfr sReg)
                      +!!+ incText sReg sReg incNum,
                    idfr sReg -incNum
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg incNum
                      +!!+ pushText (dfr utilReg),
                    dfr utilReg
            | DecDfr sReg ->
                let decNum = -(incDfrNum sAddr state)
                if sReg.isMemoryAccessible then
                    incText sReg sReg decNum
                      +!!+ pushText (dfr sReg),
                    dfr sReg
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ incText sReg utilReg decNum
                      +!!+ pushText (idfr utilReg decNum),
                    idfr utilReg decNum
            | Dfr (sReg, expr) ->
                if sReg.isMemoryAccessible then
                    pushText sAddr,
                    sAddr
                else
                    movText (Reg utilReg) (Reg sReg)
                      +!!+ pushText (Dfr (utilReg, expr)),
                    Dfr (utilReg, expr)
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                moveReferredVal utilReg sAddr state
                  +!!+ pushText (dfr utilReg),
                dfr utilReg
            | Reg _
            | Rel _
            | Abs _ ->
                pushText sAddr,
                sAddr
            | Imm _ ->
                movText (Reg utilReg) sAddr
                  +!!+ pushText (Reg utilReg),
                Reg utilReg

        let state' =
            let src' =
                if sArg = ArgSrc
                        || state.srcAddress = DecDfr SP then
                    dfr SP
                else
                    state.srcAddress

            let dest', destVal' =
                if sArg = ArgDest
                        || state.destAddress = DecDfr SP then
                    dfr SP,
                    latestSrc
                else
                    state.destAddress,
                    state.destValue

            {state with srcAddress = src';
                        destAddress = dest';
                        destValue = destVal'}

        code, state'


    let popValTo dArg state =
        let dAddr = addressFromArg dArg state
        let popText (a:addr) = "pop " + a.text

        let code, latestDest =
            match dAddr with
            | IncDfr dReg ->
                let incNum = incDfrNum dAddr state
                if dReg.isMemoryAccessible then
                    popText (dfr dReg)
                      +!!+ incText dReg dReg incNum,
                    idfr dReg -incNum
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg incNum
                      +!!+ popText (dfr utilReg),
                    dfr utilReg
            | DecDfr dReg ->
                let decNum = -(incDfrNum dAddr state)
                if dReg.isMemoryAccessible then
                    incText dReg dReg decNum
                      +!!+ popText (dfr dReg),
                    dfr dReg
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ incText dReg utilReg decNum
                      +!!+ popText (idfr utilReg decNum),
                    idfr utilReg decNum
            | Dfr (dReg, expr) ->
                if dReg.isMemoryAccessible then
                    popText dAddr,
                    dAddr
                else
                    movText (Reg utilReg) (Reg dReg)
                      +!!+ popText (Dfr (utilReg, expr)),
                    Dfr (utilReg, expr)
            | IncDDfr _
            | DecDDfr _
            | DDfr    _
            | RelDfr  _ ->
                moveReferredVal utilReg dAddr state
                  +!!+ popText (dfr utilReg),
                dfr utilReg
            | Reg _
            | Rel _
            | Abs _ ->
                popText dAddr,
                dAddr
            | _ ->
                failwithf "Invalid address: pop to %A" dAddr

        let state' =
            let src' =
                if state.srcAddress = IncDfr SP then
                    dAddr
                else
                    state.srcAddress

            let dest', destVal' =
                if dArg = ArgDest then
                    state.destAddress,
                    latestDest
                elif state.destAddress = dfr SP then
                    DecDfr SP,
                    state.destValue
                else
                    state.destAddress,
                    state.destValue

            {state with srcAddress = src';
                        destAddress = dest';
                        destValue = destVal'}

        code, state'


    let incrementReg (reg: reg) num state =
        let code =
            if reg.isMemoryAccessible then
                leaText (Reg reg) (idfr reg num)
            else
                movText (Reg utilReg) (Reg reg)
                  +!!+ leaText (Reg reg) (idfr utilReg num)

        code, state


    let binaryCalc codeStr dArg sArg state =
        let dAddr = addressFromArg dArg state
        let sAddr = addressFromArg sArg state
        let codeText (a1:addr) (a2:addr) =
            match state.iType with
            | Word ->
                codeStr + " " + a1.text + ", " + a2.text
            | Byte ->
                codeStr + " " + a1.byteText + ", " + a2.byteText

        match sAddr with
        | IncDfr sReg ->
            let incNum = incDfrNum sAddr state
            codeText dAddr (dfr sReg),

            {state with postProcess = incrementReg sReg incNum :: state.postProcess;
                        srcAddress = dfr sReg}
        | DecDfr sReg ->
            let decNum = -(incDfrNum sAddr state)
            incText sReg sReg decNum
              +!!+ codeText dAddr (dfr sReg),
            {state with srcAddress = dfr sReg}
        | _ ->
            match dAddr with
            | IncDfr dReg ->
                let incNum = incDfrNum dAddr state
                codeText (dfr dReg) sAddr,
                {state with postProcess = incrementReg dReg incNum :: state.postProcess;
                            destAddress = dfr dReg;
                            destValue = dfr dReg}
            | DecDfr dReg ->
                let decNum = -(incDfrNum dAddr state)
                incText dReg dReg decNum
                  +!!+ codeText (dfr dReg) sAddr,
                {state with destAddress = dfr dReg;
                            destValue = dfr dReg}
            | _ ->
                codeText dAddr sAddr, state


    let unaryCalc codeStr sArg state =
        let addr = addressFromArg sArg state
        let codeText (a:addr) =
            match state.iType with
            | Word ->
                codeStr + " " + a.text
            | Byte ->
                codeStr + " " + a.byteText

        match addr with
        | IncDfr reg ->
            let incNum = incDfrNum addr state
            codeText (dfr reg),
            {state with postProcess = incrementReg reg incNum :: state.postProcess;
                        srcAddress = dfr reg}
        | DecDfr reg ->
            let decNum = -(incDfrNum addr state)
            incText reg reg decNum
              +!!+ codeText (dfr reg),
            {state with srcAddress = dfr reg}
        | _ ->
            codeText addr, state


    let resolveIncDec arg state =
        let addr = addressFromArg arg state

        let code, addr' =
            match addr with
            | IncDfr reg ->
                let incNum = incDfrNum addr state
                (movText (Reg utilReg) (Reg reg)
                   +!!+ incText reg reg incNum,
                 dfr utilReg)
            | DecDfr reg ->
                let decNum = -(incDfrNum addr state)
                (incText reg reg decNum,
                 dfr reg)
            | _ ->
                ("", addr)

        let state' =
            match arg with
            | ArgSrc  -> {state with srcAddress = addr'}
            | ArgDest -> {state with destAddress = addr';
                                     destValue = addr'}
            | _       -> state

        code, state'


    let storeTempReg state =
        let tmpReg = Reg state.tempReg.Value
        movText (Abs (Expr_Sym tempMem)) tmpReg, state

    let restoreTempReg state =
        let tmpReg = Reg state.tempReg.Value
        movText tmpReg (Abs (Expr_Sym tempMem)), state


    let invert arg state =
        let addr = addressFromArg arg state
        "not " + addr.text, state


    let signExtend state =
        "cbw", state


    let exchangeVal dArg sArg state =
        let sAddr = addressFromArg sArg state
        let dAddr = addressFromArg dArg state
        let code =
            "xchg " + dAddr.text + ", " + sAddr.text

        let state' =
            let (|SwappedSrc|) (a1, a2, st) =
                match a1, a2 with
                | Reg r1, Reg r2 when st.srcAddress.isUsing r1 ->
                    swapReg st.srcAddress r2
                | Reg r1, Reg r2 when st.srcAddress.isUsing r2 ->
                    swapReg st.srcAddress r1
                | _ ->
                    st.srcAddress
            let (|SwappedDest|) (a1, a2, st) =
                match a1, a2 with
                | Reg r1, Reg r2 when st.destAddress.isUsing r1 ->
                    swapReg st.destAddress r2
                | Reg r1, Reg r2 when st.destAddress.isUsing r2 ->
                    swapReg st.destAddress r1
                | _ ->
                    st.destAddress

            let src' =
                if sArg = ArgSrc then
                    dAddr
                elif dArg = ArgSrc then
                    sAddr
                else
                    match sAddr, dAddr, state with
                    | SwappedSrc a -> a

            let dest' =
                if sArg = ArgDest then
                    dAddr
                elif dArg = ArgDest then
                    sAddr
                else
                    match sAddr, dAddr, state with
                    | SwappedDest a -> a

            {state with srcAddress = src';
                        destAddress = dest';
                        destValue = dest'}

        code, state'


    let systemCall expr state =
        "int 7" +!!+ Pseudo.data1 [expr], state


    let clearCarryFlag state =
        "clc", state


    let setCarryFlag state =
        "stc", state


    let loadFlag state =
        "pushf", state


    let saveFlag state=
        "popf", state


    let clearFlagBit dArg flag state =
        let maskBit = int16 ~~~(flagBit flag)
        let maskImm = Imm (Expr_Oct maskBit)
        binaryCalc "and" dArg (ArgAddr maskImm) state


    let setFlagBit dArg flag state =
        let setBit = int16 (flagBit flag)
        let setImm = Imm (Expr_Oct setBit)
        binaryCalc "or" dArg (ArgAddr setImm) state


    let shiftLeftOrRight dArg state =
        let dAddr = addressFromArg dArg state
        let label1 = uniqName "ash"
        let label2 = label1 + "e"

        let code =
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

        code, state


    let shiftLeftOrRight32bit uArg lArg state =
        let upper = addressFromArg uArg state
        let lower = addressFromArg lArg state
        let label1 = uniqName "ashc"
        let label2 = label1 + "e"
        let lShiftLabel = uniqName "lsft"
        let rShiftLabel = uniqName "rsft"

        let code =
            "testb cl, #0x20"            +!!+
            "jne " + label1              +!!+
            "and cx, #0x1f"              +!!+
            "je " + label2               +!!+
            nameLabel lShiftLabel        +!!+
            "sal " + lower.text + ", #1" +!!+
            "rcl " + upper.text + ", #1" +!!+
            "loop " + lShiftLabel        +!!+
            "jmp " + label2              +!!+
            nameLabel label1             +!!+
            "or cx, #0xffc0"             +!!+
            "neg cx"                     +!!+
            nameLabel rShiftLabel        +!!+
            "sar " + upper.text + ", #1" +!!+
            "rcr " + lower.text + ", #1" +!!+
            "loop " + rShiftLabel        +!!+
            nameLabel label2

        code, state


    let storePCtoRegAndJmpToDest (reg: reg) dArg state =
        let dAddr = addressFromArg dArg state
        let label1 = uniqName "jsr"
        let label2 = label1 + "e"

        let code =
            "call " + label1                  +!!+
            nameLabel label1                  +!!+
            "pop " + reg.text                 +!!+
            "add " + reg.text + ", "
              + "#" + label2 + " - " + label1 +!!+
            "jmp " + dAddr.text               +!!+
            nameLabel label2

        code, state


    let setLabel (name: string) state =
        nameLabel name, state


    let divConditionCheck (dReg: reg) sArg state =
        let sAddr = addressFromArg sArg state
        let label1 = uniqName "div"
        let label2 = label1 + "e"
        let destT = dReg.text
        let buf =
            if sAddr <> Reg utilReg then
                Reg utilReg
            else
                Abs (Expr_Sym tempMem)
        let bufT = buf.text

        let code =
            movText buf sAddr            +!!+
            "xor " + bufT + ", " + destT +!!+
            movText buf sAddr            +!!+
            "jns " + label1              +!!+
            "neg " + bufT                +!!+
            nameLabel label1             +!!+
            "sub " + bufT + ", " + destT +!!+
            "xor " + bufT + ", " + destT +!!+
            "jle " + label2

        code,
        {state with postProcess = setLabel label2 :: state.postProcess}


    let fillWithNFlag arg state =
        let addr = addressFromArg arg state
        let label = uniqName "sxt"

        let code =
            "and " + addr.text + ", #0"     +!!+
            "jns " + label                  +!!+
            "or " + addr.text + ", #0xffff" +!!+
            nameLabel label

        code, state


    let putRet state =
        "ret", state


    let updateFlagsWithoutCarry arg state =
        let addr = addressFromArg arg state
        let label1 = uniqName "flag"
        let label2 = label1 + "e"
        let ArgLabel1 = ArgAddr (Abs (Expr_Sym label1))
        let ArgLabel2 = ArgAddr (Abs (Expr_Sym label2))

        let testDest state =
            let destVal = ArgAddr state.destValue
            match state.iType with
            | Word ->
                let testImm = Imm (Expr_Oct 0xffffs)
                binaryCalc "test" destVal (ArgAddr testImm) state
            | Byte ->
                let testImm = Imm (Expr_Oct 0xffs)
                binaryCalc "testb" destVal (ArgAddr testImm) state

        let processes = [unaryCalc "jc" ArgLabel1;
                        testDest;
                        clearCarryFlag;
                        unaryCalc "jmp" ArgLabel2;
                        setLabel label1;
                        testDest;
                        setCarryFlag;
                        setLabel label2]

        let codeList, state' =
            List.fold (fun (codes, s) x -> let (code, s') = x s
                                           code :: codes, s')
                      ([], state)
                      processes

        String.concat ";  " (List.rev codeList), state'

