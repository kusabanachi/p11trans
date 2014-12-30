namespace Ack_i86

open Address
open InstructionAsm
open TransState
open Instruction
open ExpressionType

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


    let andbType code state =
        let dest = state.destAddress
        let src = state.srcAddress

        let pre, post, tmpReg =
            if dest.isByteAccessible then
                if not dest.isMemory && not src.isByteAccessible then
                    [moveRefOrVal utilReg ArgSrc;
                     binaryCalc code ArgDest ArgSrc],
                    [],
                    None
                elif dest.isMemory && src.isMemory
                        || not src.isByteAccessible then
                    [moveVal utilReg ArgSrc;
                     binaryCalc code ArgDest ArgSrc],
                    [],
                    None
                else
                    [binaryCalc code ArgDest ArgSrc],
                    [],
                    None
            elif not dest.isMemory then
                let dReg = dest.getRegister
                if src.isByteAccessible
                        && not (srcAddrAffectDestVal (src, dest)) then
                    [moveVal utilReg ArgDest;
                     binaryCalc code ArgDest ArgSrc],
                    [moveVal dReg ArgDest],
                    None
                else
                    [moveValToMem tempMem ArgSrc;
                     moveVal utilReg ArgDest;
                     binaryCalc code ArgDest ArgSrc],
                    [moveVal dReg ArgDest],
                    None
            elif src.isByteAccessible
                    && not src.isMemory
                    && not (destAddrAffectSrcVal (src, dest)) then
                [moveRef utilReg ArgDest;
                 binaryCalc code ArgDest ArgSrc],
                [],
                None
            else
                let tmpReg = findFreeReg src dest
                [storeTempReg;
                 moveVal tmpReg ArgSrc;
                 moveRef utilReg ArgDest;
                 binaryCalc code ArgDest ArgSrc],
                [restoreTempReg],
                Some tmpReg

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess;
                    tempReg = tmpReg}


    let movbType code state =
        let dest = state.destAddress
        let src = state.srcAddress

        match dest with
        | Reg AX ->
            let pre =
                if src.isByteAccessible then
                    [binaryCalc code ArgDest ArgSrc;
                     signExtend]
                else
                    [moveRefOrVal utilReg ArgSrc;
                     binaryCalc code ArgDest ArgSrc;
                     signExtend]
            {state with preProcess = state.preProcess @ pre}
        | Reg dReg ->
            let pre1 =
                if not (src = Reg dReg
                        || src.isUsing AX
                             && (swapReg src dReg).isByteAccessible
                        || src.isByteAccessible
                             && not (src.isUsing dReg)
                             && not (src.isUsing AX)) then
                    [moveRefOrVal utilReg ArgSrc]
                else
                    []
            let pre = pre1 @
                      [exchangeVal (ArgReg AX) ArgDest;
                       binaryCalc code ArgDest ArgSrc;
                       signExtend]
            let post = [exchangeVal (ArgReg dReg) (ArgReg AX)]
            {state with preProcess = state.preProcess @ pre;
                        postProcess = post @ state.postProcess}
        | _ ->
            andbType code state


    let cmpbType code state =
        let dest = state.destAddress
        let src = state.srcAddress

        let binCalc =
            if code <> "cmpb" then
                binaryCalc code ArgDest ArgSrc
            else
                binaryCalc code ArgSrc ArgDest
        let destIsAccessible =
            dest.isByteAccessible
                && not (dest.isImmediate && code <> "cmpb")
        let srcIsAccessible =
            src.isByteAccessible
                && not (src.isImmediate && code = "cmpb")

        let pre =
            if destIsAccessible then
                if not dest.isMemory && not srcIsAccessible then
                    [moveRefOrVal utilReg ArgSrc;
                     binCalc]
                elif dest.isMemory && src.isMemory
                        || not srcIsAccessible then
                    [moveVal utilReg ArgSrc;
                     binCalc]
                else
                    [binCalc]
            elif srcIsAccessible
                    && not (srcAddrAffectDestVal (src, dest))
                    && not (destAddrAffectSrcVal (src, dest)) then
                if not src.isMemory then
                    [moveRefOrVal utilReg ArgDest;
                     binCalc]
                else
                    [moveVal utilReg ArgDest;
                     binCalc]
            else
                [moveValToMem tempMem ArgSrc;
                 moveVal utilReg ArgDest;
                 binCalc]

        {state with preProcess = state.preProcess @ pre}


    let bicbType state =
        let dest = state.destAddress
        let src = state.srcAddress

        let pre, post, tmpReg =
            if dest.isByteAccessible then
                [moveVal utilReg ArgSrc;
                 invert ArgSrc;
                 binaryCalc "andb" ArgDest ArgSrc],
                 [],
                 None
            elif not dest.isMemory then
                let dReg = dest.getRegister
                [moveValToMem tempMem ArgSrc;
                 invert ArgSrc;
                 moveVal utilReg ArgDest;
                 binaryCalc "andb" ArgDest ArgSrc],
                 [moveVal dReg ArgDest],
                 None
            else
                let tmpReg = findFreeReg src dest
                [storeTempReg
                 moveVal tmpReg ArgSrc;
                 invert ArgSrc;
                 moveRef utilReg ArgDest;
                 binaryCalc "andb" ArgDest ArgSrc],
                 [restoreTempReg],
                 Some tmpReg

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess;
                    tempReg = tmpReg}


    let incbType code state =
        let addr = state.destAddress

        let pre, post =
            if addr.isByteAccessible then
                [unaryCalc code ArgDest],
                []
            elif addr.isMemory then
                [moveRef utilReg ArgDest;
                 unaryCalc code ArgDest],
                []
            else
                let reg = addr.getRegister
                [moveVal utilReg ArgDest;
                 unaryCalc code ArgDest],
                [moveVal reg ArgDest]

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess}




    let private initState dest src =
        let dest' = i86Addr dest
        let src' = i86Addr src
        {iType = Byte;
         preProcess = [];
         postProcess = [];
         tempReg = None;
         srcAddress = src';
         destAddress = dest'}


    let private immVal num = V6as.Addres.Imm (Expr_Oct (int16 num))

    let clrbCode addr =
        let src = immVal 0
        andbType "andb" (initState addr src)
        |> extractCodeText
    let combCode addr =
        let src = immVal 0xff
        andbType "xorb" (initState addr src)
        |> extractCodeText
    let incbCode addr =
        incbType "incb" (initState addr addr)
        |> extractCodeText
    let decbCode addr =
        incbType "decb" (initState addr addr)
        |> extractCodeText
    let negbCode addr =
        incbType "negb" (initState addr addr)
        |> extractCodeText
    let adcbCode addr =
        let src = immVal 0
        andbType "adcb" (initState addr src)
        |> extractCodeText
    let sbcbCode addr =
        let src = immVal 0
        andbType "sbbb" (initState addr src)
        |> extractCodeText
    let rorbCode addr =
        let src = immVal 1
        andbType "rcrb" (initState addr src)
        |> extractCodeText
    let rolbCode addr =
        let src = immVal 1
        andbType "rclb" (initState addr src)
        |> extractCodeText
    let asrbCode addr =
        let src = immVal 1
        andbType "sarb" (initState addr src)
        |> extractCodeText
    let aslbCode addr =
        let src = immVal 1
        andbType "salb" (initState addr src)
        |> extractCodeText
    let tstbCode addr =
        let src = immVal 0xff
        cmpbType "testb" (initState addr src)
        |> extractCodeText

    let movbCode dest src =
        movbType "movb" (initState dest src)
        |> extractCodeText
    let cmpbCode dest src =
        cmpbType "cmpb" (initState dest src)
        |> extractCodeText
    let bitbCode dest src =
        cmpbType "testb" (initState dest src)
        |> extractCodeText
    let bicbCode dest src =
        bicbType (initState dest src)
        |> extractCodeText
    let bisbCode dest src =
        andbType "orb" (initState dest src)
        |> extractCodeText

