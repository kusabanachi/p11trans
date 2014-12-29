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


    let andbType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

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

        let state = {iType = Byte;
                     preProcess = pre;
                     postProcess = post;
                     tempReg = tmpReg;
                     srcAddress = src;
                     destAddress = dest}

        extractCodeText state


    let movbType code i_dest i_src =
        let dest = i86Addr i_dest
        let src = i86Addr i_src

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
            let state = {iType = Byte;
                         preProcess = pre;
                         postProcess = [];
                         tempReg = None;
                         srcAddress = src;
                         destAddress = dest}
            extractCodeText state
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
            let state = {iType = Byte;
                         preProcess = pre;
                         postProcess = post;
                         tempReg = None;
                         srcAddress = src;
                         destAddress = dest}
            extractCodeText state
        | _ ->
            andbType code i_dest i_src


    let cmpbType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

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

        let state = {iType = Byte;
                     preProcess = pre;
                     postProcess = [];
                     tempReg = None;
                     srcAddress = src;
                     destAddress = dest}
        extractCodeText state


    let bicbType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

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

        let state = {iType = Byte;
                     preProcess = pre;
                     postProcess = post;
                     tempReg = tmpReg;
                     srcAddress = src;
                     destAddress = dest}

        extractCodeText state


    let incbType code addr =
        let addr = i86Addr addr

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

        let state = {iType = Byte;
                     preProcess = pre;
                     postProcess = post;
                     tempReg = None;
                     srcAddress = addr;
                     destAddress = addr}

        extractCodeText state



    let private immVal num = V6as.Addres.Imm (Expr_Oct (int16 num))

    let clrbCode addr = andbType "andb"  addr (immVal 0)
    let combCode addr = andbType "xorb"  addr (immVal 0xff)
    let incbCode addr = incbType "incb"  addr
    let decbCode addr = incbType "decb"  addr
    let negbCode addr = incbType "negb"  addr
    let adcbCode addr = andbType "adcb"  addr (immVal 0)
    let sbcbCode addr = andbType "sbbb"  addr (immVal 0)
    let rorbCode addr = andbType "rcrb"  addr (immVal 1)
    let rolbCode addr = andbType "rclb"  addr (immVal 1)
    let asrbCode addr = andbType "sarb"  addr (immVal 1)
    let aslbCode addr = andbType "salb"  addr (immVal 1)
    let tstbCode addr = cmpbType "testb" addr (immVal 0xff)

    let movbCode dest src = movbType "movb"  dest src
    let cmpbCode dest src = cmpbType "cmpb"  dest src
    let bitbCode dest src = cmpbType "testb" dest src
    let bicbCode dest src = bicbType         dest src
    let bisbCode dest src = andbType "orb"   dest src

