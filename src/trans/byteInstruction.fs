namespace Ack_i86

open Address
open InstructionAsm
open TransStatus
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

        let status = {iType = Byte;
                      preProcess = pre;
                      postProcess = post;
                      tempReg = tmpReg;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


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
            let status = {iType = Byte;
                          preProcess = pre;
                          postProcess = [];
                          tempReg = None;
                          srcAddress = src;
                          destAddress = dest}
            extractCodeText status
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
            let status = {iType = Byte;
                          preProcess = pre;
                          postProcess = post;
                          tempReg = None;
                          srcAddress = src;
                          destAddress = dest}
            extractCodeText status
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

        let status = {iType = Byte;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = src;
                      destAddress = dest}
        extractCodeText status


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

        let status = {iType = Byte;
                      preProcess = pre;
                      postProcess = post;
                      tempReg = tmpReg;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


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

        let status = {iType = Byte;
                      preProcess = pre;
                      postProcess = post;
                      tempReg = None;
                      srcAddress = addr;
                      destAddress = addr}

        extractCodeText status


