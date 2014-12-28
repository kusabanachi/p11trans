namespace Ack_i86

open Address
open V6as.Expres
open InstructionAsm
open TransStatus
open ExpressionType
open ConditionCode

module Instruction =

    let private findFreeReg (a1: addr) (a2: addr) =
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

        let pre, post, tmpReg =
            if dest.isAccessible then
                let pre =
                    if not dest.isMemory && not src.isAccessible then
                        [moveRefOrVal utilReg ArgSrc;
                         binaryCalc code ArgDest ArgSrc]
                    elif dest.isMemory && src.isMemory
                            || destAddrAffectSrcVal (src, dest) then
                        [moveVal utilReg ArgSrc;
                         binaryCalc code ArgDest ArgSrc]
                    else
                        [binaryCalc code ArgDest ArgSrc]
                pre, [], None
            elif not src.isMemory
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

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = post;
                      tempReg = tmpReg;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


    let movType code i_dest i_src =
        let dest = i86Addr i_dest
        let src = i86Addr i_src

        if dest = DecDfr SP then
            let status = {iType = Word;
                          preProcess = [pushVal ArgSrc];
                          postProcess = [];
                          tempReg = None;
                          srcAddress = src;
                          destAddress = dest}
            extractCodeText status
        elif src = IncDfr SP && not (dest.isUsing SP) then
            let status = {iType = Word;
                          preProcess = [popValTo ArgDest];
                          postProcess = [];
                          tempReg = None;
                          srcAddress = src;
                          destAddress = dest}
            extractCodeText status
        elif dest = dfr SP && not src.isImmediate then
            if not (src.isUsing SP) then
                let status = {iType = Word;
                              preProcess = [popValTo (ArgReg utilReg);
                                            pushVal ArgSrc];
                              postProcess = [];
                              tempReg = None;
                              srcAddress = src;
                              destAddress = dest}
                extractCodeText status
            else
                let status = {iType = Word;
                              preProcess = [moveVal utilReg ArgSrc;
                                            popValTo ArgTempMem;
                                            pushVal ArgSrc];
                              postProcess = [];
                              tempReg = None;
                              srcAddress = src;
                              destAddress = dest}
                extractCodeText status
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

            let pre =
                if not src.isAccessible then
                    [moveRefOrVal utilReg ArgSrc;
                     binaryCalc code ArgDest ArgSrc]
                else
                    [binaryCalc code ArgDest ArgSrc]

            let status = {iType = Word;
                          preProcess = pre
                          postProcess = [];
                          tempReg = None;
                          srcAddress = src;
                          destAddress = dest}
            extractCodeText status
        elif dest.isAccessible then
            addType code i_dest i_src
        elif not src.isMemory
                && not (destAddrAffectSrcVal (src, dest)) then
            addType code i_dest i_src
        elif not (dest.isUsing SP) then
            let status = {iType = Word;
                          preProcess = [pushVal ArgSrc;
                                        popValTo ArgDest];
                          postProcess = [];
                          tempReg = None;
                          srcAddress = src;
                          destAddress = dest}
            extractCodeText status
        else
            addType code i_dest i_src


    let cmpType code dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let binCalc =
            if code <> "cmp" then
                binaryCalc code ArgDest ArgSrc
            else
                binaryCalc code ArgSrc ArgDest
        let destIsAccessible =
            dest.isAccessible
                && not (dest.isImmediate && code <> "cmp")
        let srcIsAccessible =
            src.isAccessible
                && not (src.isImmediate && code = "cmp")

        let pre =
            if destIsAccessible then
                if not dest.isMemory && not srcIsAccessible then
                    [moveRefOrVal utilReg ArgSrc;
                     binCalc]
                elif dest.isMemory && src.isMemory
                        || not srcIsAccessible
                        || destAddrAffectSrcVal (src, dest) then
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

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


    let bicType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let pre, post, tmpReg =
            if dest.isAccessible then
                [moveVal utilReg ArgSrc;
                 invert ArgSrc;
                 binaryCalc "and" ArgDest ArgSrc],
                 [],
                 None
            else
                let tmpReg = findFreeReg src dest
                [storeTempReg;
                 moveVal tmpReg ArgSrc;
                 invert ArgSrc;
                 moveRef utilReg ArgDest;
                 binaryCalc "and" ArgDest ArgSrc],
                [restoreTempReg],
                Some tmpReg

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = post;
                      tempReg = tmpReg;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


    let incType code addr =
        let addr = i86Addr addr

        let pre =
            if not addr.isAccessible then
                [moveRef utilReg ArgDest;
                 unaryCalc code ArgDest]
            else
                [unaryCalc code ArgDest]

        let status = {iType = Word;
                      preProcess = pre
                      postProcess = [];
                      tempReg = None;
                      srcAddress = addr;
                      destAddress = addr}

        extractCodeText status


    let mulType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let pre, post =
            match dest with
            | Reg AX ->
                if src.isImmediate || not src.isAccessible then
                    [moveRefOrVal utilReg ArgSrc;
                     unaryCalc "imul" ArgSrc]
                else
                    [unaryCalc "imul" ArgSrc]
                ,
                [exchangeVal (ArgReg AX) (ArgReg DX)]
            | Reg DX ->
                let pre1, axSv =
                    if src.isImmediate then
                        [moveVal utilReg (ArgReg AX);
                         moveVal AX ArgDest;
                         moveVal DX ArgSrc],
                        Reg utilReg
                    elif not src.isAccessible || src.isUsing AX then
                        match src with
                        | Reg    AX
                        | DecDfr AX
                        | Dfr   (AX, _) ->
                            [moveRefOrVal utilReg ArgSrc;
                             moveVal AX ArgDest],
                            Reg utilReg
                        | _ ->
                            [moveRef utilReg ArgSrc;
                             moveValToMem tempMem (ArgReg AX);
                             moveVal AX ArgDest],
                            namedMem tempMem
                    else
                        [moveVal utilReg (ArgReg AX);
                         moveVal AX ArgDest],
                        Reg utilReg
                pre1 @ [unaryCalc "imul" ArgSrc],
                [exchangeVal (ArgReg DX) (ArgReg AX);
                 exchangeVal (ArgReg AX) (ArgAddr axSv)]
            | Reg dReg when dReg = CX || dReg = DI ->
                let nextR = nextReg dReg

                let pre1 =
                    if src = Reg nextR then
                        [exchangeVal (ArgReg DX) (ArgReg nextR);
                         exchangeVal (ArgReg AX) ArgDest]
                    elif (not (src.isAccessible)
                            || (src.isUsing dReg && src <> Reg dReg)
                            || src.isUsing nextR
                            || src.isImmediate)
                            && not (src.isUsing AX && (swapReg src dReg).isAccessible) then
                        [moveRefOrVal utilReg ArgSrc;
                         moveVal nextR (ArgReg DX);
                         exchangeVal (ArgReg AX) ArgDest]
                    else
                        [moveVal nextR (ArgReg DX);
                         exchangeVal (ArgReg AX) ArgDest]
                pre1 @ [unaryCalc "imul" ArgSrc],
                [exchangeVal (ArgReg dReg) (ArgReg DX);
                 exchangeVal (ArgReg nextR) (ArgReg AX);
                 exchangeVal (ArgReg AX) (ArgReg DX)]
            | Reg dReg (* SI or BP *) ->
                let pre1, dxSv =
                    if src.isImmediate then
                        [moveVal utilReg (ArgReg DX);
                         moveVal DX ArgSrc;
                         exchangeVal (ArgReg AX) ArgDest],
                        Reg utilReg
                    elif (not (src.isAccessible)
                            || (src.isUsing dReg && src <> Reg dReg))
                            &&  not (src.isUsing AX && (swapReg src dReg).isAccessible) then
                        match src with
                        | DecDfr DX
                        | Dfr   (DX, _) ->
                            [moveRef utilReg ArgSrc;
                             exchangeVal (ArgReg AX) ArgDest],
                            Reg utilReg
                        | _ ->
                            [moveRef utilReg ArgSrc;
                             moveValToMem tempMem (ArgReg DX);
                             exchangeVal (ArgReg AX) ArgDest],
                            namedMem tempMem
                    else
                        [moveVal utilReg (ArgReg DX);
                         exchangeVal (ArgReg AX) ArgDest],
                        Reg utilReg
                pre1 @ [unaryCalc "imul" ArgSrc],
                [exchangeVal (ArgReg dReg) (ArgReg AX);
                 moveVal DX (ArgAddr dxSv)]
            | _ ->
                failwithf "Invalid address"

        let status = {iType = Word;
                      preProcess = pre
                      postProcess = post
                      tempReg = None;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


    let divType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let conditionCheck =
            let dReg = dest.getRegister
            if not src.isAccessible
                    || src.isIncrement
                    || src.isDecrement then
                [moveVal utilReg ArgSrc;
                 divConditionCheck dReg ArgSrc]
            else
                [divConditionCheck dReg ArgSrc]

        let pre =
            match dest with
            | Reg AX ->
                let pre1 =
                    if src.isImmediate || not src.isAccessible then
                        [moveRefOrVal utilReg ArgSrc]
                    else
                        []
                conditionCheck @
                pre1 @
                [exchangeVal (ArgReg AX) (ArgReg DX);
                 unaryCalc "idiv" ArgSrc]
            | Reg dReg when dReg = CX || dReg = DI ->
                let nextR = nextReg dReg
                let pre1 =
                    if (src.isUsing AX
                            && not (swapReg src nextR).isAccessible)
                        || (src.isUsing dReg
                            && not (swapReg src DX).isAccessible)
                        || (src.isUsing DX
                            && not (swapReg src dReg).isAccessible)
                        || (src.isUsing nextR
                            && not (swapReg src AX).isAccessible)
                        || src.isImmediate
                        || not src.isAccessible then
                        [moveRefOrVal utilReg ArgSrc]
                    else
                        []
                conditionCheck @
                pre1 @
                [exchangeVal (ArgReg DX) (ArgReg dReg);
                 exchangeVal (ArgReg AX) (ArgReg nextR);
                 unaryCalc "idiv" ArgSrc;
                 exchangeVal (ArgReg dReg) (ArgReg AX);
                 exchangeVal (ArgReg nextR) (ArgReg DX);
                 exchangeVal (ArgReg AX) (ArgReg DX)]
            | _ ->
                []

        let status = {iType = Word;
                      preProcess = pre
                      postProcess = []
                      tempReg = None;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


    let ashType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let pre =
            if src <> Reg CX then
                [moveVal utilReg ArgSrc;
                 exchangeVal (ArgReg CX) ArgSrc;
                 shiftLeftOrRight ArgDest]
            elif dest = Reg CX then
                [moveVal utilReg ArgDest;
                 shiftLeftOrRight ArgDest]
            else
                [moveVal utilReg (ArgReg CX);
                 shiftLeftOrRight ArgDest]

        let post = [moveVal CX (ArgReg utilReg)]

        let status = {iType = Word;
                      preProcess = pre
                      postProcess = post
                      tempReg = None;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


    let ashcType dest src =
        let dest = i86Addr dest
        let src = i86Addr src

        let pre =
            let nextR =
                if dest = Reg DX then
                    utilReg
                else
                    nextReg dest.getRegister

            if src <> Reg CX then
                [moveVal utilReg ArgSrc;
                 exchangeVal (ArgReg CX) ArgSrc;
                 shiftLeftOrRight32bit ArgDest (ArgReg nextR)]
            elif dest = Reg CX then
                [moveVal utilReg ArgDest;
                 shiftLeftOrRight32bit ArgDest (ArgReg nextR)]
            else
                [moveVal utilReg (ArgReg CX);
                 shiftLeftOrRight32bit ArgDest (ArgReg nextR)]

        let post = [moveVal CX (ArgReg utilReg)]

        let status = {iType = Word;
                      preProcess = pre
                      postProcess = post
                      tempReg = None;
                      srcAddress = src;
                      destAddress = dest}

        extractCodeText status


    let sysType expr =
        let status = {iType = Word;
                      preProcess = [systemCall expr];
                      postProcess = [];
                      tempReg = None;
                      srcAddress = Imm expr;
                      destAddress = Imm expr}

        extractCodeText status


    let jsrType dest reg =
        match i86Addr reg with
        | Reg IP ->
            incType "call" dest
        | Reg reg ->
            let dest = i86Addr dest
            let pre =
                if not dest.isAccessible then
                    [moveRef utilReg ArgDest;
                     pushVal (ArgReg reg);
                     storePCtoRegAndJmpToDest reg ArgDest]
                else
                    [resolveIncDec ArgDest;
                     pushVal (ArgReg reg);
                     storePCtoRegAndJmpToDest reg ArgDest]

            let status = {iType = Word;
                          preProcess = pre;
                          postProcess = [];
                          tempReg = None;
                          srcAddress = Reg reg;
                          destAddress = dest}

            extractCodeText status
        | _ ->
            failwithf "Invalid address"


    let rtsType addr =
        let addr = i86Addr addr

        let pre =
            match addr with
            | Reg IP ->
                [putRet]
            | Reg reg ->
                [moveVal utilReg (ArgReg reg);
                 popValTo (ArgReg reg);
                 unaryCalc "jmp" (ArgAddr (dfr utilReg))]
            | _ ->
                failwithf "Invalid address"

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = addr;
                      destAddress = addr}

        extractCodeText status


    let sxtType addr =
        let addr = i86Addr addr

        let pre =
            if not addr.isAccessible
                    || addr.isIncrement
                    || addr.isDecrement then
                [moveRef utilReg ArgDest;
                 fillWithNFlag ArgDest]
            else
                [fillWithNFlag ArgDest]

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = addr;
                      destAddress = addr}

        extractCodeText status


    let markType expr =
        let twofoldExpr = Expr_Op ('*', expr, Expr_Oct 2s)
        let regR5 = V6as.Addres.Reg V6as.Addres.R5

        let pre =
            [binaryCalc "add" (ArgReg SP) (ArgAddr (Imm twofoldExpr));
             moveVal utilReg (ArgReg BP);
             popValTo (ArgReg BP);
             unaryCalc "jmp" (ArgAddr (dfr utilReg))] // rtsType regR5

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = Reg BP;
                      destAddress = Reg BP}

        extractCodeText status


    let sobType expr src =
        let src = i86Addr src
        let expr = i86Addr expr

        let pre =
            if src = Reg CX then
                [unaryCalc "loop" ArgDest]
            else
                [unaryCalc "dec" ArgSrc
                 unaryCalc "jne" ArgDest]

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = src;
                      destAddress = expr}

        extractCodeText status


    let flagClear flag =
        let pre =
            if flag = condFlag.Carry then
                [clearCarryFlag]
            else
                [loadFlag;
                 moveVal utilReg (ArgReg SP);
                 clearFlagBit (ArgAddr (dfr utilReg)) flag
                 saveFlag]

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = Reg SP;
                      destAddress = Reg SP}

        extractCodeText status


    let flagSet flag =
        let pre =
            if flag = condFlag.Carry then
                [setCarryFlag]
            else
                [loadFlag;
                 moveVal utilReg (ArgReg SP);
                 setFlagBit (ArgAddr (dfr utilReg)) flag
                 saveFlag]

        let status = {iType = Word;
                      preProcess = pre;
                      postProcess = [];
                      tempReg = None;
                      srcAddress = Reg SP;
                      destAddress = Reg SP}

        extractCodeText status


