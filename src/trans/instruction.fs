namespace Ack_i86

open Address
open InstructionAsm
open TransState
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


    let addType code state =
        let dest = state.destAddress
        let src = state.srcAddress

        let pre, post, tmpReg =
            if dest.isAccessible then
                if not dest.isMemory && not src.isAccessible then
                    [moveRefOrVal utilReg ArgSrc;
                     binaryCalc code ArgDest ArgSrc],
                    [],
                    None
                elif dest.isMemory && src.isMemory
                        || destAddrAffectSrcVal (src, dest) then
                    [moveVal utilReg ArgSrc;
                     binaryCalc code ArgDest ArgSrc],
                    [],
                    None
                else
                    [binaryCalc code ArgDest ArgSrc],
                    [],
                    None
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

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess;
                    tempReg = tmpReg}


    let movType code state =
        let dest = state.destAddress
        let src = state.srcAddress

        if dest = DecDfr SP then
            let pre = [pushVal ArgSrc]
            {state with preProcess = state.preProcess @ pre}
        elif src = IncDfr SP && not (dest.isUsing SP) then
            let pre = [popValTo ArgDest]
            {state with preProcess = state.preProcess @ pre}
        elif dest = dfr SP && not src.isImmediate then
            let pre =
                if not (src.isUsing SP) then
                    [popValTo (ArgReg utilReg);
                     pushVal ArgSrc];
                else
                    [moveVal utilReg ArgSrc;
                     popValTo ArgTempMem;
                     pushVal ArgSrc];
            {state with preProcess = state.preProcess @ pre}
        elif not dest.isMemory then
            let src' =
                let dReg = dest.getRegister
                match src with
                | IncDfr sReg when sReg = dReg ->
                    dfr sReg
                | IncDDfr sReg when sReg = dReg ->
                    ddfr sReg
                | _ ->
                    src
            let pre =
                if not src'.isAccessible then
                    [moveRefOrVal utilReg ArgSrc;
                     binaryCalc code ArgDest ArgSrc]
                else
                    [binaryCalc code ArgDest ArgSrc]
            {state with preProcess = state.preProcess @ pre;
                        srcAddress = src'}
        elif dest.isAccessible then
            addType code state
        elif not src.isMemory
                && not (destAddrAffectSrcVal (src, dest)) then
            addType code state
        elif not (dest.isUsing SP) then
            let pre = [pushVal ArgSrc;
                       popValTo ArgDest];
            {state with preProcess = state.preProcess @ pre}
        else
            addType code state


    let cmpType code state =
        let dest = state.destAddress
        let src = state.srcAddress

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

        {state with preProcess = state.preProcess @ pre}


    let bicType state =
        let dest = state.destAddress
        let src = state.srcAddress

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

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess;
                    tempReg = tmpReg}


    let incType code state =
        let addr = state.destAddress

        let pre =
            if not addr.isAccessible then
                [moveRef utilReg ArgDest;
                 unaryCalc code ArgDest]
            else
                [unaryCalc code ArgDest]

        {state with preProcess = state.preProcess @ pre}


    let mulType state =
        let dest = state.destAddress
        let src = state.srcAddress

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

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess}


    let divType state =
        let dest = state.destAddress
        let src = state.srcAddress

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

        {state with preProcess = state.preProcess @ pre}


    let ashType state =
        let dest = state.destAddress
        let src = state.srcAddress

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

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess}


    let ashcType state =
        let dest = state.destAddress
        let src = state.srcAddress

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

        {state with preProcess = state.preProcess @ pre;
                    postProcess = post @ state.postProcess}


    let sysType expr state =
        let pre = [systemCall expr]
        {state with preProcess = state.preProcess @ pre}


    let jsrType state =
        match state.srcAddress with
        | Reg IP ->
            incType "call" state
        | Reg reg ->
            let dest = state.destAddress
            let pre =
                if not dest.isAccessible then
                    [moveRef utilReg ArgDest;
                     pushVal (ArgReg reg);
                     storePCtoRegAndJmpToDest reg ArgDest]
                else
                    [resolveIncDec ArgDest;
                     pushVal (ArgReg reg);
                     storePCtoRegAndJmpToDest reg ArgDest]

            {state with preProcess = state.preProcess @ pre}
        | _ ->
            failwithf "Invalid address"


    let rtsType state =
        let addr = state.destAddress

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

        {state with preProcess = state.preProcess @ pre}


    let sxtType state =
        let addr = state.destAddress

        let pre =
            if not addr.isAccessible
                    || addr.isIncrement
                    || addr.isDecrement then
                [moveRef utilReg ArgDest;
                 fillWithNFlag ArgDest]
            else
                [fillWithNFlag ArgDest]

        {state with preProcess = state.preProcess @ pre}


    let markType expr state =
        let twofoldExpr = Expr_Op ('*', expr, Expr_Oct 2s)

        let pre = [binaryCalc "add" (ArgReg SP) (ArgAddr (Imm twofoldExpr))]
        rtsType {state with preProcess = state.preProcess @ pre}


    let sobType state =
        let src = state.srcAddress

        let pre =
            if src = Reg CX then
                [unaryCalc "loop" ArgDest]
            else
                [unaryCalc "dec" ArgSrc
                 unaryCalc "jne" ArgDest]

        {state with preProcess = state.preProcess @ pre}


    let flagClearType flag state =
        let pre =
            if flag = condFlag.Carry then
                [clearCarryFlag]
            else
                [loadFlag;
                 moveVal utilReg (ArgReg SP);
                 clearFlagBit (ArgAddr (dfr utilReg)) flag
                 saveFlag]

        {state with preProcess = state.preProcess @ pre}


    let flagSetType flag state =
        let pre =
            if flag = condFlag.Carry then
                [setCarryFlag]
            else
                [loadFlag;
                 moveVal utilReg (ArgReg SP);
                 setFlagBit (ArgAddr (dfr utilReg)) flag
                 saveFlag]

        {state with preProcess = state.preProcess @ pre}




    let private initState dest src =
        let dest' = i86Addr dest
        let src' = i86Addr src
        {iType = Word;
         srcAddress = src';
         destAddress = dest';
         destValue = dest';
         tempReg = None;
         preProcess = [];
         midProcess = [];
         postProcess = []}


    let private immVal num = V6as.Addres.Imm (Expr_Oct (int16 num))

    let clrCode addr =
        let src = immVal 0
        addType "and" (initState addr src)
        |> extractCodeText
    let comCode addr =
        let src = immVal 0xffff
        addType "xor" (initState addr src)
        |> extractCodeText
    let incCode addr =
        incType "inc" (initState addr addr)
        |> extractCodeText
    let decCode addr =
        incType "dec" (initState addr addr)
        |> extractCodeText
    let negCode addr =
        incType "neg" (initState addr addr)
        |> extractCodeText
    let adcCode addr =
        let src = immVal 0
        addType "adc" (initState addr src)
        |> extractCodeText
    let sbcCode addr =
        let src = immVal 0
        addType "sbb" (initState addr src)
        |> extractCodeText
    let rorCode addr =
        let src = immVal 1
        addType "rcr" (initState addr src)
        |> extractCodeText
    let rolCode addr =
        let src = immVal 1
        addType "rcl" (initState addr src)
        |> extractCodeText
    let asrCode addr =
        let src = immVal 1
        addType "sar" (initState addr src)
        |> extractCodeText
    let aslCode addr =
        let src = immVal 1
        addType "sal" (initState addr src)
        |> extractCodeText
    let jmpCode addr =
        incType "jmp" (initState addr addr)
        |> extractCodeText
    let swabCode addr =
        let src = immVal 8
        addType "rol" (initState addr src)
        |> extractCodeText
    let tstCode addr =
        let src = immVal 0xffff
        cmpType "test" (initState addr src)
        |> extractCodeText
    let rtsCode addr =
        rtsType (initState addr addr)
        |> extractCodeText
    let sxtCode addr =
        sxtType (initState addr addr)
        |> extractCodeText

    let movCode dest src =
        let state =
            {initState dest src with
               midProcess = [updateFlagsWithoutCarry ArgDest]}
        movType "mov" state
        |> extractCodeText
    let cmpCode dest src =
        cmpType "cmp" (initState dest src)
        |> extractCodeText
    let bitCode dest src =
        cmpType "test" (initState dest src)
        |> extractCodeText
    let bicCode dest src =
        bicType (initState dest src)
        |> extractCodeText
    let bisCode dest src =
        addType "or" (initState dest src)
        |> extractCodeText
    let addCode dest src =
        addType "add" (initState dest src)
        |> extractCodeText
    let subCode dest src =
        addType "sub" (initState dest src)
        |> extractCodeText
    let jsrCode dest src =
        jsrType (initState dest src)
        |> extractCodeText
    let ashCode dest src =
        ashType (initState dest src)
        |> extractCodeText
    let ashcCode dest src =
        ashcType (initState dest src)
        |> extractCodeText
    let mulCode dest src =
        mulType (initState dest src)
        |> extractCodeText
    let divCode dest src =
        divType (initState dest src)
        |> extractCodeText
    let xorCode dest src =
        addType "xor" (initState dest src)
        |> extractCodeText
    let sobCode dest src =
        sobType (initState dest src)
        |> extractCodeText

    let brCode addr  =
        incType "jmp" (initState addr addr)
        |> extractCodeText
    let bneCode addr =
        incType "jne" (initState addr addr)
        |> extractCodeText
    let beqCode addr =
        incType "je" (initState addr addr)
        |> extractCodeText
    let bgeCode addr =
        incType "jge" (initState addr addr)
        |> extractCodeText
    let bltCode addr =
        incType "jl" (initState addr addr)
        |> extractCodeText
    let bgtCode addr =
        incType "jg" (initState addr addr)
        |> extractCodeText
    let bleCode addr =
        incType "jle" (initState addr addr)
        |> extractCodeText
    let bplCode addr =
        incType "jns" (initState addr addr)
        |> extractCodeText
    let bmiCode addr =
        incType "js" (initState addr addr)
        |> extractCodeText
    let bhiCode addr =
        incType "ja" (initState addr addr)
        |> extractCodeText
    let blosCode addr =
        incType "jbe" (initState addr addr)
        |> extractCodeText
    let bvcCode addr =
        incType "jno" (initState addr addr)
        |> extractCodeText
    let bvsCode addr =
        incType "jo" (initState addr addr)
        |> extractCodeText
    let bhisCode addr =
        incType "jae" (initState addr addr)
        |> extractCodeText
    let becCode addr =
        incType "jnc" (initState addr addr)
        |> extractCodeText
    let bloCode addr =
        incType "jb" (initState addr addr)
        |> extractCodeText
    let bcsCode addr =
        incType "jc" (initState addr addr)
        |> extractCodeText
    let jbrCode addr =
        incType "jmp" (initState addr addr)
        |> extractCodeText
    let jneCode addr =
        incType "jne" (initState addr addr)
        |> extractCodeText
    let jeqCode addr =
        incType "je" (initState addr addr)
        |> extractCodeText
    let jgeCode addr =
        incType "jge" (initState addr addr)
        |> extractCodeText
    let jltCode addr =
        incType "jl" (initState addr addr)
        |> extractCodeText
    let jgtCode addr =
        incType "jg" (initState addr addr)
        |> extractCodeText
    let jleCode addr =
        incType "jle" (initState addr addr)
        |> extractCodeText
    let jplCode addr =
        incType "jns" (initState addr addr)
        |> extractCodeText
    let jmiCode addr =
        incType "js" (initState addr addr)
        |> extractCodeText
    let jhiCode addr =
        incType "ja" (initState addr addr)
        |> extractCodeText
    let jlosCode addr =
        incType "jbe" (initState addr addr)
        |> extractCodeText
    let jvcCode addr =
        incType "jno" (initState addr addr)
        |> extractCodeText
    let jvsCode addr =
        incType "jo" (initState addr addr)
        |> extractCodeText
    let jhisCode addr =
        incType "jae" (initState addr addr)
        |> extractCodeText
    let jecCode addr =
        incType "jnc" (initState addr addr)
        |> extractCodeText
    let jloCode addr =
        incType "jb" (initState addr addr)
        |> extractCodeText
    let jcsCode addr =
        incType "jc" (initState addr addr)
        |> extractCodeText

    let sysCode expr =
        sysType expr {iType = Word;
                      srcAddress = Imm expr; destAddress = Imm expr;
                      destValue = Imm expr; tempReg = None;
                      preProcess = []; midProcess = [];
                      postProcess = []}
        |> extractCodeText
    let markCode expr =
        markType expr {iType = Word;
                       srcAddress = Reg BP; destAddress = Reg BP;
                       destValue = Reg BP; tempReg = None;
                       preProcess = []; midProcess = [];
                       postProcess = []}
        |> extractCodeText

    let flagClear flag =
        flagClearType flag {iType = Word;
                            srcAddress = Reg SP; destAddress = Reg SP;
                            destValue = Reg SP; tempReg = None;
                            preProcess = []; midProcess = [];
                            postProcess = []}
        |> extractCodeText
    let flagSet flag =
        flagSetType flag {iType = Word;
                          srcAddress = Reg SP; destAddress = Reg SP;
                          destValue = Reg SP; tempReg = None;
                          preProcess = []; midProcess = [];
                          postProcess = []}
        |> extractCodeText

