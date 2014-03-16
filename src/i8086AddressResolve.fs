module p11trans.i8086AddressResolve

(* resolve i8086's address operation for
   operand of instructions *)

open p11trans.i8086ProcedureStep
open p11trans.utility
open p11trans.intermediate


// return if the address is using the register or not.
// addr -> reg -> bool
let private isUsingTheReg addr theReg =
    match addr with
    | Register(reg)
    | IncDfr(reg,_) | DecDfr(reg,_) | IdxDfr(reg,_) | Dfr(reg)
    | IncDDfr(reg,_) | DecDDfr(reg,_) | IdxDDfr(reg,_) | DDfr(reg)
        when reg = theReg -> true
    | _ ->
        false


// get free register, which is not used by the two addresses.
// addr -> addr -> reg
let private getFreeReg addr1 addr2 =
    let isNotUsedReg reg =
        not (isUsingTheReg addr1 reg || isUsingTheReg addr2 reg)

    if isNotUsedReg R0 then
        R0
    elif isNotUsedReg R1 then
        R1
    else
        R2


// get next register of the argument.
// reg -> reg
let private getNextReg = function
    | R0 -> R1
    | R1 -> R2
    | R2 -> R3
    | R3 -> R4
    | R4 -> R5
    | _ ->
        failwithf "Invalid register."


// resolve i8086's two addresses operation.
module twoAddressResolve =

    // a case of that both src and dest are memory address
    // string -> addr -> addr -> procedureStep list
    let private getMemToMemProcedure code dest src =
        let (| DestIsAccessible
              | Other |) (dest, src) =
            if isAccessibleAddress dest then
                DestIsAccessible
            else
                Other

        match (dest, src) with
        | DestIsAccessible ->
            [
            [MoveSrcVal_toUtilReg]           |> incDecCheck (Src, src)
            [BinaryCalc(code, ODest, OSrc)]  |> incDecCheck (Dest, dest)
            ] |> List.concat
        | _ ->
            let freeReg = getFreeReg src dest
            [
            [StoreRegVal(freeReg)]
            [MoveSrcVal_toReg(freeReg)]      |> incDecCheck (Src, src)
            [MoveDestRef_toUtilReg]          |> incDecCheck (Dest, dest)
            [BinaryCalc(code, ODest, OSrc)]
            [RestoreRegVal(freeReg)]
            ] |> List.concat

    // get steps of procedure for i8086's two address operation.
    // string -> addr -> addr -> procedureStep list
    let private getProcedureImple code dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure code dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]          |> incDecCheck (Dest, dest)
            [BinaryCalc(code, ODest, OSrc)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
            [BinaryCalc(code, ODest, OSrc)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(code, ODest, OSrc)]  |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(code, ODest, OSrc)]  |> incDecCheck (Src, src)
        else
            [BinaryCalc(code, ODest, OSrc)]

    // entry of getting steps of procedure for i8086's two address operation.
    // string -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple code dest src
        Some(procedure)



// resolve i8086's one address operation.
module oneAddressResolve =

    // get steps of procedure for i8086's one address operation.
    // string -> addr -> procedureStep list option
    let getProcedure code dest =
        let procedure =
            if not (isAccessibleAddress dest) then
                [
                [MoveDestRef_toUtilReg]       |> incDecCheck (Dest, dest)
                [UnaryCalc(code, ODest)]
                ] |> List.concat
            else
                [UnaryCalc(code, ODest)]      |> incDecCheck (Dest, dest)
        Some(procedure)



// resolve i8086's move address operation.
module moveAddressResolve =

    // return if the address is sp register or not.
    // addr -> bool
    let private isNotSpAddr = function
        | Register(SP)
        | IncDfr(SP,_) | DecDfr(SP,_) | IdxDfr(SP,_) | Dfr(SP)
        | IncDDfr(SP,_) | DecDDfr(SP,_) | DDfr(SP) | IdxDDfr(SP,_)
            -> false
        | _ -> true

    // a case of that both src and dest are memory address
    // string -> addr -> addr -> procedureStep list
    let private getMemToMemProcedure code dest src =
        let (| DestIsAccessible
              | DestIsNotStack
               | Other |) (dest, src) =
            if isAccessibleAddress dest then
                DestIsAccessible
            elif isNotSpAddr dest then
                DestIsNotStack
            else
                Other

        match (dest, src) with
        | DestIsAccessible ->
            [
            [MoveSrcVal_toUtilReg]           |> incDecCheck (Src, src)
            [BinaryCalc(code, ODest, OSrc)]  |> incDecCheck (Dest, dest)
            ] |> List.concat
        | DestIsNotStack ->
            [
            [PushSrcVal]               |> incDecCheck (Src, src)
            [PopToDest]                |> incDecCheck (Dest, dest)
            ] |> List.concat
        | _ ->
            let freeReg = getFreeReg src dest
            [
            [StoreRegVal(freeReg)]
            [MoveSrcVal_toReg(freeReg)]  |> incDecCheck (Src, src)
            [MoveDestRef_toUtilReg]      |> incDecCheck (Dest, dest)
            [BinaryCalc(code, ODest, OSrc)]
            [RestoreRegVal(freeReg)]
            ] |> List.concat


    // get steps of procedure for i8086's move address operation.
    // string -> addr -> addr -> procedureStep list
    let private getNotStackProcedure code dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure code dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]          |> incDecCheck (Dest, dest)
            [BinaryCalc(code, ODest, OSrc)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
            [BinaryCalc(code, ODest, OSrc)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(code, ODest, OSrc)]  |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(code, ODest, OSrc)]  |> incDecCheck (Src, src)
        else
            [BinaryCalc(code, ODest, OSrc)]


    // entry of getting steps of procedure for i8086's move address operation.
    // apply push or pop steps, if it is available.
    // string -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure =
            match (dest, src) with
            | (DecDfr(SP, 2), _) ->
                [PushSrcVal]                 |> incDecCheck (Src, src)
            | (Dfr(SP), _) ->
                [
                [IncrementDestReg(2)]
                [PushSrcVal]                 |> incDecCheck (Src, src)
                ] |> List.concat
            | (_, IncDfr(SP, 2)) ->
                [PopToDest]                  |> incDecCheck (Dest, dest)
            | _ ->
                getNotStackProcedure code dest src
        Some(procedure)



// resolve i8086's two address operation, which does not store result.
module twoAddressResolveWithoutStoring =

    // a case of that both src and dest are memory address
    // string -> addr -> addr -> procedureStep list
    let private getMemToMemProcedure code dest src =
        let (| DestIsAccessible | SrcIsAccessible
              | Other |) (dest, src) =
            if isAccessibleAddress dest then
                DestIsAccessible
            elif isAccessibleAddress src then
                SrcIsAccessible
            else
                Other

        match (dest, src) with
        | DestIsAccessible ->
            [
            [MoveSrcVal_toUtilReg]           |> incDecCheck (Src, src)
            [BinaryCalc(code, OSrc, ODest)]  |> incDecCheck (Dest, dest)
            ] |> List.concat
        | SrcIsAccessible ->
            [
            [MoveDestVal_toUtilReg]          |> incDecCheck (Dest, dest)
            [BinaryCalc(code, OSrc, ODest)]  |> incDecCheck (Src, src)
            ] |> List.concat
        | _ ->
            [
            [MoveSrcVal_toTempMem]           |> incDecCheck (Src, src)
            [MoveDestVal_toUtilReg]          |> incDecCheck (Dest, dest)
            [BinaryCalc(code, OSrc, ODest)]
            ] |> List.concat


    // get steps of procedure for i8086's two address operation,
    // which doesn't store result.
    // string -> addr -> addr -> procedureStep list
    let private getProcedureImple code dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure code dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]          |> incDecCheck (Dest, dest)
            [BinaryCalc(code, OSrc, ODest)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
            [BinaryCalc(code, OSrc, ODest)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(code, OSrc, ODest)]  |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(code, OSrc, ODest)]  |> incDecCheck (Src, src)
        else
            [BinaryCalc(code, OSrc, ODest)]

    // entry of getting steps of procedure for i8086's two address operation,
    // which doesn't store result.
    // string -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple code dest src
        Some(procedure)



// resolve i8086's multiply operation's address.
module mulAddressResolve =

    // return if the address is a immediate value or not.
    // addr -> bool
    let private isImmediateValue = function
        | Imm(_) -> true
        | _ -> false

    // get steps of procedure for i8086's mul operation.
    // string -> addr -> reg -> procedureStep list option
    let getProcedure code src reg =
        let (| DestRegIsR0 | DestRegIsR1
              | DestRegIsEven | DestRegIsOdd |) (reg) =
            if reg = R0 then
                DestRegIsR0
            elif reg = R1 then
                DestRegIsR1
            elif reg = R0 || reg = R2 || reg = R4 then
                DestRegIsEven
            else
                DestRegIsOdd

        let procedure =
            match reg with
            | DestRegIsR0 ->
                if isImmediateValue src then
                    [
                    [MoveSrcVal_toUtilReg]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
                elif not (isAccessibleAddress src) then
                    [
                    [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
                else
                    [
                    [UnaryCalc(code, OSrc)]          |> incDecCheck (Src, src)
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
            | DestRegIsR1 ->
                if src = Register(R0) then
                    [
                    [MoveSrcVal_toUtilReg]
                    [BinaryCalc("mov", OAddr(Register(R0)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("mov", OAddr(Register(R1)), OAddr(Register(R0)))]
                    [BinaryCalc("mov", OAddr(Register(R0)), OAddr(Register(Util)))]
                    ] |> List.concat
                elif isImmediateValue src then
                    [
                    [MoveSrcVal_toUtilReg]
                    [StoreRegVal(R0)]
                    [BinaryCalc("mov", OAddr(Register(R0)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("mov", OAddr(Register(R1)), OAddr(Register(R0)))]
                    [RestoreRegVal(R0)]
                    ] |> List.concat
                elif not (isAccessibleAddress src) then
                    [
                    [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
                    [StoreRegVal(R0)]
                    [BinaryCalc("mov", OAddr(Register(R0)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("mov", OAddr(Register(R1)), OAddr(Register(R0)))]
                    [RestoreRegVal(R0)]
                    ] |> List.concat
                else
                    [
                    [BinaryCalc("mov", OAddr(Register(Util)), OAddr(Register(R0)))]
                    [BinaryCalc("mov", OAddr(Register(R0)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]          |> incDecCheck (Src, src)
                    [BinaryCalc("mov", OAddr(Register(R1)), OAddr(Register(R0)))]
                    [BinaryCalc("mov", OAddr(Register(R0)), OAddr(Register(Util)))]
                    ] |> List.concat
            | DestRegIsEven ->
                let nextReg = getNextReg reg
                if isImmediateValue src || src = Register(R0) ||
                       src = Register(reg) || src = Register(nextReg) then
                    [
                    [MoveSrcVal_toUtilReg]
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [BinaryCalc("mov", OAddr(Register(nextReg)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R1)))]
                    [BinaryCalc("xchg", OAddr(Register(nextReg)), OAddr(Register(R0)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
                elif not (isAccessibleAddress src) ||
                       isUsingTheReg src reg || isUsingTheReg src nextReg then
                    [
                    [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [BinaryCalc("mov", OAddr(Register(nextReg)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R1)))]
                    [BinaryCalc("xchg", OAddr(Register(nextReg)), OAddr(Register(R0)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
                else
                    [
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [BinaryCalc("mov", OAddr(Register(nextReg)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]          |> incDecCheck (Src, src)
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R1)))]
                    [BinaryCalc("xchg", OAddr(Register(nextReg)), OAddr(Register(R0)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
            | DestRegIsOdd ->
                if isImmediateValue src || src = Register(R0) || src = Register(reg) then
                    [
                    [MoveSrcVal_toUtilReg]
                    [StoreRegVal(R1)]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(reg)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [RestoreRegVal(R1)]
                    ] |> List.concat
                elif not (isAccessibleAddress src) || isUsingTheReg src reg then
                    [
                    [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
                    [StoreRegVal(R1)]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(reg)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [RestoreRegVal(R1)]
                    ] |> List.concat
                else
                    [
                    [BinaryCalc("mov", OAddr(Register(Util)), OAddr(Register(R1)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(reg)))]
                    [UnaryCalc(code, OSrc)]          |> incDecCheck (Src, src)
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [BinaryCalc("mov", OAddr(Register(R1)), OAddr(Register(Util)))]
                    ] |> List.concat
        Some(procedure)



// resolve i8086's divide operation's address.
module divAddressResolve =

    // return if the address is a immediate value or not.
    // addr -> bool
    let private isImmediateValue = function
        | Imm(_) -> true
        | _ -> false

    // get steps of procedure for i8086's div operation.
    // string -> addr -> reg -> procedureStep list option
    let getProcedure code src reg =
        let (| DestRegIsR0 | Other |) (reg) =
            if reg = R0 then
                DestRegIsR0
            else
                Other

        let procedure =
            match reg with
            | DestRegIsR0 ->
                if isImmediateValue src || src = Register(R0) || src = Register(R1) then
                    [
                    [MoveSrcVal_toUtilReg]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]
                    ] |> List.concat
                elif not (isAccessibleAddress src) then
                    [
                    [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]
                    ] |> List.concat
                else
                    [
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    [UnaryCalc(code, OSrc)]          |> incDecCheck (Src, src)
                    ] |> List.concat
            | Other ->
                let nextReg = getNextReg reg
                if isImmediateValue src || src = Register(R0) || src = Register(R1) ||
                       src = Register(reg) || src = Register(nextReg) then
                    [
                    [MoveSrcVal_toUtilReg]
                    [BinaryCalc("xchg", OAddr(Register(R1)), OAddr(Register(reg)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(nextReg)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [BinaryCalc("xchg", OAddr(Register(nextReg)), OAddr(Register(R1)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
                elif not (isAccessibleAddress src) ||
                       isUsingTheReg src reg || isUsingTheReg src nextReg then
                    [
                    [MoveSrcRef_toUtilReg]           |> incDecCheck (Src, src)
                    [BinaryCalc("xchg", OAddr(Register(R1)), OAddr(Register(reg)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(nextReg)))]
                    [UnaryCalc(code, OSrc)]
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [BinaryCalc("xchg", OAddr(Register(nextReg)), OAddr(Register(R1)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
                else
                    [
                    [BinaryCalc("xchg", OAddr(Register(R1)), OAddr(Register(reg)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(nextReg)))]
                    [UnaryCalc(code, OSrc)]          |> incDecCheck (Src, src)
                    [BinaryCalc("xchg", OAddr(Register(reg)), OAddr(Register(R0)))]
                    [BinaryCalc("xchg", OAddr(Register(nextReg)), OAddr(Register(R1)))]
                    [BinaryCalc("xchg", OAddr(Register(R0)), OAddr(Register(R1)))]
                    ] |> List.concat
        Some(procedure)



// resolve i8086's sxt operation's address.
module sxtAddressResolve =

    // temporary label name.
    // string
    let private tempLabelName = "8"

    // return if the address is increment operand or not.
    // addr -> bool
    let private isIncrementAddr = function
        | IncDfr(_,_) | IncDDfr(_,_)
            -> true
        | _ -> false

    // the procedureStep to clear the dest operand's value.
    // procedureStep list
    let private stepsToClearDest =
        [BinaryCalc("mov", ODest, OAddr(Imm(Expr("0"))))]

    // the procedureStep to jump to forward label,
    // if the negative condition flag is not set.
    // procedureStep list
    let private stepsToJumpToLabelIfNotNegCondition =
        [UnaryCalc("jns", OAddr(Rel(Expr(tempLabelName + "f"))))]

    // the procedureStep to invert the dest operand's value.
    // procedureStep list
    let private stepsToInvertDest =
        [UnaryCalc("not", ODest)]

    // get steps of procedure for i8086's sxt operation.
    // addr -> procedureStep list option
    let getProcedure dest =
        let procedure =
            if not (isAccessibleAddress dest) then
                [
                [MoveDestRef_toUtilReg]       |> incDecCheck (Dest, dest)
                stepsToClearDest
                stepsToJumpToLabelIfNotNegCondition
                stepsToInvertDest
                [TempLabel(tempLabelName)]
                ] |> List.concat
            elif isIncrementAddr dest then
                [
                stepsToClearDest
                stepsToJumpToLabelIfNotNegCondition
                stepsToInvertDest
                [TempLabel(tempLabelName)]    |> incDecCheck (Dest, dest)
                ] |> List.concat
            else
                [
                stepsToClearDest              |> incDecCheck (Dest, dest)
                stepsToJumpToLabelIfNotNegCondition
                stepsToInvertDest
                [TempLabel(tempLabelName)]
                ] |> List.concat
        Some(procedure)

