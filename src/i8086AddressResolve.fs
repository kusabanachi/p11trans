module p11trans.i8086AddressResolve

(* resolve i8086's address operation for
   operand of instructions *)

open p11trans.i8086ProcedureStep
open p11trans.utility
open p11trans.intermediate


// return if the address is using the register or not.
// addr -> reg -> bool
let isUsingTheReg addr reg =
    match addr with
    | Register(reg)
    | IncDfr(reg,_) | DecDfr(reg,_) | IdxDfr(reg,_) | Dfr(reg)
    | IncDDfr(reg,_) | DecDDfr(reg,_) | IdxDDfr(reg,_) | DDfr(reg) ->
        true
    | _ ->
        false


// get free register, which is not used by the two addresses.
// addr -> addr -> reg
let getFreeReg addr1 addr2 =
    let isNotUsedReg reg =
        not (isUsingTheReg addr1 reg || isUsingTheReg addr2 reg)

    if isNotUsedReg R0 then
        R0
    elif isNotUsedReg R1 then
        R1
    else
        R2


// resolve i8086's two addresses operation.
module twoAddressResolve =

    // a case of that both src and dest are memory address
    // addr -> addr -> procedureStep list
    let private getMemToMemProcedure dest src =
        let (| DestIsAccessible
              | Other |) (dest, src) =
            if isAccessibleAddress dest then
                DestIsAccessible
            else
                Other

        match (dest, src) with
        | DestIsAccessible ->
            [
            [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(ODest, OSrc)]  |> incDecCheck (Dest, dest)
            ] |> List.concat
        | _ ->
            let freeReg = getFreeReg src dest
            [
            [StoreRegVal(freeReg)]
            [MoveSrcVal_toReg(freeReg)]  |> incDecCheck (Src, src)
            [MoveDestRef_toUtilReg]      |> incDecCheck (Dest, dest)
            [BinaryCalc(ODest, OSrc)]
            [RestoreRegVal(freeReg)]
            ] |> List.concat

    // get steps of procedure for i8086's two address operation.
    // addr -> addr -> procedureStep list
    let private getProcedureImple dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(ODest, OSrc)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(ODest, OSrc)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(ODest, OSrc)]  |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(ODest, OSrc)]  |> incDecCheck (Src, src)
        else
            [BinaryCalc(ODest, OSrc)]

    // entry of getting steps of procedure for i8086's two address operation.
    // 'a -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple dest src
        Some(procedure)



// resolve i8086's one address operation.
module oneAddressResolve =

    // get steps of procedure for i8086's one address operation.
    // 'a -> addr -> procedureStep list option
    let getProcedure code dest =
        let procedure =
            if not (isAccessibleAddress dest) then
                [
                [MoveDestRef_toUtilReg] |> incDecCheck (Dest, dest)
                [UnaryCalc(ODest)]
                ] |> List.concat
            else
                [UnaryCalc(ODest)]      |> incDecCheck (Dest, dest)
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
    // addr -> addr -> procedureStep list
    let private getMemToMemProcedure dest src =
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
            [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(ODest, OSrc)]  |> incDecCheck (Dest, dest)
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
            [BinaryCalc(ODest, OSrc)]
            [RestoreRegVal(freeReg)]
            ] |> List.concat


    // get steps of procedure for i8086's move address operation.
    // addr -> addr -> procedureStep list
    let private getNotStackProcedure dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(ODest, OSrc)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(ODest, OSrc)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(ODest, OSrc)]  |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(ODest, OSrc)]  |> incDecCheck (Src, src)
        else
            [BinaryCalc(ODest, OSrc)]


    // entry of getting steps of procedure for i8086's move address operation.
    // apply push or pop steps, if it is available.
    // 'a -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure =
            match (dest, src) with
            | (DecDfr(SP, 2), _) ->
                [PushSrcVal]           |> incDecCheck (Src, src)
            | (Dfr(SP), _) ->
                [
                [IncrementDestReg(2)]
                [PushSrcVal]           |> incDecCheck (Src, src)
                ] |> List.concat
            | (_, IncDfr(SP, 2)) ->
                [PopToDest]            |> incDecCheck (Dest, dest)
            | _ ->
                getNotStackProcedure dest src
        Some(procedure)



// resolve i8086's two address operation, which does not store result.
module twoAddressResolveWithoutStoring =

    // a case of that both src and dest are memory address
    // addr -> addr -> procedureStep list
    let private getMemToMemProcedure dest src =
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
            [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(OSrc, ODest)]  |> incDecCheck (Dest, dest)
            ] |> List.concat
        | SrcIsAccessible ->
            [
            [MoveDestVal_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OSrc, ODest)]  |> incDecCheck (Src, src)
            ] |> List.concat
        | _ ->
            [
            [MoveSrcVal_toTempMem]     |> incDecCheck (Src, src)
            [MoveDestVal_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OSrc, ODest)]
            ] |> List.concat


    // get steps of procedure for i8086's two address operation,
    // which doesn't store result.
    // addr -> addr -> procedureStep list
    let private getProcedureImple dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OSrc, ODest)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(OSrc, ODest)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(OSrc, ODest)]  |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(OSrc, ODest)]  |> incDecCheck (Src, src)
        else
            [BinaryCalc(OSrc, ODest)]

    // entry of getting steps of procedure for i8086's two address operation,
    // which doesn't store result.
    // 'a -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple dest src
        Some(procedure)

