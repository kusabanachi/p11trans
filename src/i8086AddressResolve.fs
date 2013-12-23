module p11trans.i8086AddressResolve

(* resolve i8086's address operation for
   operand of instructions *)

open p11trans.i8086ProcedureStep
open p11trans.utility
open p11trans.intermediate


// return if the address is directly accessible or not.
// addr -> bool
let isAccessibleAddress = function
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r, _) | Dfr(r)
        when r = R3 || r = R4 || r = R5 || r = Util -> true
    | Register(_) | Rel(_) | RelDfr(_) | Imm(_) -> true
    | _ -> false


// return if the address is writable register or not.
// addr -> bool
let isWritableRegister = function
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r,_) | Dfr(r)
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_)
        when r <> SP && r <> PC -> true
    | _ -> false


// return if the address is memory or not.
// addr -> bool
let isMemAddr = function
    | Register(_) | Imm(_) -> false
    | _ -> true



// incDecCheck's target
type target =
     | Src
     | Dest


// check if the addr is designated auto increment or auto decrement.
// if so, change the procedure step or add a inc/dec procedure step.
// target * addr -> procedureStep list -> procedureStep list
let incDecCheck (target, addr) (stepList:procedureStep list) =

    // if it's properly, change the step to increment the address.
    let changeToPopStep step addr =
        if addr = IncDfr(SP, 2) || addr = IncDDfr(SP, 2) then
            match step with
            | MoveSrcVal_toUtilReg  -> Some(PopSrcVal_toUtilReg)
            | MoveSrcVal_toDestReg  -> Some(PopSrcVal_toDestReg)
            | MoveSrcVal_toTempMem  -> Some(PopSrcVal_toTempMem)
            | MoveSrcRef_toUtilReg when addr = IncDDfr(SP,2)
                                    -> Some(PopSrcRef_toUtilReg)
            | MoveSrcRef_toUtilReg when addr = IncDfr(SP,2)
                                    -> Some(PopSrcVal_toUtilReg)
            | MoveDestRef_toUtilReg when addr = IncDDfr(SP,2)
                                    -> Some(PopDestRef_toUtilReg)
            | MoveDestVal_toUtilReg -> Some(PopDestVal_toUtilReg)
            | _ -> None
        else
            None

    let step = stepList.Head    // stepList has only one element
    match addr with
    | IncDfr(_, incNum) | IncDDfr(_, incNum) ->
        match step with
        | StoreSrcReg
        | StoreDestReg ->
            [step; IncrementStoreMem(incNum)]
        | _ ->
            let popCodeOpt = changeToPopStep step addr
            if popCodeOpt.IsSome then
                [popCodeOpt.Value]
            elif target = Src then
                [step; IncrementSrcReg(incNum)]
            else
                [step; IncrementDestReg(incNum)]

    | DecDfr(_,decNum) | DecDDfr(_,decNum) ->
        if target = Src then
            [DecrementSrcReg(decNum); step]
        else
            [DecrementDestReg(decNum); step]
    | _ ->
        [step]



// resolve i8086's two addresses operation.
module twoAddressResolve =

    // a case of that both src and dest are memory address
    // addr -> addr -> procedureStep list
    let private getMemToMemProcedure dest src =
        let (| DestIsAccessible | SrcIsWritableRegister
              | DestIsWritableRegister | Other |) (dest, src) =
            if isAccessibleAddress dest then
                DestIsAccessible
            elif isWritableRegister src then
                SrcIsWritableRegister
            elif isWritableRegister dest then
                DestIsWritableRegister
            else
                Other

        match (dest, src) with
        | DestIsAccessible ->
            [
            [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Dest, dest)
            ] |> List.concat
        | SrcIsWritableRegister ->
            [
            [StoreSrcReg]              |> incDecCheck (Src, src)
            [MoveSrcVal_toSrcReg]
            [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OAddr, OAddr)]
            [RestoreSrcReg]
            ] |> List.concat
        | DestIsWritableRegister ->
            [
            [StoreDestReg]             |> incDecCheck (Dest, dest)
            [MoveSrcVal_toDestReg]     |> incDecCheck (Src, src)
            [MoveDestRef_toUtilReg_fromTempMem]
            [BinaryCalc(OAddr, OAddr)]
            [RestoreDestReg]
            ] |> List.concat
        | _ ->
            [
            [MoveSrcVal_toTempMem]     |> incDecCheck (Src, src)
            [MoveDestVal_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OAddr, OAddr)]
            [PushResult]
            [MoveDestRef_toUtilReg]
            [PopToDest]
            ] |> List.concat

    // get steps of procedure for i8086's two address operation.
    // addr -> addr -> procedureStep list
    let private getProcedureImple dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OAddr, OAddr)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(OAddr, OAddr)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Src, src)
        else
            [BinaryCalc(OAddr, OAddr)]

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
                [UnaryCalc(OAddr)]
                ] |> List.concat
            else
                [UnaryCalc(OAddr)]      |> incDecCheck (Dest, dest)
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
        let (| DestIsAccessible | DestIsNotStack
              | SrcIsWritableRegister | Other |) (dest, src) =
            if isAccessibleAddress dest then
                DestIsAccessible
            elif isNotSpAddr dest then
                DestIsNotStack
            elif isWritableRegister src then
                SrcIsWritableRegister
            else
                Other

        match (dest, src) with
        | DestIsAccessible ->
            [
            [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Dest, dest)
            ] |> List.concat
        | DestIsNotStack ->
            [
            [PushSrcVal]               |> incDecCheck (Src, src)
            [PopToDest]                |> incDecCheck (Dest, dest)
            ] |> List.concat
        | SrcIsWritableRegister ->
            [
            [StoreSrcReg]              |> incDecCheck (Src, src)
            [MoveSrcVal_toSrcReg]
            [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OAddr, OAddr)]
            [RestoreSrcReg]
            ] |> List.concat
        | _ ->
            [
            [StoreDestReg]             |> incDecCheck (Dest, dest)
            [PushSrcVal]               |> incDecCheck (Src, src)
            [MoveDestRef_toUtilReg_fromTempMem]
            [PopToDest]
            ] |> List.concat


    // get steps of procedure for i8086's move address operation.
    // addr -> addr -> procedureStep list
    let private getNotStackProcedure dest src =
        if isMemAddr dest && isMemAddr src then
            getMemToMemProcedure dest src
        elif not (isAccessibleAddress dest) then
            [
            [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OAddr, OAddr)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(OAddr, OAddr)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Src, src)
        else
            [BinaryCalc(OAddr, OAddr)]


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
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Dest, dest)
            ] |> List.concat
        | SrcIsAccessible ->
            [
            [MoveDestVal_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Src, src)
            ] |> List.concat
        | _ ->
            [
            [MoveSrcVal_toTempMem]     |> incDecCheck (Src, src)
            [MoveDestVal_toUtilReg]    |> incDecCheck (Dest, dest)
            [BinaryCalc(OAddr, OAddr)]
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
            [BinaryCalc(OAddr, OAddr)]
            ] |> List.concat
        elif not (isAccessibleAddress src) then
            [
            [MoveSrcRef_toUtilReg]     |> incDecCheck (Src, src)
            [BinaryCalc(OAddr, OAddr)]
            ] |> List.concat
        elif isMemAddr dest then
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Dest, dest)
        elif isMemAddr src then
            [BinaryCalc(OAddr, OAddr)] |> incDecCheck (Src, src)
        else
            [BinaryCalc(OAddr, OAddr)]

    // entry of getting steps of procedure for i8086's two address operation,
    // which doesn't store result.
    // 'a -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple dest src
        Some(procedure)

