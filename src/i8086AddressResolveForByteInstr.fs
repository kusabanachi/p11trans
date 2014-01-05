module p11trans.i8086AddressResolveForByteInstr

(* resolve i8086's address operation for
   operand of byte instructions *)

open p11trans.i8086ProcedureStep
open p11trans.utility
open p11trans.intermediate


// return if the address is directly byte-addressable or not.
// addr -> bool
let private isByteAddressable = function
    | Register(r)
        when r = R0 || r = R1 || r = R2 || r = Util ->
        true
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r, _) | Dfr(r)
        when r = R3 || r = R4 || r = R5 || r = Util ->
        true
    | Rel(_) | Imm(_) | Abs(_) ->
        true
    | _ ->
        false


// resolve i8086's two addresses operation of byte instruction.
module twoAddressResolveForByteInstruction =

    // the procedureStep of byte calculation and sign extension.
    // procedureStep list
    let private stepsOfCalcWithSignExtn =
        [ByteBinaryCalc(ODest, OSrc); ConvertAxByteIntoWord]

    // the procedureStep of exchange ax for dest,
    // byte calculation and sign extension.
    // procedureStep list
    let private stepsOfExchangeAndCalcWithSignExtn =
        [
        [XChgAxForDestVal]
        stepsOfCalcWithSignExtn
        [ReXChgAxForDestVal]
        ] |> List.concat


    // get steps of procedure for i8086's two address operation
    // of byte instruction.
    // addr -> addr -> procedureStep list
    let private getProcedureImple dest src =
        if dest = Register(R0) then
            if isByteAddressable src then
                stepsOfCalcWithSignExtn    |> incDecCheck (Src, src)
            else
                [
                [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
                stepsOfCalcWithSignExtn
                ] |> List.concat
        elif isRegister dest then
            if isByteAddressable src then
                [
                stepsOfExchangeAndCalcWithSignExtn  |> incDecCheck (Src, src)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
                stepsOfExchangeAndCalcWithSignExtn
                ] |> List.concat
        elif isAccessibleAddress dest then
            if isByteAddressable src && not (isMemAddr src) then
                [
                [ByteBinaryCalc(ODest, OSrc)]  |> incDecCheck (Dest, dest)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
                [ByteBinaryCalc(ODest, OSrc)]  |> incDecCheck (Dest, dest)
                ] |> List.concat
        else
            if isByteAddressable src && not (isMemAddr src) then
                [
                [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
                [ByteBinaryCalc(ODest, OSrc)]
                ] |> List.concat
            else
                [
                [MoveSrcVal_toTempMem]     |> incDecCheck (Src, src)
                [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
                [XChgAxForDestVal]
                [ByteBinaryCalc(ODest, OSrc)]
                [ReXChgAxForDestVal]
                ] |> List.concat


    // entry of getting steps of procedure for i8086's
    // two address operation of byte instruction.
    // 'a -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple dest src
        Some(procedure)


// resolve i8086's one address operation of byte instruction.
module oneAddressResolveForByteInstruction =

    // the procedureStep of byte calculation and sign extension.
    // procedureStep list
    let private stepsOfCalcWithSignExtn =
        [ByteUnaryCalc(ODest); ConvertAxByteIntoWord]


    // get steps of procedure for i8086's one address operation
    // of byte instruction.
    // 'a -> addr -> procedureStep list option
    let getProcedure code dest =
        let procedure =
            if dest = Register(R0) then
                stepsOfCalcWithSignExtn
            elif isRegister dest then
                [
                [XChgAxForDestVal]
                stepsOfCalcWithSignExtn
                [ReXChgAxForDestVal]
                ] |> List.concat
            elif isAccessibleAddress dest then
                [ByteUnaryCalc(ODest)]  |> incDecCheck (Dest, dest)
            else
                [
                [MoveDestRef_toUtilReg] |> incDecCheck (Dest, dest)
                [ByteUnaryCalc(ODest)]
                ] |> List.concat
        Some(procedure)


// resolve i8086's two addresses operation of byte instruction,
// which does not store result.
module twoAddressResolveForByteInstructionWithoutStoring =

    // get steps of procedure for i8086's two address operation
    // of byte instruction, which doesn't store result.
    // addr -> addr -> procedureStep list
    let private getProcedureImple dest src =
        if not (isMemAddr dest) then
            if isByteAddressable src then
                [
                [ByteBinaryCalc(OSrc, ODest)]  |> incDecCheck (Src, src)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]         |> incDecCheck (Src, src)
                [ByteBinaryCalc(OSrc, ODest)]
                ] |> List.concat
        elif isAccessibleAddress dest then
            if isByteAddressable src && not (isMemAddr src) then
                [
                [ByteBinaryCalc(OSrc, ODest)]  |> incDecCheck (Dest, dest)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]         |> incDecCheck (Src, src)
                [ByteBinaryCalc(OSrc, ODest)]  |> incDecCheck (Dest, dest)
                ] |> List.concat
        else
            if isByteAddressable src && not (isMemAddr src) then
                [
                [MoveDestRef_toUtilReg]        |> incDecCheck (Dest, dest)
                [ByteBinaryCalc(OSrc, ODest)]
                ] |> List.concat
            else
                [
                [MoveSrcVal_toTempMem]         |> incDecCheck (Src, src)
                [MoveDestVal_toUtilReg]        |> incDecCheck (Dest, dest)
                [ByteBinaryCalc(OSrc, ODest)]
                ] |> List.concat


    // entry of getting steps of procedure for i8086's
    // two address operation of byte instruction.
    // 'a -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple dest src
        Some(procedure)


