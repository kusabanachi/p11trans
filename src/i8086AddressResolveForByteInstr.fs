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
    // string -> procedureStep list
    let private stepsOfCalcWithSignExtn code =
        [ByteBinaryCalc(code, ODest, OSrc); ConvertAxByteIntoWord]

    // the procedureStep of exchange ax for dest,
    // byte calculation and sign extension.
    // string -> procedureStep list
    let private stepsOfExchangeAndCalcWithSignExtn code =
        [
        [XChgAxForDestVal]
        stepsOfCalcWithSignExtn code
        [ReXChgAxForDestVal]
        ] |> List.concat


    // get steps of procedure for i8086's two address operation
    // of byte instruction.
    // string -> addr -> addr -> procedureStep list
    let private getProcedureImple code dest src =
        if dest = Register(R0) then
            if isByteAddressable src then
                stepsOfCalcWithSignExtn code |> incDecCheck (Src, src)
            else
                [
                [MoveSrcVal_toUtilReg]       |> incDecCheck (Src, src)
                stepsOfCalcWithSignExtn code
                ] |> List.concat
        elif isRegister dest then
            if isByteAddressable src && src <> Register(R0) then
                [
                stepsOfExchangeAndCalcWithSignExtn code
                                             |> incDecCheck (Src, src)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]       |> incDecCheck (Src, src)
                stepsOfExchangeAndCalcWithSignExtn code
                ] |> List.concat
        elif isAccessibleAddress dest then
            if isByteAddressable src && not (isMemAddr src) then
                [
                [ByteBinaryCalc(code, ODest, OSrc)]
                                             |> incDecCheck (Dest, dest)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]       |> incDecCheck (Src, src)
                [ByteBinaryCalc(code, ODest, OSrc)]
                                             |> incDecCheck (Dest, dest)
                ] |> List.concat
        else
            if isByteAddressable src && not (isMemAddr src) then
                [
                [MoveDestRef_toUtilReg]      |> incDecCheck (Dest, dest)
                [ByteBinaryCalc(code, ODest, OSrc)]
                ] |> List.concat
            else
                [
                [MoveSrcVal_toTempMem]       |> incDecCheck (Src, src)
                [MoveDestRef_toUtilReg]      |> incDecCheck (Dest, dest)
                [XChgAxForDestVal]
                [ByteBinaryCalc(code, ODest, OSrc)]
                [ReXChgAxForDestVal]
                ] |> List.concat


    // entry of getting steps of procedure for i8086's
    // two address operation of byte instruction.
    // string -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple code dest src
        Some(procedure)


// resolve i8086's one address operation of byte instruction.
module oneAddressResolveForByteInstruction =

    // the procedureStep of byte calculation and sign extension.
    // string -> procedureStep list
    let private stepsOfCalcWithSignExtn code =
        [ByteUnaryCalc(code, ODest); ConvertAxByteIntoWord]


    // get steps of procedure for i8086's one address operation
    // of byte instruction.
    // string -> addr -> procedureStep list option
    let getProcedure code dest =
        let procedure =
            if dest = Register(R0) then
                stepsOfCalcWithSignExtn code
            elif isRegister dest then
                [
                [XChgAxForDestVal]
                stepsOfCalcWithSignExtn code
                [ReXChgAxForDestVal]
                ] |> List.concat
            elif isAccessibleAddress dest then
                [ByteUnaryCalc(code, ODest)]  |> incDecCheck (Dest, dest)
            else
                [
                [MoveDestRef_toUtilReg]       |> incDecCheck (Dest, dest)
                [ByteUnaryCalc(code, ODest)]
                ] |> List.concat
        Some(procedure)


// resolve i8086's two addresses operation of byte instruction,
// which does not store result.
module twoAddressResolveForByteInstructionWithoutStoring =

    // get steps of procedure for i8086's two address operation
    // of byte instruction, which doesn't store result.
    // string -> addr -> addr -> procedureStep list
    let private getProcedureImple code dest src =
        if not (isMemAddr dest) then
            if isByteAddressable src then
                [
                [ByteBinaryCalc(code, OSrc, ODest)]
                                             |> incDecCheck (Src, src)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]       |> incDecCheck (Src, src)
                [ByteBinaryCalc(code, OSrc, ODest)]
                ] |> List.concat
        elif isAccessibleAddress dest then
            if isByteAddressable src && not (isMemAddr src) then
                [
                [ByteBinaryCalc(code, OSrc, ODest)]
                                             |> incDecCheck (Dest, dest)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]       |> incDecCheck (Src, src)
                [ByteBinaryCalc(code, OSrc, ODest)]
                                             |> incDecCheck (Dest, dest)
                ] |> List.concat
        else
            if isByteAddressable src && not (isMemAddr src) then
                [
                [MoveDestRef_toUtilReg]
                                             |> incDecCheck (Dest, dest)
                [ByteBinaryCalc(code, OSrc, ODest)]
                ] |> List.concat
            else
                [
                [MoveSrcVal_toTempMem]       |> incDecCheck (Src, src)
                [MoveDestVal_toUtilReg]      |> incDecCheck (Dest, dest)
                [ByteBinaryCalc(code, OSrc, ODest)]
                ] |> List.concat


    // entry of getting steps of procedure for i8086's
    // two address operation of byte instruction.
    // string -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple code dest src
        Some(procedure)


