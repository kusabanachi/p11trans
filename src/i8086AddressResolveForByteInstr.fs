module p11trans.i8086AddressResolveForByteInstr

(* resolve i8086's address operation for
   operand of byte instructions *)

open p11trans.i8086ProcedureStep
open p11trans.utility
open p11trans.intermediate


// resolve i8086's two addresses operation of byte instruction.
module twoAddressResolveForByteInstruction =

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

    // return if the address is register or not.
    // addr -> bool
    let private isRegister = function
        | Register(_) ->
            true
        | _ ->
            false


    // the procedureStep of byte calculation and sign extension.
    // procedureStep list
    let private stepsOfCalcWithSignExtn =
        [ByteBinaryCalc(OAddr, OAddr); ConvertAxByteIntoWord]

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
                [ByteBinaryCalc(OAddr, OAddr)]  |> incDecCheck (Dest, dest)
                ] |> List.concat
            else
                [
                [MoveSrcVal_toUtilReg]     |> incDecCheck (Src, src)
                [ByteBinaryCalc(OAddr, OAddr)]  |> incDecCheck (Dest, dest)
                ] |> List.concat
        else
            if isByteAddressable src && not (isMemAddr src) then
                [
                [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
                [ByteBinaryCalc(OAddr, OAddr)]
                ] |> List.concat
            else
                [
                [MoveSrcVal_toTempMem]     |> incDecCheck (Src, src)
                [MoveDestRef_toUtilReg]    |> incDecCheck (Dest, dest)
                [XChgAxForDestVal]
                [ByteBinaryCalc(OAddr, OAddr)]
                [ReXChgAxForDestVal]
                ] |> List.concat


    // entry of getting steps of procedure for i8086's
    // two address operation of byte instruction.
    // 'a -> addr -> addr -> procedureStep list option
    let getProcedure code dest src =
        let procedure = getProcedureImple dest src
        Some(procedure)


