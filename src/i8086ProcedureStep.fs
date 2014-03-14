module p11trans.i8086ProcedureStep

(* steps of i8086's procedure *)

open p11trans.intermediate


// kind of operand
type operand =
    | ODest
    | OSrc

// type of step of procedure
type procedureStep =
    | MoveSrcVal_toReg of reg
    | MoveSrcVal_toTempMem
    | MoveSrcRef_toUtilReg
    | MoveDestVal_toUtilReg
    | MoveDestRef_toUtilReg
    | PushSrcVal
    | PopToDest

    | MoveSrcVal_toReg_withIncDec of reg
    | MoveSrcVal_toTempMem_withIncDec
    | MoveSrcRef_toUtilReg_withIncDec
    | MoveDestVal_toUtilReg_withIncDec
    | MoveDestRef_toUtilReg_withIncDec
    | PushSrcVal_withIncDec
    | PopToDest_withIncDec

    | StoreRegVal of reg
    | RestoreRegVal of reg
    | UnaryCalc of string * operand
    | BinaryCalc of string * operand * operand
    | ByteUnaryCalc of string * operand
    | ByteBinaryCalc of string * operand * operand
    | XChgAxForDestVal
    | ReXChgAxForDestVal
    | ConvertAxByteIntoWord
    | IncrementSrcReg of int
    | DecrementSrcReg of int
    | IncrementDestReg of int
    | DecrementDestReg of int
    | PopSrcVal_toReg of reg
    | PopSrcVal_toTempMem
    | PopSrcRef_toUtilReg
    | PopDestRef_toUtilReg
    | PopDestVal_toUtilReg

// procedureStep
let MoveSrcVal_toUtilReg = MoveSrcVal_toReg Util
let PopSrcVal_toUtilReg = PopSrcVal_toReg Util




// return if the address is directly accessible or not.
// addr -> bool
let isAccessibleAddress = function
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r, _) | Dfr(r)
        when r = R3 || r = R4 || r = R5 || r = Util -> true
    | Register(_) | Rel(_) | Imm(_) | Abs(_) -> true
    | _ -> false


// return if the address is memory or not.
// addr -> bool
let isMemAddr = function
    | Register(_) | Imm(_) -> false
    | _ -> true


// return if the address is register or not.
// addr -> bool
let isRegister = function
    | Register(_) ->
        true
    | _ ->
        false




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
            | MoveSrcVal_toReg(reg) -> Some(PopSrcVal_toReg(reg))
            | MoveSrcVal_toTempMem  -> Some(PopSrcVal_toTempMem)
            | MoveSrcRef_toUtilReg when addr = IncDDfr(SP,2)
                                    -> Some(PopSrcRef_toUtilReg)
            | MoveSrcRef_toUtilReg when addr = IncDfr(SP,2)
                                    -> Some(PopSrcVal_toReg(Util))
            | MoveDestRef_toUtilReg when addr = IncDDfr(SP,2)
                                    -> Some(PopDestRef_toUtilReg)
            | MoveDestVal_toUtilReg -> Some(PopDestVal_toUtilReg)
            | _ -> None
        else
            None

    // if it's properly, change the step to move and (in|de)crement the address.
    let changeToMoveWithIncDecStep step =
        match step with
        | MoveSrcVal_toReg(reg) -> Some(MoveSrcVal_toReg_withIncDec(reg))
        | MoveSrcVal_toTempMem  -> Some(MoveSrcVal_toTempMem_withIncDec)
        | MoveSrcRef_toUtilReg  -> Some(MoveSrcRef_toUtilReg_withIncDec)
        | MoveDestVal_toUtilReg -> Some(MoveDestVal_toUtilReg_withIncDec)
        | MoveDestRef_toUtilReg -> Some(MoveDestRef_toUtilReg_withIncDec)
        | PushSrcVal            -> Some(PushSrcVal_withIncDec)
        | PopToDest             -> Some(PopToDest_withIncDec)
        | _ -> None

    // append (in|de)crement step to steps.
    let appendAloneIncDecStep stepList target addr =
        match addr with
        | IncDfr(_, num) | IncDDfr(_, num) ->
            if target = Src then
                stepList @ [IncrementSrcReg(num)]
            else
                stepList @ [IncrementDestReg(num)]
        | DecDfr(_, num) | DecDDfr(_, num) ->
            if target = Src then
                DecrementSrcReg(num) :: stepList
            else
                DecrementDestReg(num) :: stepList
        | _ ->
            stepList


    let stepHead = stepList.Head
    let stepTail = stepList.Tail

    match addr with
    | IncDfr(_, num) | IncDDfr(_, num)
    | DecDfr(_, num) | DecDDfr(_, num) ->
        let popCodeOption = changeToPopStep stepHead addr
        if popCodeOption.IsSome then
            popCodeOption.Value :: stepTail
        else
            let moveCodeOption = changeToMoveWithIncDecStep stepHead
            if moveCodeOption.IsSome then
                moveCodeOption.Value :: stepTail
            else
                appendAloneIncDecStep stepList target addr
    | _ ->
        stepList


