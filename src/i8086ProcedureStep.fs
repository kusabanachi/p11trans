module p11trans.i8086ProcedureStep

(* steps of i8086's procedure *)


// kind of operand
type operand =
    | OReg
    | OAddr
    | OExpr

// type of step of procedure
type procedureStep =
    | MoveSrcVal_toUtilReg
    | MoveSrcVal_toSrcReg
    | MoveSrcVal_toDestReg
    | MoveSrcVal_toTempMem
    | MoveSrcRef_toUtilReg
    | MoveDestVal_toUtilReg
    | MoveDestRef_toUtilReg
    | MoveDestRef_toUtilReg_fromTempMem
    | UnaryCalc of operand
    | BinaryCalc of operand * operand
    | StoreSrcReg
    | RestoreSrcReg
    | StoreDestReg
    | RestoreDestReg
    | PushResult
    | PopToDest
    | PushSrcVal
    | IncrementSrcReg of int
    | DecrementSrcReg of int
    | IncrementDestReg of int
    | DecrementDestReg of int
    | IncrementStoreMem of int
    | PopSrcVal_toUtilReg
    | PopSrcVal_toDestReg
    | PopSrcVal_toTempMem
    | PopSrcRef_toUtilReg
    | PopDestRef_toUtilReg
    | PopDestVal_toUtilReg

