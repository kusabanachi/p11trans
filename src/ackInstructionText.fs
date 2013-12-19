module p11trans.ackInstructionText

(* convert steps of procedure
   to ACK i8086 instruction assembly code text *)

open p11trans.intermediate
open p11trans.utility
open p11trans.ackExpression
open p11trans.i8086AddressResolve


// get a register text.
// reg -> string
let getRegStr = function
    | R0 -> "ax"
    | R1 -> "dx"
    | R2 -> "cx"
    | R3 -> "si"
    | R4 -> "di"
    | R5 -> "bp"
    | SP -> "sp"
    | PC -> "ip"
    | Util -> "bx"


// text of memory, which is temp space to store value.
// string
let tempValMem = "tmpMem"


// get a deffered register notation.
// string -> string
let getDfrStr str = "(" + str + ")"


// get a expression's text.
// expr -> string
let getExprStr expr = getExpressionStr expr

// get a expression of immediate value's text.
// expr -> string
let getExprImm expr = "#" + getExpressionStr expr


// get a register of address.
// addr -> reg
let getRegOfAddr = function
    | Register(r)
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r,_) | Dfr(r)
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_)
      -> r
    | x ->
        failwithf "%A is not Register Address" x


// get a register text of address.
// addr -> string
let getRegStrOfAddr addr =
    getRegStr (getRegOfAddr addr)


// get a memory index value's text of address
// addr -> string
let getMemIndexStr = function
    | IdxDfr(_,e) | IdxDDfr(_,e) -> getExprStr e
    | _ -> ""


// return if memory via the register is directly accessible or not.
// reg -> bool
let isMemoryAccessibleRegister = function
    | R3 | R4 | R5 | Util -> true | _ -> false


// get instruction text,
// that move value of address to register.
// addr -> reg -> string
let moveValueToReg inAddr outReg =
    let index = getMemIndexStr inAddr
    let oreg = getRegStr outReg
    let ureg = getRegStr Util

    match inAddr with
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r,_) | Dfr(r) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + oreg + ", " + index + (getDfrStr ireg)
        else
            "mov " + ureg + ", " + ireg
              +!!+ "mov " + oreg + ", " + index + (getDfrStr ureg)
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            if ireg <> oreg then
                "mov " + ureg + ", " + index + (getDfrStr ireg)
                  +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
            else
                "mov " + oreg + ", " + index + (getDfrStr ireg)
                  +!!+ "mov " + oreg + ", " + (getDfrStr oreg)
        else
            "mov " + ureg + ", " + ireg
              +!!+ "mov " + ureg + ", " + index + (getDfrStr ureg)
              +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
    | Rel(e) | RelDfr(e) ->
        "mov " + oreg + ", " + (getExprStr e)
    | Abs(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
    | Imm(e) ->
        "mov " + oreg + ", " + (getExprImm e)
    | Register(r) ->
        let ireg = getRegStr r
        "mov " + oreg + ", " + ireg


// get instruction text,
// that move value of address to memory.
// addr -> string -> string
let moveValueToMem addr mem =
    let index = getMemIndexStr addr
    let ureg = getRegStr Util

    match addr with
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r,_) | Dfr(r) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + index + (getDfrStr reg)
              +!!+ "mov " + mem + ", " + ureg
        else
            "mov " + ureg + ", " + reg
              +!!+ "mov " + ureg + ", " + index + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + index + (getDfrStr reg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
        else
            "mov " + ureg + ", " + reg
              +!!+ "mov " + ureg + ", " + index + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
    | Rel(e) | RelDfr(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "mov " + mem + ", " + ureg
    | Abs(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
          +!!+ "mov " + mem + ", " + ureg
    | Imm(e) ->
        "mov " + mem + ", " + (getExprImm e)
    | Register(r) ->
        let ireg = getRegStr r
        "mov " + mem + ", " + ireg


// get instruction text,
// that move value's reference to register.
// return instruction text and address of the reference.
// addr -> reg -> string * addr
let moveRefToReg inAddr outReg =
    let index = getMemIndexStr inAddr
    let oreg = getRegStr outReg
    let ureg = getRegStr Util

    match inAddr with
    | IncDfr(r,_) | DecDfr(r,_) | Dfr(r) ->
        let ireg = getRegStr r
        ("mov " + oreg + ", " + ireg,
         Dfr(outReg))
    | IdxDfr(r,e) ->
        let ireg = getRegStr r
        ("mov " + oreg + ", " + ireg,
         IdxDfr(outReg,e))
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            ("mov " + oreg + ", " + index + (getDfrStr ireg),
             Dfr(outReg))
        else
            ("mov " + ureg + ", " + ireg
               +!!+ "mov " + oreg + ", " + index + (getDfrStr ureg),
             Dfr(outReg))
    | Rel(e) | RelDfr(e) ->
        ("mov " + oreg + ", " + (getExprImm e),
         Dfr(outReg))
    | Abs(e) ->
        ("mov " + oreg + ", " + (getExprStr e),
         Dfr(outReg))
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that move value's reference from memory to register.
// return instruction text and address of the reference.
// addr -> reg -> string -> string * addr
let moveRefToRegFromMem inAddr outReg mem =
    let index = getMemIndexStr inAddr
    let oreg = getRegStr outReg
    let ureg = getRegStr Util

    match inAddr with
    | IncDfr(_,_) | DecDfr(_,_) | Dfr(_)
    | Rel(_) | RelDfr(_) ->
        ("mov " + oreg + ", " + mem,
         Dfr(outReg))
    | IdxDfr(_,e) ->
        ("mov " + oreg + ", " + mem,
         IdxDfr(outReg,e))
    | IncDDfr(_,_) | DecDDfr(_,_) | DDfr(_) | IdxDDfr(_,_)
    | Abs(_) ->
        ("mov " + ureg + ", " + mem
           +!!+ "mov " + oreg + ", " + index + (getDfrStr ureg),
         Dfr(outReg))
    | _ ->
        failwithf "Invalid address"


// get address text of instruction's operand.
// addr -> string
let getAddrOperandText addr =
    match addr with
    | Register(r) ->
        getRegStr r
    | IncDfr(r,_) | DecDfr(r,_) | Dfr(r) | IdxDfr(r,_) ->
        let regStr = getRegStr r
        let index = getMemIndexStr addr
        index + (getDfrStr regStr)
    | Rel(e) | RelDfr(e) ->
        getExprStr e
    | Imm(e) ->
        getExprImm e
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that calculate with a dest operand.
// string -> addr -> string
let calcWithDest opcode dest =
    opcode + " " + getAddrOperandText dest


// get instruction text,
// that calculate with dest and src operand.
// string -> addr -> addr -> string
let calcWithDestAndSrc opcode dest src =
    opcode + " " + getAddrOperandText dest + ", " + getAddrOperandText src


// get instruction text,
// that store register's value to temp memory.
// addr -> string
let storeRegToTempMem addr =
    "mov " + tempValMem + ", " + (getRegStrOfAddr addr)


// get instruction text,
// that restore register's value from temp memory.
// addr -> string
let restoreRegFromTempMem addr =
    "mov " + (getRegStrOfAddr addr) + ", " + tempValMem


// get instruction text,
// that push value of address to stack.
// addr -> string
let pushFromAddr addr =
    let index = getMemIndexStr addr
    let ureg = getRegStr Util

    match addr with
    | Register(r) ->
        "push " + (getRegStr r)
    | IncDfr(r,_) | DecDfr(r,_) | Dfr(r) | IdxDfr(r,_) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "push " + index + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "push " + index + (getDfrStr ureg)
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + index + (getDfrStr reg)
              +!!+ "push " + (getDfrStr ureg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "mov " + ureg + ", " + index + (getDfrStr ureg)
              +!!+ "push " + (getDfrStr ureg)
    | Rel(e) | RelDfr(e) ->
        "push " + (getExprStr e)
    | Abs(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "push " + (getDfrStr ureg)
    | Imm(e) ->
        "mov " + ureg + ", " + (getExprImm e)
          +!!+ "push " + ureg


// get instruction text,
// that pop value from stack to address.
// addr -> string
let popToAddr addr =
    let index = getMemIndexStr addr
    let ureg = getRegStr Util

    match addr with
    | Register(r) ->
        "pop " + (getRegStr r)
    | IncDfr(r,_) | DecDfr(r,_) | Dfr(r) | IdxDfr(r,_) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "pop " + index + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "pop " + index + (getDfrStr ureg)
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + index + (getDfrStr reg)
              +!!+ "pop " + (getDfrStr ureg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "mov " + ureg + ", " + index + (getDfrStr ureg)
              +!!+ "pop " + (getDfrStr ureg)
    | Rel(e) | RelDfr(e) ->
        "pop " + (getExprStr e)
    | Abs(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "pop " + (getDfrStr ureg)
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that increment register of address.
// addr -> int -> string
let incrementAddr addr incNum =
    let register = getRegOfAddr addr
    let reg = getRegStr register
    if isMemoryAccessibleRegister register then
        "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
    else
        "add " + reg + ", #" + incNum.ToString()


// get instruction text,
// that increment value at memory.
// string -> int -> string
let incrementMem mem incNum =
    "add " + mem + ", #" + incNum.ToString()


// get instruction text,
// that decrement register of address.
// addr -> int -> string
let decrementAddr addr decNum =
    let register = getRegOfAddr addr
    let reg = getRegStr register
    "sub " + reg + ", #" + decNum.ToString()


// get instruction text,
// that pop value from stack address to register.
// addr -> register -> string
let popValueToReg inAddr outReg =
    let oreg = getRegStr outReg
    match inAddr with
    | IncDfr(SP,2) ->
        "pop " + oreg
    | IncDDfr(SP,2) ->
        let ureg = getRegStr Util
        "pop " + ureg
          +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that pop value from stack address to memory.
// addr -> string -> string
let popValueToMem addr mem =
    match addr with
    | IncDfr(SP,2) ->
        "pop " + mem
    | IncDDfr(SP,2) ->
        let ureg = getRegStr Util
        "pop " + ureg
          +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
          +!!+ "mov " + mem + ", " + ureg
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that pop value's reference from stack address to register.
// return instruction text and address of the reference.
// addr -> register -> string * addr
let popRefToReg inAddr outReg =
    match inAddr with
    | IncDDfr(SP,2) ->
        let oreg = getRegStr outReg
        ("pop " + oreg,
         Dfr(outReg))
    | _ ->
        failwithf "Invalid address"



// tag of address elements
type addrTag =
    | OrgSrc
    | OrgDest
    | Src
    | Dest
    | Result


// search a element from list
// addrTag -> (addrTag * addr) list -> addr
let searchElem elem elemList =
    let (_,addr) = List.find (fun (el,_) -> el = elem) elemList
    addr


// search src element from list
// (addrTag * addr) list -> addr
let searchSrcElem elemList =
    let isSrcOperand = function
        | (OrgSrc,_) | (Src,_) -> true
        | _ -> false
    let (_,addr) = List.find isSrcOperand elemList
    addr

// search dest element from list
// (addrTag * addr) list -> addr
let searchDestElem elemList =
    let isDestOperand = function
        | (OrgDest,_) | (Dest,_) -> true
        | _ -> false
    let (_,addr) = List.find isDestOperand elemList
    addr


// transform a procedure step to ACK i8086 instruction text.
// string list * string * (addrTag * addr) list
//     -> procedureStep
//     -> string list * string * (addrTag * addr) list
let transformStep (codeList, opcode, elemList) step =
    match step with
    | MoveSrcVal_toUtilReg ->
        let src = searchSrcElem elemList
        let code = moveValueToReg src Util
        (code::codeList,
         opcode,
         (Src, Register(Util))::elemList)
    | MoveSrcVal_toSrcReg ->
        let src = searchElem OrgSrc elemList
        let srcReg = getRegOfAddr src
        let code = moveValueToReg src srcReg
        (code::codeList,
         opcode,
         (Src, Register(srcReg))::elemList)
    | MoveSrcVal_toDestReg ->
        let src = searchSrcElem elemList
        let dest = searchElem OrgDest elemList
        let destReg = getRegOfAddr dest
        let code = moveValueToReg src destReg
        (code::codeList,
         opcode,
         (Src, Register(destReg))::elemList)
    | MoveSrcVal_toTempMem ->
        let src = searchSrcElem elemList
        let code = moveValueToMem src tempValMem
        (code::codeList,
         opcode,
         (Src, Rel(Expr(tempValMem)))::elemList)
    | MoveSrcRef_toUtilReg ->
        let src = searchElem OrgSrc elemList
        let (code, srcAddr) = moveRefToReg src Util
        (code::codeList,
         opcode,
         (Src, srcAddr)::elemList)
    | MoveDestVal_toUtilReg ->
        let dest = searchDestElem elemList
        let code = moveValueToReg dest Util
        (code::codeList,
         opcode,
         (Dest, Register(Util))::elemList)
    | MoveDestRef_toUtilReg ->
        let dest = searchElem OrgDest elemList
        let (code, destAddr) = moveRefToReg dest Util
        (code::codeList,
         opcode,
         (Dest, destAddr)::elemList)
    | MoveDestRef_toUtilReg_fromTempMem ->
        let dest = searchElem OrgDest elemList
        let (code, destAddr) = moveRefToRegFromMem dest Util tempValMem
        (code::codeList,
         opcode,
         (Dest, destAddr)::elemList)
    | UnaryCalc(OAddr) ->
        let dest = searchDestElem elemList
        let code = calcWithDest opcode dest
        (code::codeList, opcode, elemList)
    | BinaryCalc(OAddr, OAddr) ->
        let dest = searchDestElem elemList
        let src = searchSrcElem elemList
        let code = calcWithDestAndSrc opcode dest src
        (code::codeList,
         opcode,
         (Result, dest)::elemList)
    | StoreSrcReg ->
        let src = searchElem OrgSrc elemList
        let code = storeRegToTempMem src
        (code::codeList, opcode, elemList)
    | RestoreSrcReg ->
        let src = searchElem OrgSrc elemList
        let code = restoreRegFromTempMem src
        (code::codeList, opcode, elemList)
    | StoreDestReg ->
        let dest = searchElem OrgDest elemList
        let code = storeRegToTempMem dest
        (code::codeList, opcode, elemList)
    | RestoreDestReg ->
        let dest = searchElem OrgDest elemList
        let code = restoreRegFromTempMem dest
        (code::codeList, opcode, elemList)
    | PushResult ->
        let result = searchElem Result elemList
        let code = pushFromAddr result
        (code::codeList, opcode, elemList)
    | PopToDest ->
        let dest = searchDestElem elemList
        let code = popToAddr dest
        (code::codeList, opcode, elemList)
    | PushSrcVal ->
        let src = searchSrcElem elemList
        let code = pushFromAddr src
        (code::codeList, opcode, elemList)
    | IncrementSrcReg(incNum) ->
        let src = searchElem OrgSrc elemList
        let code = incrementAddr src incNum
        (code::codeList, opcode, elemList)
    | DecrementSrcReg(decNum) ->
        let src = searchElem OrgSrc elemList
        let code = decrementAddr src decNum
        (code::codeList, opcode, elemList)
    | IncrementDestReg(incNum) ->
        let dest = searchElem OrgDest elemList
        let code = incrementAddr dest incNum
        (code::codeList, opcode, elemList)
    | DecrementDestReg(decNum) ->
        let dest = searchElem OrgDest elemList
        let code = decrementAddr dest decNum
        (code::codeList, opcode, elemList)
    | IncrementStoreMem(incNum) ->
        let code = incrementMem tempValMem incNum
        (code::codeList, opcode, elemList)

    | PopSrcVal_toUtilReg ->
        let src = searchSrcElem elemList
        let code = popValueToReg src Util
        (code::codeList,
         opcode,
         (Src, Register(Util))::elemList)
    | PopSrcVal_toDestReg ->
        let src = searchSrcElem elemList
        let dest = searchElem OrgDest elemList
        let destReg = getRegOfAddr dest
        let code = popValueToReg src destReg
        (code::codeList,
         opcode,
         (Src, Register(destReg))::elemList)
    | PopSrcVal_toTempMem ->
        let src = searchSrcElem elemList
        let code = popValueToMem src tempValMem
        (code::codeList,
         opcode,
         (Src, Rel(Expr(tempValMem)))::elemList)
    | PopSrcRef_toUtilReg ->
        let src = searchElem OrgSrc elemList
        let (code, srcAddr) = popRefToReg src Util
        (code::codeList,
         opcode,
         (Src, srcAddr)::elemList)
    | PopDestRef_toUtilReg ->
        let dest = searchElem OrgDest elemList
        let (code, destAddr) = popRefToReg dest Util
        (code::codeList,
         opcode,
         (Dest, destAddr)::elemList)
    | PopDestVal_toUtilReg ->
        let dest = searchDestElem elemList
        let code = popValueToReg dest Util
        (code::codeList,
         opcode,
         (Src, Register(Util))::elemList)
    | _ ->
        failwithf "Unimplemented step"


// transform procedure of code which has two addresses of operands
// to ACK i8086 instruction text.
// procedureStep list -> string -> addr -> addr -> string list
let transformTwoAddrProcedureToText procedure code dest src =
    let (codeList,_,_) =
        List.fold transformStep
                  ([], code, [(OrgDest, dest); (OrgSrc, src)])
                  procedure
    List.rev codeList


// transform procedure of code which has one address of operand
// to ACK i8086 instruction text.
// procedureStep list -> string -> addr -> string list
let transformOneAddrProcedureToText procedure code dest =
    let (codeList,_,_) =
        List.fold transformStep
                  ([], code, [(OrgDest, dest)])
                  procedure
    List.rev codeList

