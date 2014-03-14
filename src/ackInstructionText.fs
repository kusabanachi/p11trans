module p11trans.ackInstructionText

(* convert steps of procedure
   to ACK i8086 instruction assembly code text *)

open p11trans.intermediate
open p11trans.utility
open p11trans.ackExpression
open p11trans.i8086ProcedureStep


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


// get a lower 8bit register text.
// reg -> string
let getLow8bitRegStr = function
    | R0 -> "al"
    | R1 -> "dl"
    | R2 -> "cl"
    | Util -> "bl"
    | _ ->
        failwithf "Invalid address"


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
    | Rel(e) | Abs(e) ->
        "mov " + oreg + ", " + (getExprStr e)
    | RelDfr(e) ->
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
    | Rel(e) | Abs(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "mov " + mem + ", " + ureg
    | RelDfr(e) ->
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
    | Rel(e) | Abs(e) ->
        ("mov " + oreg + ", " + (getExprImm e),
         Dfr(outReg))
    | RelDfr(e) ->
        ("mov " + oreg + ", " + (getExprStr e),
         Dfr(outReg))
    | _ ->
        failwithf "Invalid address"


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
    | Rel(e) | Abs(e) ->
        "push " + (getExprStr e)
    | RelDfr(e) ->
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
    | Rel(e) | Abs(e) ->
        "pop " + (getExprStr e)
    | RelDfr(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "pop " + (getDfrStr ureg)
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that move value of address to specified register with auto (in|de)crement.
// addr -> reg -> string
let moveValueToReg_withIncDec inAddr outReg =
    let oreg = getRegStr outReg
    let ureg = getRegStr Util

    match inAddr with
    | IncDfr(r, incNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + oreg + ", " + (getDfrStr ireg)
              +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ireg)
        else
            "mov " + ureg + ", " + ireg
              +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
    | IncDDfr(r, incNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + (getDfrStr ireg)
              +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
              +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ireg)
        else
            "mov " + ureg + ", " + ireg
              +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
    | DecDfr(r, decNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ireg)
              +!!+ "mov " + oreg + ", " + (getDfrStr ireg)
        else
            "mov " + ureg + ", " + ireg
              +!!+ "lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + oreg + ", " + (-decNum).ToString() + (getDfrStr ureg)
    | DecDDfr(r, decNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ireg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ireg)
              +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
        else
            "mov " + ureg + ", " + ireg
              +!!+ "lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + oreg + ", " + (getDfrStr ureg)
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that move value of address to specified memory with auto (in|de)crement.
// addr -> string -> string
let moveValueToMem_withIncDec addr mem =
    let ureg = getRegStr Util

    match addr with
    | IncDfr(r, incNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "mov " + mem + ", " + ureg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
    | IncDDfr(r, incNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
    | DecDfr(r, decNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr reg)
              +!!+ "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "mov " + mem + ", " + ureg
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
    | DecDDfr(r, decNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr reg)
              +!!+ "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "mov " + mem + ", " + ureg
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that move value's reference to specified register with auto (in|de)crement.
// return instruction text and address of the reference.
// addr -> reg -> string * addr
let moveRefToReg_withIncDec inAddr outReg =
    let oreg = getRegStr outReg
    let ureg = getRegStr Util

    match inAddr with
    | IncDfr(r, incNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            ("mov " + oreg + ", " + ireg
               +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ireg),
             Dfr(outReg))
        elif isMemoryAccessibleRegister outReg then
            ("mov " + oreg + ", " + ireg
               +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr oreg),
             Dfr(outReg))
        else
            ("mov " + oreg + ", " + ireg
               +!!+ "mov " + ureg + ", " + ireg
               +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ureg),
             Dfr(outReg))
    | IncDDfr(r, incNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            ("mov " + oreg + ", " + (getDfrStr ireg)
               +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ireg),
             Dfr(outReg))
        else
            ("mov " + ureg + ", " + ireg
               +!!+ "lea " + ireg + ", " + incNum.ToString() + (getDfrStr ureg)
               +!!+ "mov " + oreg + ", " + (getDfrStr ureg),
             Dfr(outReg))
    | DecDfr(r, decNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            ("lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ireg)
               +!!+ "mov " + oreg + ", " + ireg,
             Dfr(outReg))
        else
            ("mov " + ureg + ", " + ireg
               +!!+ "lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ureg)
               +!!+ "mov " + oreg + ", " + ireg,
             Dfr(outReg))
    | DecDDfr(r, decNum) ->
        let ireg = getRegStr r
        if isMemoryAccessibleRegister r then
            ("lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ireg)
               +!!+ "mov " + oreg + ", " + (getDfrStr ireg),
             Dfr(outReg))
        else
            ("mov " + ureg + ", " + ireg
               +!!+ "lea " + ireg + ", " + (-decNum).ToString() + (getDfrStr ureg)
               +!!+ "mov " + oreg + ", " + (-decNum).ToString() + (getDfrStr ureg),
             Dfr(outReg))
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that push value of address to stack with auto (in|de)crement.
// addr -> string
let pushFromAddr_withIncDec addr =
    let ureg = getRegStr Util

    match addr with
    | IncDfr(r, incNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "push " + (getDfrStr reg)
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "push " + (getDfrStr ureg)
    | IncDDfr(r, incNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "push " + (getDfrStr ureg)
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "push " + (getDfrStr ureg)
    | DecDfr(r,decNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr reg)
              +!!+ "push " + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "push " + (-decNum).ToString() + (getDfrStr ureg)
    | DecDDfr(r,decNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr reg)
              +!!+ "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "push " + (getDfrStr ureg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "push " + (getDfrStr ureg)
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that pop value from stack to address with auto (in|de)crement.
// addr -> string
let popToAddr_withIncDec addr =
    let ureg = getRegStr Util

    match addr with
    | IncDfr(r, incNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "pop " + (getDfrStr reg)
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "pop " + (getDfrStr ureg)
    | IncDDfr(r, incNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "pop " + (getDfrStr ureg)
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (getDfrStr ureg)
              +!!+ "pop " + (getDfrStr ureg)
    | DecDfr(r, decNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr reg)
              +!!+ "pop " + (getDfrStr reg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "pop " + (-decNum).ToString() + (getDfrStr ureg)
    | DecDDfr(r, decNum) ->
        let reg = getRegStr r
        if isMemoryAccessibleRegister r then
            "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr reg)
              +!!+ "mov " + ureg + ", " + (getDfrStr reg)
              +!!+ "pop " + (getDfrStr ureg)
        else
            "mov " + ureg + ", " + reg
              +!!+ "lea " + reg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "mov " + ureg + ", " + (-decNum).ToString() + (getDfrStr ureg)
              +!!+ "pop " + (getDfrStr ureg)
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
    | Rel(e) | Abs(e) ->
        getExprStr e
    | Imm(e) ->
        getExprImm e
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that calculate with a argument.
// string -> addr -> string
let calcWithOneArg opcode dest =
    opcode + " " + getAddrOperandText dest


// get instruction text,
// that calculate with two arguments.
// string -> addr -> addr -> string
let calcWithTwoArgs opcode dest src =
    opcode + " " + getAddrOperandText dest + ", " + getAddrOperandText src


// get address text of byte instruction's operand.
// addr -> string
let getByteAddrOperandText addr =
    match addr with
    | Register(r) ->
        getLow8bitRegStr r
    | IncDfr(r,_) | DecDfr(r,_) | Dfr(r) | IdxDfr(r,_) ->
        let regStr = getRegStr r
        let index = getMemIndexStr addr
        index + (getDfrStr regStr)
    | Rel(e) | Abs(e) ->
        getExprStr e
    | Imm(e) ->
        getExprImm e
    | _ ->
        failwithf "Invalid address"


// get instruction text,
// that byte calculate with a argument.
// string -> addr -> string
let byteCalcWithOneArg opcode dest =
    opcode + " " + getByteAddrOperandText dest


// get instruction text,
// that byte calculate with two arguments.
// string -> addr -> addr -> string
let byteCalcWithTwoArgs opcode dest src =
    opcode + " " + getByteAddrOperandText dest
           + ", " + getByteAddrOperandText src


// get instruction text,
// that exchange ax regsiter's value for value of address.
// addr -> string
let exchangeAxRegForValue addr =
    let index = getMemIndexStr addr
    let ureg = getRegStr Util

    match addr with
    | IncDfr(r,_) | DecDfr(r,_) | IdxDfr(r,_) | Dfr(r) ->
        let areg = getRegStr r
        if isMemoryAccessibleRegister r then
            "xchg ax, " + index + (getDfrStr areg)
        else
            "mov " + ureg + ", " + areg
              +!!+ "xchg ax, " + index + (getDfrStr ureg)
    | IncDDfr(r,_) | DecDDfr(r,_) | DDfr(r) | IdxDDfr(r,_) ->
        let areg = getRegStr r
        if isMemoryAccessibleRegister r then
            "mov " + ureg + ", " + index + (getDfrStr areg)
              +!!+ "xchg ax, " + (getDfrStr ureg)
        else
            "mov " + ureg + ", " + areg
              +!!+ "mov " + ureg + ", " + index + (getDfrStr ureg)
              +!!+ "xchg ax, " + (getDfrStr ureg)
    | Rel(e) | Abs(e) ->
        "xchg ax, " + (getExprStr e)
    | RelDfr(e) ->
        "mov " + ureg + ", " + (getExprStr e)
          +!!+ "xchg ax, " + (getDfrStr ureg)
    | Imm(e) ->
        "xchg ax, " + (getExprImm e)
    | Register(r) ->
        let areg = getRegStr r
        "xchg ax, " + areg


// get instruction text, that convert al's byte into ax's word.
// string
let convertAxByteIntoWord = "cbw"


// get instruction text,
// that store register's value to temp memory.
// reg -> string
let storeRegToTempMem reg =
    "mov " + tempValMem + ", " + (getRegStr reg)


// get instruction text,
// that restore register's value from temp memory.
// reg -> string
let restoreRegFromTempMem reg =
    "mov " + (getRegStr reg) + ", " + tempValMem


// get instruction text,
// that increment register of address.
// addr -> int -> string
let incrementAddr addr incNum =
    let register = getRegOfAddr addr
    let reg = getRegStr register
    if isMemoryAccessibleRegister register then
        "lea " + reg + ", " + incNum.ToString() + (getDfrStr reg)
    else
        let ureg = getRegStr Util
        "mov " + ureg + ", " + reg
          +!!+ "lea " + reg + ", " + incNum.ToString() + (getDfrStr ureg)


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

// remove dest element from list
// (addrTag * addr) list -> (addrTag * addr) list
let removeDestAxReg elemList =
    let rec rmDestAx list =
        match list with
        | [] -> []
        | (Dest, Register(R0))::xs -> xs
        | x::xs -> x::rmDestAx xs
    rmDestAx elemList


// transform a procedure step to ACK i8086 instruction text.
// string list * (addrTag * addr) list
//     -> procedureStep
//     -> string list * (addrTag * addr) list
let transformStep (codeList, elemList) step =
    match step with
    | MoveSrcVal_toReg(reg) ->
        let src = searchSrcElem elemList
        let code = moveValueToReg src reg
        (code::codeList, (Src, Register(reg))::elemList)
    | MoveSrcVal_toTempMem ->
        let src = searchSrcElem elemList
        let code = moveValueToMem src tempValMem
        (code::codeList, (Src, Rel(Expr(tempValMem)))::elemList)
    | MoveSrcRef_toUtilReg ->
        let src = searchElem OrgSrc elemList
        let (code, srcAddr) = moveRefToReg src Util
        (code::codeList, (Src, srcAddr)::elemList)
    | MoveDestVal_toUtilReg ->
        let dest = searchDestElem elemList
        let code = moveValueToReg dest Util
        (code::codeList, (Dest, Register(Util))::elemList)
    | MoveDestRef_toUtilReg ->
        let dest = searchElem OrgDest elemList
        let (code, destAddr) = moveRefToReg dest Util
        (code::codeList, (Dest, destAddr)::elemList)
    | PushSrcVal ->
        let src = searchSrcElem elemList
        let code = pushFromAddr src
        (code::codeList, elemList)
    | PopToDest ->
        let dest = searchDestElem elemList
        let code = popToAddr dest
        (code::codeList, elemList)

    | MoveSrcVal_toReg_withIncDec(reg) ->
        let src = searchSrcElem elemList
        let code = moveValueToReg_withIncDec src reg
        (code::codeList, (Src, Register(reg))::elemList)
    | MoveSrcVal_toTempMem_withIncDec ->
        let src = searchSrcElem elemList
        let code = moveValueToMem_withIncDec src tempValMem
        (code::codeList, (Src, Rel(Expr(tempValMem)))::elemList)
    | MoveSrcRef_toUtilReg_withIncDec ->
        let src = searchElem OrgSrc elemList
        let (code, srcAddr) = moveRefToReg_withIncDec src Util
        (code::codeList, (Src, srcAddr)::elemList)
    | MoveDestVal_toUtilReg_withIncDec ->
        let dest = searchDestElem elemList
        let code = moveValueToReg_withIncDec dest Util
        (code::codeList, (Dest, Register(Util))::elemList)
    | MoveDestRef_toUtilReg_withIncDec ->
        let dest = searchElem OrgDest elemList
        let (code, destAddr) = moveRefToReg_withIncDec dest Util
        (code::codeList, (Dest, destAddr)::elemList)
    | PushSrcVal_withIncDec ->
        let src = searchSrcElem elemList
        let code = pushFromAddr_withIncDec src
        (code::codeList, elemList)
    | PopToDest_withIncDec ->
        let dest = searchDestElem elemList
        let code = popToAddr_withIncDec dest
        (code::codeList, elemList)

    | StoreRegVal(reg) ->
        let code = storeRegToTempMem reg
        (code::codeList, elemList)
    | RestoreRegVal(reg) ->
        let code = restoreRegFromTempMem reg
        (code::codeList, elemList)
    | UnaryCalc(opcode, ODest) ->
        let dest = searchDestElem elemList
        let code = calcWithOneArg opcode dest
        (code::codeList, elemList)
    | BinaryCalc(opcode, ODest, OSrc) ->
        let dest = searchDestElem elemList
        let src = searchSrcElem elemList
        let code = calcWithTwoArgs opcode dest src
        (code::codeList, elemList)
    | BinaryCalc(opcode, OSrc, ODest) ->
        let src = searchSrcElem elemList
        let dest = searchDestElem elemList
        let code = calcWithTwoArgs opcode src dest
        (code::codeList, elemList)
    | ByteUnaryCalc(opcode, ODest) ->
        let dest = searchDestElem elemList
        let code = byteCalcWithOneArg opcode dest
        (code::codeList, elemList)
    | ByteBinaryCalc(opcode, ODest, OSrc) ->
        let dest = searchDestElem elemList
        let src = searchSrcElem elemList
        let code = byteCalcWithTwoArgs opcode dest src
        (code::codeList, elemList)
    | ByteBinaryCalc(opcode, OSrc, ODest) ->
        let src = searchSrcElem elemList
        let dest = searchDestElem elemList
        let code = byteCalcWithTwoArgs opcode src dest
        (code::codeList, elemList)
    | XChgAxForDestVal ->
        let dest = searchDestElem elemList
        let code = exchangeAxRegForValue dest
        (code::codeList, (Dest, Register(R0))::elemList)
    | ReXChgAxForDestVal ->
        let elemList' = removeDestAxReg elemList
        let dest = searchDestElem elemList'
        let code = exchangeAxRegForValue dest
        (code::codeList, elemList')
    | ConvertAxByteIntoWord ->
        let code = convertAxByteIntoWord
        (code::codeList, elemList)
    | IncrementSrcReg(incNum) ->
        let src = searchElem OrgSrc elemList
        let code = incrementAddr src incNum
        (code::codeList, elemList)
    | DecrementSrcReg(decNum) ->
        let src = searchElem OrgSrc elemList
        let code = incrementAddr src -decNum
        (code::codeList, elemList)
    | IncrementDestReg(incNum) ->
        let dest = searchElem OrgDest elemList
        let code = incrementAddr dest incNum
        (code::codeList, elemList)
    | DecrementDestReg(decNum) ->
        let dest = searchElem OrgDest elemList
        let code = incrementAddr dest -decNum
        (code::codeList, elemList)

    | PopSrcVal_toReg(reg) ->
        let src = searchSrcElem elemList
        let code = popValueToReg src reg
        (code::codeList, (Src, Register(reg))::elemList)
    | PopSrcVal_toTempMem ->
        let src = searchSrcElem elemList
        let code = popValueToMem src tempValMem
        (code::codeList, (Src, Rel(Expr(tempValMem)))::elemList)
    | PopSrcRef_toUtilReg ->
        let src = searchElem OrgSrc elemList
        let (code, srcAddr) = popRefToReg src Util
        (code::codeList, (Src, srcAddr)::elemList)
    | PopDestRef_toUtilReg ->
        let dest = searchElem OrgDest elemList
        let (code, destAddr) = popRefToReg dest Util
        (code::codeList, (Dest, destAddr)::elemList)
    | PopDestVal_toUtilReg ->
        let dest = searchDestElem elemList
        let code = popValueToReg dest Util
        (code::codeList, (Dest, Register(Util))::elemList)
    | _ ->
        failwithf "Unimplemented step"


// transform procedure of code which has two addresses of operands
// to ACK i8086 instruction text.
// procedureStep list -> addr -> addr -> string list
let transformTwoAddrProcedureToText procedure dest src =
    let (codeList,_) =
        List.fold transformStep
                  ([], [(OrgDest, dest); (OrgSrc, src)])
                  procedure
    List.rev codeList


// transform procedure of code which has one address of operand
// to ACK i8086 instruction text.
// procedureStep list -> addr -> string list
let transformOneAddrProcedureToText procedure dest =
    let (codeList,_) =
        List.fold transformStep
                  ([], [(OrgDest, dest)])
                  procedure
    List.rev codeList

