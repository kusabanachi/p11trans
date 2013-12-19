module p11trans.ackInstruction

(* entry of translation intermediate expressions
   to ACK i8086 instruction assembly code *)

open p11trans.utility
open p11trans.intermediate
open i8086AddressResolve


// the number used for system call
let trapNumber = 7


// get ACK i8086 code text. the code has two addresses of opeands.
// string -> addr -> addr -> string
let twoAddrCode code src dest =
    match twoAddressResolve.getProcedure code dest src with
    | Some procedure ->
        let codeList = ackInstructionText.transformTwoAddrProcedureToText
                           procedure code dest src
        String.concat ";  " codeList
    | _ ->
        let codeStr = sprintf "(%s  %s, %s)" code (dest.ToString()) (src.ToString())
        failwithf "Failed to resolve address - %s" codeStr


// get ACK i8086 code text. the code has one address of opeand.
// string -> addr -> string
let oneAddrCode code addr =
    match oneAddressResolve.getProcedure code addr with
    | Some procedure ->
        let codeList = ackInstructionText.transformOneAddrProcedureToText
                           procedure code addr
        String.concat ";  " codeList
    | _ ->
        let codeStr = sprintf "(%s  %s)" code (addr.ToString())
        failwithf "Failed to resolve address - %s" codeStr


// get ACK i8086 move code text. the code doesn't use destination's original value.
// string -> addr -> addr -> string
let moveCode code src dest =
    match moveAddressResolve.getProcedure code dest src with
    | Some procedure ->
        let codeList = ackInstructionText.transformTwoAddrProcedureToText
                           procedure code dest src
        String.concat ";  " codeList
    | _ ->
        let codeStr = sprintf "(%s  %s, %s)" code (dest.ToString()) (src.ToString())
        failwithf "Failed to resolve address - %s" codeStr


// get ACK i8086 code text. the code is with two address argument and not store result.
// string -> addr -> addr -> string
let twoAddrCodeWithoutStoring code src dest =
    match twoAddressResolveWithoutStoring.getProcedure code dest src with
    | Some procedure ->
        let codeList = ackInstructionText.transformTwoAddrProcedureToText
                           procedure code dest src
        String.concat ";  " codeList
    | _ ->
        let codeStr = sprintf "(%s  %s, %s)" code (dest.ToString()) (src.ToString())
        failwithf "Failed to resolve address - %s" codeStr


// get ACK i8086 system call text.
// string -> expr -> string
let syscallCode code = function
    | Expr(expr) ->
    code + " " + trapNumber.ToString()
      +!!+ ".data1 " + sysent.getSyscallNumber expr


// get ACK i8086 instruction text.
// code -> string
let rec getInstructionText = function
    // flag code
    | FSet(flags) ->
        if flags = [Cf] then "clc" else "Failed.."
    | FClr(flags) ->
        if flags = [Cf] then "stc" else "Failed.."

    // branch code
    | BR(addr) -> oneAddrCode "jmp" addr
    | BNE(addr) -> oneAddrCode "jne" addr
    | BEQ(addr) -> oneAddrCode "je" addr
    | BGE(addr) -> oneAddrCode "jge" addr
    | BLT(addr) -> oneAddrCode "jl" addr
    | BGT(addr) -> oneAddrCode "jg" addr
    | BLE(addr) -> oneAddrCode "jle" addr
    | BPL(addr) -> oneAddrCode "jns" addr
    | BMI(addr) -> oneAddrCode "js" addr
    | BHI(addr) -> oneAddrCode "ja" addr
    | BLOS(addr) -> oneAddrCode "jbe" addr
    | BVC(addr) -> oneAddrCode "jno" addr
    | BVS(addr) -> oneAddrCode "jo" addr
    | BHIS(addr) -> oneAddrCode "jae" addr
    | BEC(addr) -> oneAddrCode "jnc" addr
    | BCC(addr) -> oneAddrCode "jnc" addr
    | BLO(addr) -> oneAddrCode "jb" addr
    | BCS(addr) -> oneAddrCode "jc" addr
    | BES(addr) -> oneAddrCode "jc" addr

    | JBR(addr) -> oneAddrCode "jmp" addr
    | JNE(addr) -> oneAddrCode "jne" addr
    | JEQ(addr) -> oneAddrCode "je" addr
    | JGE(addr) -> oneAddrCode "jge" addr
    | JLT(addr) -> oneAddrCode "jl" addr
    | JGT(addr) -> oneAddrCode "jg" addr
    | JLE(addr) -> oneAddrCode "jle" addr
    | JPL(addr) -> oneAddrCode "jns" addr
    | JMI(addr) -> oneAddrCode "js" addr
    | JHI(addr) -> oneAddrCode "ja" addr
    | JLOS(addr) -> oneAddrCode "jbe" addr
    | JVC(addr) -> oneAddrCode "jno" addr
    | JVS(addr) -> oneAddrCode "jo" addr
    | JHIS(addr) -> oneAddrCode "jae" addr
    | JEC(addr) -> oneAddrCode "jnc" addr
    | JCC(addr) -> oneAddrCode "jnc" addr
    | JLO(addr) -> oneAddrCode "jb" addr
    | JCS(addr) -> oneAddrCode "jc" addr
    | JES(addr) -> oneAddrCode "jc" addr

    // single operand code
    | CLR(addr) -> twoAddrCode "and" (Imm(Expr("0"))) addr
    //| CLRB(addr) -> twoAddrCode "andb" (Imm(Expr("0"))) addr
    | COM(addr) -> twoAddrCode "xor" (Imm(Expr("177777"))) addr
    //| COMB(addr) -> twoAddrCode "xorb" (Imm(Expr("377"))) addr
    | INC(addr) -> oneAddrCode "inc" addr
    //| INCB(addr) -> oneAddrCode "incb" addr
    | DEC(addr) -> oneAddrCode "dec" addr
    //| DECB(addr) -> oneAddrCode "decb" addr
    | NEG(addr) -> oneAddrCode "neg" addr
    //| NEGB(addr) -> oneAddrCode "negb" addr
    | ADC(addr) -> twoAddrCode "adc" (Imm(Expr("0"))) addr
    //| ADCB(addr) -> twoAddrCode "adcb" (Imm(Expr("0"))) addr
    | SBC(addr) -> twoAddrCode "sbb" (Imm(Expr("0"))) addr
    //| SBCB(addr) -> twoAddrCode "sbbb" (Imm(Expr("0"))) addr
    | ROR(addr) -> twoAddrCode "rcr" (Imm(Expr("1"))) addr
    //| RORB(addr) -> twoAddrCode "rcrb" (Imm(Expr("1"))) addr
    | ROL(addr) -> twoAddrCode "rcl" (Imm(Expr("1"))) addr
    //| ROLB(addr) -> twoAddrCode "rclb" (Imm(Expr("1"))) addr
    | ASR(addr) -> twoAddrCode "sar" (Imm(Expr("1"))) addr
    //| ASRB(addr) -> twoAddrCode "sarb" (Imm(Expr("1"))) addr
    | ASL(addr) -> twoAddrCode "sal" (Imm(Expr("1"))) addr
    //| ASLB(addr) -> twoAddrCode "salb" (Imm(Expr("1"))) addr
    | JMP(addr) -> oneAddrCode "jmp" addr
    | SWAB(addr) ->
        getInstructionText (ROR(addr))
          +!!+ getInstructionText (ROR(addr))
          +!!+ getInstructionText (ROR(addr))
          +!!+ getInstructionText (ROR(addr))
    | TST(addr) ->
        twoAddrCodeWithoutStoring "test" (Imm(Expr("177777"))) addr
    //| TSTB(addr) ->
    //    twoAddrCodeWithoutStoring "testb" (Imm(Expr("377"))) addr

    // double operand code
    | MOV(src, dest) -> moveCode "mov" src dest
    //| MOVB(src, dest) -> moveCode "movb" src dest
    | CMP(src, dest) -> twoAddrCodeWithoutStoring "cmp" dest src
    //| CMPB(src, dest) -> twoAddrCodeWithoutStoring "cmpb" dest src
    | BIT(src, dest) -> twoAddrCodeWithoutStoring "test" src dest
    //| BITB(src, dest) -> twoAddrCodeWithoutStoring "testb" src dest
    //| BIC(src, dest) -> twoAddrCode ""
    //| BICB(src, dest) -> twoAddrCode ""
    | BIS(src, dest) -> twoAddrCode "or" src dest
    //| BISB(src, dest) -> twoAddrCode "orb" src dest
    | ADD(src, dest) -> twoAddrCode "add" src dest
    | SUB(src, dest) -> twoAddrCode "sub" src dest

    //  Miscellaneous
    | JSR(reg, dest) ->
        if reg = PC then
            oneAddrCode "call" dest
        else
            //getInstructionText (MOV(Register(reg), DecDfr(SP)))
            //+!!+ getInstructionText (MOV(Register(PC), Register(reg)))
            //+!!+ getInstructionText (BR(dest))
            failwithf "jsr with not PC register is unimplemented.."
    | RTS(reg) ->
        if reg = PC then
            "ret"
        else
            getInstructionText (MOV(IncDfr(SP,2), Register(reg)))
            +!!+ getInstructionText (BR(Register(reg)))
    | SYS(expr) -> syscallCode "int" expr

    //| ASH(addr, reg) ->
    //| ASHC(addr, reg) ->
    //| MUL(addr, reg) ->
    //| DIV(addr, reg) ->
    //| XOR(reg, addr) -> "xor"
    //| SXT(addr) ->
    //| MARK(expr)
    //| SOB(reg, expr) ->

    | unknown -> failwithf "%A is unimplemented..." unknown

