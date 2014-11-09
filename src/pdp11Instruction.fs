module p11trans.pdp11Instruction

(* read pdp11's instruction assembly codes *)

open p11trans.utility
open p11trans.intermediate


// register matching
// string -> reg option
let (|RegisterType|_|) = function
    | "r0" -> Some R0
    | "r1" -> Some R1
    | "r2" -> Some R2
    | "r3" -> Some R3
    | "r4" -> Some R4
    | "r5" -> Some R5
    | "sp" -> Some SP
    | "pc" -> Some PC
    | _ -> None

// convert string to register.
// string -> reg option
let toReg = (|RegisterType|_|)


// convert string to expression.
// string -> expr option
let toExpr str = pdp11Expr.getExpression str


// instruction type. Word instruction or byte instruction.
type instructionType =
    | WordInst
    | ByteInst

// convert string to address of operand.
// string -> instructionType -> addr option
let toAddr str iType =
    let r = "(r[0-5]|sp|pc)"
    let e = "([^ \f\n\r\t\v\$\*]+)"
    let regPtn = "^\s*" + r + "\s*$"
    let incDPtn = "^\s*\(" + r + "\)\+\s*$"
    let decDPtn = "^\s*-\(" + r + "\)\s*$"
    let idxDPtn = "^\s*" + e + "\(" + r + "\)\s*$"
    let dfrPtn = "^\s*\(" + r + "\)\s*$"
    let dfrPtn2 = "^\s*\*" + r + "\s*$"
    let incDDPtn = "^\s*\*\(" + r + "\)\+\s*$"
    let decDDPtn = "^\s*\*-\(" + r + "\)\s*$"
    let ddfrPtn = "^\s*\*\(" + r + "\)\s*$"
    let idxDDPtn = "^\s*\*" + e + "\(" + r + "\)\s*$"
    let relPtn = "^\s*" + e + "\s*$"
    let immPtn = "^\s*\$" + e + "\s*$"
    let relDPtn = "^\s*\*" + e + "\s*$"
    let absPtn = "^\s*\*\$" + e + "\s*$"

    match str with
    | RegexMatch regPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[1].Value
            return Register(reg);
        }
    | RegexMatch incDPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[1].Value
            let incBytes = iType |> function
                                    | ByteInst when reg <> SP -> 1
                                    | _ -> 2
            return IncDfr(reg, incBytes);
        }
    | RegexMatch decDPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[1].Value
            let decBytes = iType |> function
                                    | ByteInst when reg <> SP -> 1
                                    | _ -> 2
            return DecDfr(reg, decBytes);
        }
    | RegexMatch idxDPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[2].Value
            let! expr = toExpr rMatch.Groups.[1].Value
            return IdxDfr(reg, expr);
        }
    | RegexMatch dfrPtn rMatch | RegexMatch dfrPtn2 rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[1].Value
            return Dfr(reg);
        }
    | RegexMatch incDDPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[1].Value
            return IncDDfr(reg, 2);
        }
    | RegexMatch decDDPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[1].Value
            return DecDDfr(reg, 2);
        }
    | RegexMatch ddfrPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[1].Value
            return DDfr(reg);
        }
    | RegexMatch idxDDPtn rMatch ->
        maybe {
            let! reg = toReg rMatch.Groups.[2].Value
            let! expr = toExpr rMatch.Groups.[1].Value
            return IdxDDfr(reg, expr);
        }
    | RegexMatch relPtn rMatch ->
        maybe {
            let! expr = toExpr rMatch.Groups.[1].Value
            return Rel(expr);
        }
    | RegexMatch immPtn rMatch ->
        maybe {
            let! expr = toExpr rMatch.Groups.[1].Value
            return Imm(expr);
        }
    | RegexMatch relDPtn rMatch ->
        maybe {
            let! expr = toExpr rMatch.Groups.[1].Value
            return RelDfr(expr);
        }
    | RegexMatch absPtn rMatch ->
        maybe {
            let! expr = toExpr rMatch.Groups.[1].Value
            return Abs(expr);
        }
    | _ ->
        failwithf "Invalid address string - %s" str


// convert string to two addresses of operand part.
// string -> instructionType -> (addr * addr) option
let toTwoAddr str iType =
    maybe {
        let! (arg1, arg2) = utility.getTwoArgs str
        let! addr1 = toAddr arg1 iType
        let! addr2 = toAddr arg2 iType
        return (addr1, addr2)
    }


// convert string to register and address of operand part.
// string -> instructionType -> (reg * addr) option
let toRegAddr str iType =
    maybe {
        let! (arg1, arg2) = utility.getTwoArgs str
        let! reg = toReg arg1
        let! addr = toAddr arg2 iType
        return (reg, addr)
    }


// convert string to address and register of operand part.
// string -> instructionType -> (addr * reg) option
let toAddrReg str iType =
    maybe {
        let! (arg1, arg2) = utility.getTwoArgs str
        let! addr = toAddr arg1 iType
        let! reg = toReg arg2
        return (addr, reg)
    }


// convert string to register and expression of operand part.
// string -> (reg * expr) option
let toRegExpr str =
    maybe {
        let! (arg1, arg2) = utility.getTwoArgs str
        let! reg = toReg arg1
        let! expr = toExpr arg2
        return (reg, expr)
    }


// adapt arguments to function.
// 'a option -> ('a -> 'b) -> 'b option
let withOpr opr code = adaptArgs code opr


// read list of flags.
// clc | clv | cln ..
// string -> flag list
let flagList (str:string) =
    let codes = str.Split([|'|'|])
    let searchFlags acc code = 
        let cPattern = "^\s*(cl|se)c\s*$"    // clc or sec
        let vPattern = "^\s*(cl|se)v\s*$"    // clv or sev
        let zPattern = "^\s*(cl|se)z\s*$"    // clz or sez
        let nPattern = "^\s*(cl|se)n\s*$"    // cln or sen
        match code with
        | RegexMatch cPattern rMatch -> Cf :: acc
        | RegexMatch vPattern rMatch -> Vf :: acc
        | RegexMatch zPattern rMatch -> Zf :: acc
        | RegexMatch nPattern rMatch -> Nf :: acc
        | _ -> acc
    Array.fold
        searchFlags
        []
        codes.[0.. codes.Length - 1]


// convert code string and operand string to intermediate code expression.
// string -> string -> code option
let getInstruction codeStr oprStr =

    match codeStr with
    // flag code
    | "clc"|"clv"|"clz"|"cln" -> Some( FClr(flagList(codeStr + oprStr)) )
    | "sec"|"sev"|"sez"|"sen" -> Some( FSet(flagList(codeStr + oprStr)) )

    // branch code
    | "br"   -> BR   |> withOpr (toAddr oprStr WordInst)
    | "bne"  -> BNE  |> withOpr (toAddr oprStr WordInst)
    | "beq"  -> BEQ  |> withOpr (toAddr oprStr WordInst)
    | "bge"  -> BGE  |> withOpr (toAddr oprStr WordInst)
    | "blt"  -> BLT  |> withOpr (toAddr oprStr WordInst)
    | "bgt"  -> BGT  |> withOpr (toAddr oprStr WordInst)
    | "ble"  -> BLE  |> withOpr (toAddr oprStr WordInst)
    | "bpl"  -> BPL  |> withOpr (toAddr oprStr WordInst)
    | "bmi"  -> BMI  |> withOpr (toAddr oprStr WordInst)
    | "bhi"  -> BHI  |> withOpr (toAddr oprStr WordInst)
    | "blos" -> BLOS |> withOpr (toAddr oprStr WordInst)
    | "bvc"  -> BVC  |> withOpr (toAddr oprStr WordInst)
    | "bvs"  -> BVS  |> withOpr (toAddr oprStr WordInst)
    | "bhis" -> BHIS |> withOpr (toAddr oprStr WordInst)
    | "bec"  -> BEC  |> withOpr (toAddr oprStr WordInst)
    | "bcc"  -> BCC  |> withOpr (toAddr oprStr WordInst)
    | "blo"  -> BLO  |> withOpr (toAddr oprStr WordInst)
    | "bcs"  -> BCS  |> withOpr (toAddr oprStr WordInst)
    | "bes"  -> BES  |> withOpr (toAddr oprStr WordInst)
    | "jbr"  -> JBR  |> withOpr (toAddr oprStr WordInst)
    | "jne"  -> JNE  |> withOpr (toAddr oprStr WordInst)
    | "jeq"  -> JEQ  |> withOpr (toAddr oprStr WordInst)
    | "jge"  -> JGE  |> withOpr (toAddr oprStr WordInst)
    | "jlt"  -> JLT  |> withOpr (toAddr oprStr WordInst)
    | "jgt"  -> JGT  |> withOpr (toAddr oprStr WordInst)
    | "jle"  -> JLE  |> withOpr (toAddr oprStr WordInst)
    | "jpl"  -> JPL  |> withOpr (toAddr oprStr WordInst)
    | "jmi"  -> JMI  |> withOpr (toAddr oprStr WordInst)
    | "jhi"  -> JHI  |> withOpr (toAddr oprStr WordInst)
    | "jlos" -> JLOS |> withOpr (toAddr oprStr WordInst)
    | "jvc"  -> JVC  |> withOpr (toAddr oprStr WordInst)
    | "jvs"  -> JVS  |> withOpr (toAddr oprStr WordInst)
    | "jhis" -> JHIS |> withOpr (toAddr oprStr WordInst)
    | "jec"  -> JEC  |> withOpr (toAddr oprStr WordInst)
    | "jcc"  -> JCC  |> withOpr (toAddr oprStr WordInst)
    | "jlo"  -> JLO  |> withOpr (toAddr oprStr WordInst)
    | "jcs"  -> JCS  |> withOpr (toAddr oprStr WordInst)
    | "jes"  -> JES  |> withOpr (toAddr oprStr WordInst)

    // single operand code
    | "clr"  -> CLR  |> withOpr (toAddr oprStr WordInst)
    | "clrb" -> CLRB |> withOpr (toAddr oprStr ByteInst)
    | "com"  -> COM  |> withOpr (toAddr oprStr WordInst)
    | "comb" -> COMB |> withOpr (toAddr oprStr ByteInst)
    | "inc"  -> INC  |> withOpr (toAddr oprStr WordInst)
    | "incb" -> INCB |> withOpr (toAddr oprStr ByteInst)
    | "dec"  -> DEC  |> withOpr (toAddr oprStr WordInst)
    | "decb" -> DECB |> withOpr (toAddr oprStr ByteInst)
    | "neg"  -> NEG  |> withOpr (toAddr oprStr WordInst)
    | "negb" -> NEGB |> withOpr (toAddr oprStr ByteInst)
    | "adc"  -> ADC  |> withOpr (toAddr oprStr WordInst)
    | "adcb" -> ADCB |> withOpr (toAddr oprStr ByteInst)
    | "sbc"  -> SBC  |> withOpr (toAddr oprStr WordInst)
    | "sbcb" -> SBCB |> withOpr (toAddr oprStr ByteInst)
    | "ror"  -> ROR  |> withOpr (toAddr oprStr WordInst)
    | "rorb" -> RORB |> withOpr (toAddr oprStr ByteInst)
    | "rol"  -> ROL  |> withOpr (toAddr oprStr WordInst)
    | "rolb" -> ROLB |> withOpr (toAddr oprStr ByteInst)
    | "asr"  -> ASR  |> withOpr (toAddr oprStr WordInst)
    | "asrb" -> ASRB |> withOpr (toAddr oprStr ByteInst)
    | "asl"  -> ASL  |> withOpr (toAddr oprStr WordInst)
    | "aslb" -> ASLB |> withOpr (toAddr oprStr ByteInst)
    | "jmp"  -> JMP  |> withOpr (toAddr oprStr WordInst)
    | "swab" -> SWAB |> withOpr (toAddr oprStr WordInst)
    | "tst"  -> TST  |> withOpr (toAddr oprStr WordInst)
    | "tstb" -> TSTB |> withOpr (toAddr oprStr ByteInst)


    // double operand code
    | "mov"  -> MOV  |> withOpr (toTwoAddr oprStr WordInst)
    | "movb" -> MOVB |> withOpr (toTwoAddr oprStr ByteInst)
    | "cmp"  -> CMP  |> withOpr (toTwoAddr oprStr WordInst)
    | "cmpb" -> CMPB |> withOpr (toTwoAddr oprStr ByteInst)
    | "bit"  -> BIT  |> withOpr (toTwoAddr oprStr WordInst)
    | "bitb" -> BITB |> withOpr (toTwoAddr oprStr ByteInst)
    | "bic"  -> BIC  |> withOpr (toTwoAddr oprStr WordInst)
    | "bicb" -> BICB |> withOpr (toTwoAddr oprStr ByteInst)
    | "bis"  -> BIS  |> withOpr (toTwoAddr oprStr WordInst)
    | "bisb" -> BISB |> withOpr (toTwoAddr oprStr ByteInst)
    | "add"  -> ADD  |> withOpr (toTwoAddr oprStr WordInst)
    | "sub"  -> SUB  |> withOpr (toTwoAddr oprStr WordInst)

    //  Miscellaneous
    | "jsr"  -> JSR  |> withOpr (toRegAddr oprStr WordInst)
    | "rts"  -> RTS  |> withOpr (toReg oprStr)
    | "sys"  -> SYS  |> withOpr (toExpr oprStr)
    | "ash"  -> ASH  |> withOpr (toAddrReg oprStr WordInst)
    | "ashc" -> ASHC |> withOpr (toAddrReg oprStr WordInst)
    | "mul"  -> MUL  |> withOpr (toAddrReg oprStr WordInst)
    | "div"  -> DIV  |> withOpr (toAddrReg oprStr WordInst)
    | "xor"  -> XOR  |> withOpr (toRegAddr oprStr WordInst)
    | "sxt"  -> SXT  |> withOpr (toAddr oprStr WordInst)
    | "mark" -> MARK |> withOpr (toExpr oprStr)
    | "sob"  -> SOB  |> withOpr (toRegExpr oprStr)

    // not matched
    | _ -> None

