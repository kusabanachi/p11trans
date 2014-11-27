namespace Ack_i86

module Ack_i86_trans =

    open StatementType
    open Instruction
    open ByteInstruction
    open Expres

    let ack_i86_asm pdp11as =
        let singleOp code addr =
            let immVal num = Addres.Imm (Expr_Oct (int16 num))
            match code with
            // branch code
            | "br"   -> incType "jmp"  addr
            | "bne"  -> incType "jne"  addr
            | "beq"  -> incType "je"   addr
            | "bge"  -> incType "jge"  addr
            | "blt"  -> incType "jl"   addr
            | "bgt"  -> incType "jg"   addr
            | "ble"  -> incType "jle"  addr
            | "bpl"  -> incType "jns"  addr
            | "bmi"  -> incType "js"   addr
            | "bhi"  -> incType "ja"   addr
            | "blos" -> incType "jbe"  addr
            | "bvc"  -> incType "jno"  addr
            | "bvs"  -> incType "jo"   addr
            | "bhis" -> incType "jae"  addr
            | "bec"  -> incType "jnc"  addr
            | "bcc"  -> incType "jnc"  addr
            | "blo"  -> incType "jb"   addr
            | "bcs"  -> incType "jc"   addr
            | "bes"  -> incType "jc"   addr

            | "jbr"  -> incType "jmp"  addr
            | "jne"  -> incType "jne"  addr
            | "jeq"  -> incType "je"   addr
            | "jge"  -> incType "jge"  addr
            | "jlt"  -> incType "jl"   addr
            | "jgt"  -> incType "jg"   addr
            | "jle"  -> incType "jle"  addr
            | "jpl"  -> incType "jns"  addr
            | "jmi"  -> incType "js"   addr
            | "jhi"  -> incType "ja"   addr
            | "jlos" -> incType "jbe"  addr
            | "jvc"  -> incType "jno"  addr
            | "jvs"  -> incType "jo"   addr
            | "jhis" -> incType "jae"  addr
            | "jec"  -> incType "jnc"  addr
            | "jcc"  -> incType "jnc"  addr
            | "jlo"  -> incType "jb"   addr
            | "jcs"  -> incType "jc"   addr
            | "jes"  -> incType "jc"   addr

            // single operand code
            | "clr"  -> addType  "and"  addr (immVal 0)
            | "clrb" -> andbType "andb" addr (immVal 0)
            | "com"  -> addType  "xor"  addr (immVal 0xffff)
            | "comb" -> andbType "xorb" addr (immVal 0xff)
            | "inc"  -> incType  "inc"  addr
            | "dec"  -> incType  "dec"  addr
            | "neg"  -> incType  "neg"  addr
            | "adc"  -> addType  "adc"  addr (immVal 0)
            | "adcb" -> andbType "adcb" addr (immVal 0)
            | "sbc"  -> addType  "sbb"  addr (immVal 0)
            | "sbcb" -> andbType "sbbb" addr (immVal 0)
            | "ror"  -> addType  "rcr"  addr (immVal 1)
            | "rorb" -> andbType "rcrb" addr (immVal 1)
            | "rol"  -> addType  "rcl"  addr (immVal 1)
            | "rolb" -> andbType "rclb" addr (immVal 1)
            | "asr"  -> addType  "sar"  addr (immVal 1)
            | "asrb" -> andbType "sarb" addr (immVal 1)
            | "asl"  -> addType  "sal"  addr (immVal 1)
            | "aslb" -> andbType "salb" addr (immVal 1)
            | "jmp"  -> incType  "jmp"  addr
            | "tst"  -> cmpType  "test" addr (immVal 0xffff)
            | _ -> ""

        let doubleOp code dest src =
            match code with
            // double operand code
            | "mov"  -> movType  "mov"  dest src
            | "cmp"  -> cmpType  "cmp"  dest src
            | "bit"  -> cmpType  "test" dest src
            | "bis"  -> addType  "or"   dest src
            | "bisb" -> andbType "orb"  dest src
            | "add"  -> addType  "add"  dest src
            | "sub"  -> addType  "sub"  dest src

            //  Miscellaneous
            | "jsr"  ->
                if src = Addres.Reg Addres.PC then
                    incType "call" dest
                else
                   //getInstructionText (MOV(Register(reg), DecDfr(SP)))
                   //+!!+ getInstructionText (MOV(Register(PC), Register(reg)))
                   //+!!+ getInstructionText (BR(dest))
                   failwith "jsr with not PC register is unimplemented.."
            | _ -> ""

        let transStatement = function
            | SingleOp (code, addr)      -> singleOp code addr
            | DoubleOp (code, src, dest) -> doubleOp code dest src
            | _ -> ""

        List.map transStatement pdp11as

