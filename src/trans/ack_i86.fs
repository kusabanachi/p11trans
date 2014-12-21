namespace Ack_i86

open ExpressionType
open V6as
open V6as.StatementType
open Instruction
open ByteInstruction
open Assign
open Label
open Comment
open Eos

module Ack_i86_trans =

    let ack_i86_asm pdp11as =
        let singleOp code addr =
            let immVal num = Addres.Imm (Expr_Oct (int16 num))
            match code with
            | "clr"  -> addType  "and"   addr (immVal 0)
            | "clrb" -> andbType "andb"  addr (immVal 0)
            | "com"  -> addType  "xor"   addr (immVal 0xffff)
            | "comb" -> andbType "xorb"  addr (immVal 0xff)
            | "inc"  -> incType  "inc"   addr
            | "incb" -> incbType "incb"  addr
            | "dec"  -> incType  "dec"   addr
            | "decb" -> incbType "decb"  addr
            | "neg"  -> incType  "neg"   addr
            | "negb" -> incbType "negb"  addr
            | "adc"  -> addType  "adc"   addr (immVal 0)
            | "adcb" -> andbType "adcb"  addr (immVal 0)
            | "sbc"  -> addType  "sbb"   addr (immVal 0)
            | "sbcb" -> andbType "sbbb"  addr (immVal 0)
            | "ror"  -> addType  "rcr"   addr (immVal 1)
            | "rorb" -> andbType "rcrb"  addr (immVal 1)
            | "rol"  -> addType  "rcl"   addr (immVal 1)
            | "rolb" -> andbType "rclb"  addr (immVal 1)
            | "asr"  -> addType  "sar"   addr (immVal 1)
            | "asrb" -> andbType "sarb"  addr (immVal 1)
            | "asl"  -> addType  "sal"   addr (immVal 1)
            | "aslb" -> andbType "salb"  addr (immVal 1)
            | "jmp"  -> incType  "jmp"   addr
            | "swab" -> addType  "rol"   addr (immVal 8)
            | "tst"  -> cmpType  "test"  addr (immVal 0xffff)
            | "tstb" -> cmpbType "testb" addr (immVal 0xff)
            | "rts"  -> rtsType          addr
            | "sxt"  -> sxtType          addr

            | "clrf"
            | "negf"
            | "absf"
            | "tstf"
            | "ldfps"
            | "stfps" -> "nop"
            | _ -> sprintf "Not suported opcode : %s" code

        let doubleOp code dest src =
            match code with
            | "mov"  -> movType  "mov"   dest src
            | "movb" -> movbType "movb"  dest src
            | "cmp"  -> cmpType  "cmp"   dest src
            | "cmpb" -> cmpbType "cmpb"  dest src
            | "bit"  -> cmpType  "test"  dest src
            | "bitb" -> cmpbType "testb" dest src
            | "bic"  -> bicType          dest src
            | "bicb" -> bicbType         dest src
            | "bis"  -> addType  "or"    dest src
            | "bisb" -> andbType "orb"   dest src
            | "add"  -> addType  "add"   dest src
            | "sub"  -> addType  "sub"   dest src

            | "jsr"  -> jsrType          dest src
            | "ash"
            | "als"  -> ashType          dest src
            | "ashc"
            | "alsc" -> ashcType         dest src
            | "mul"
            | "mpy"  -> mulType          dest src
            | "div"
            | "dvd"  -> divType          dest src
            | "xor"  -> addType  "xor"   dest src
            | "sob"  -> sobType          dest src

            | "movf"
            | "movif"
            | "movfi"
            | "movof"
            | "movfo"
            | "addf"
            | "subf"
            | "mulf"
            | "divf"
            | "cmpf"
            | "modf"
            | "movie"
            | "movei"  -> "nop"
            | _ -> sprintf "Not suported opcode : %s" code

        let exprOp code expr =
            let addr = Addres.Rel expr
            match code with
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
            | "bec"
            | "bcc"  -> incType "jnc"  addr
            | "blo"  -> incType "jb"   addr
            | "bcs"
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

            | "sys"  -> sysType  expr
            | "mark" -> markType expr
            | _ -> sprintf "Not suported opcode : %s" code


        let transStatement = function
            | SingleOp (code, addr)      -> singleOp code addr
            | DoubleOp (code, src, dest) -> doubleOp code dest src
            | ExprOp   (code, expr)      -> exprOp   code expr
            | Assignment (sym, expr)     -> assign sym expr
            | NameLabel name             -> nameLabel name
            | NumericLabel num           -> numLabel num
            | Comment text               -> comment text
            | Str str                    -> Pseudo.ascii str
            | Byte byteExprs             -> Pseudo.data1 byteExprs
            | Even                       -> Pseudo.even
            | If _                       -> "! .if is not suported."
            | EndIf                      -> "! .endif is not suported."
            | Global names               -> Pseudo.globalSym names
            | Text                       -> Pseudo.text
            | Data                       -> Pseudo.data
            | Bss                        -> Pseudo.bss
            | Common (name,expr)         -> Pseudo.common name expr
            | Expr expr                  -> Pseudo.data2 expr
            | Eos c                      -> eos c

        let stringList = List.map transStatement pdp11as
        System.String.Concat stringList

