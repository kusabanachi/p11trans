namespace Ack_i86

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
            match code with
            | "clr"  -> clrCode addr
            | "clrb" -> clrbCode addr
            | "com"  -> comCode addr
            | "comb" -> combCode addr
            | "inc"  -> incCode addr
            | "incb" -> incbCode addr
            | "dec"  -> decCode addr
            | "decb" -> decbCode addr
            | "neg"  -> negCode addr
            | "negb" -> negbCode addr
            | "adc"  -> adcCode addr
            | "adcb" -> adcbCode addr
            | "sbc"  -> sbcCode addr
            | "sbcb" -> sbcbCode addr
            | "ror"  -> rorCode addr
            | "rorb" -> rorbCode addr
            | "rol"  -> rolCode addr
            | "rolb" -> rolbCode addr
            | "asr"  -> asrCode addr
            | "asrb" -> asrbCode addr
            | "asl"  -> aslCode addr
            | "aslb" -> aslbCode addr
            | "jmp"  -> jmpCode addr
            | "swab" -> swabCode addr
            | "tst"  -> tstCode addr
            | "tstb" -> tstbCode addr
            | "rts"  -> rtsCode addr
            | "sxt"  -> sxtCode addr

            | "clrf"
            | "negf"
            | "absf"
            | "tstf"
            | "ldfps"
            | "stfps" -> "nop"
            | _ -> sprintf "Not suported opcode : %s" code

        let doubleOp code dest src =
            match code with
            | "mov"  -> movCode dest src
            | "movb" -> movbCode dest src
            | "cmp"  -> cmpCode dest src
            | "cmpb" -> cmpbCode dest src
            | "bit"  -> bitCode dest src
            | "bitb" -> bitbCode dest src
            | "bic"  -> bicCode dest src
            | "bicb" -> bicbCode dest src
            | "bis"  -> bisCode dest src
            | "bisb" -> bisbCode dest src
            | "add"  -> addCode dest src
            | "sub"  -> subCode dest src

            | "jsr"  -> jsrCode dest src
            | "ash"
            | "als"  -> ashCode dest src
            | "ashc"
            | "alsc" -> ashcCode dest src
            | "mul"
            | "mpy"  -> mulCode dest src
            | "div"
            | "dvd"  -> divCode dest src
            | "xor"  -> xorCode dest src
            | "sob"  -> sobCode dest src

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
            | "br"   -> brCode addr
            | "bne"  -> bneCode addr
            | "beq"  -> beqCode addr
            | "bge"  -> bgeCode addr
            | "blt"  -> bltCode addr
            | "bgt"  -> bgtCode addr
            | "ble"  -> bleCode addr
            | "bpl"  -> bplCode addr
            | "bmi"  -> bmiCode addr
            | "bhi"  -> bhiCode addr
            | "blos" -> blosCode addr
            | "bvc"  -> bvcCode addr
            | "bvs"  -> bvsCode addr
            | "bhis" -> bhisCode addr
            | "bec"
            | "bcc"  -> becCode addr
            | "blo"  -> bloCode addr
            | "bcs"
            | "bes"  -> bcsCode addr

            | "jbr"  -> jbrCode addr
            | "jne"  -> jneCode addr
            | "jeq"  -> jeqCode addr
            | "jge"  -> jgeCode addr
            | "jlt"  -> jltCode addr
            | "jgt"  -> jgtCode addr
            | "jle"  -> jleCode addr
            | "jpl"  -> jplCode addr
            | "jmi"  -> jmiCode addr
            | "jhi"  -> jhiCode addr
            | "jlos" -> jlosCode addr
            | "jvc"  -> jvcCode addr
            | "jvs"  -> jvsCode addr
            | "jhis" -> jhisCode addr
            | "jec"
            | "jcc"  -> jecCode addr
            | "jlo"  -> jloCode addr
            | "jcs"
            | "jes"  -> jcsCode addr

            | "sys"  -> sysCode expr
            | "mark" -> markCode expr
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
            | FlagClear flag             -> flagClear flag
            | FlagSet flag               -> flagSet flag

        let stringList = List.map transStatement pdp11as
        System.String.Concat stringList

