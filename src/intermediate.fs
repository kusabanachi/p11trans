module p11trans.intermediate

(* intermediate types to express the content of
   original pdp11 assembly codes *)

// registers
// 'Util' is non-existent register in Pdp11
type reg =
    | R0 | R1 | R2 | R3 | R4 | R5 | SP | PC | Util

// expression
type expr = Expr of string

// address of operand
type addr =
    | Register of reg
    | IncDfr of reg * int
    | DecDfr of reg * int
    | IdxDfr of reg * expr
    | Dfr of reg
    | IncDDfr of reg * int
    | DecDDfr of reg * int
    | DDfr of reg
    | IdxDDfr of reg * expr
    | Rel of expr
    | Imm of expr
    | RelDfr of expr
    | Abs of expr

// symbol
type symbol = Symbol of string

// flags of cpu status register
type flag = | Cf | Vf | Zf | Nf

// machine code
type code =
    // flag code
    | FSet of flag list
    | FClr of flag list

    // branch code
    | BR of addr    | BNE of addr
    | BEQ of addr   | BGE of addr
    | BLT of addr   | BGT of addr
    | BLE of addr   | BPL of addr
    | BMI of addr   | BHI of addr
    | BLOS of addr  | BVC of addr
    | BVS of addr   | BHIS of addr
    | BEC of addr   | BCC of addr
    | BLO of addr   | BCS of addr
    | BES of addr
    | JBR of addr   | JNE of addr
    | JEQ of addr   | JGE of addr
    | JLT of addr   | JGT of addr
    | JLE of addr   | JPL of addr
    | JMI of addr   | JHI of addr
    | JLOS of addr  | JVC of addr
    | JVS of addr   | JHIS of addr
    | JEC of addr   | JCC of addr
    | JLO of addr   | JCS of addr
    | JES of addr

    // single operand code
    | CLR of addr   | CLRB of addr
    | COM of addr   | COMB of addr
    | INC of addr   | INCB of addr
    | DEC of addr   | DECB of addr
    | NEG of addr   | NEGB of addr
    | ADC of addr   | ADCB of addr
    | SBC of addr   | SBCB of addr
    | ROR of addr   | RORB of addr
    | ROL of addr   | ROLB of addr
    | ASR of addr   | ASRB of addr
    | ASL of addr   | ASLB of addr
    | JMP of addr   | SWAB of addr
    | TST of addr   | TSTB of addr

    // double operand code
    | MOV of addr * addr   | MOVB of addr * addr
    | CMP of addr * addr   | CMPB of addr * addr
    | BIT of addr * addr   | BITB of addr * addr
    | BIC of addr * addr   | BICB of addr * addr
    | BIS of addr * addr   | BISB of addr * addr
    | ADD of addr * addr   | SUB of addr * addr

    //  miscellaneous code
    | JSR of reg * addr   | RTS of reg
    | SYS of expr
    | ASH of addr * reg    | ASHC of addr * reg
    | MUL of addr * reg    | DIV of addr * reg
    | XOR of reg * addr   | SXT of addr
    | MARK of expr        | SOB of reg * expr


// pseudo-operations
type pseudoOp =
    | Byte of expr list
    | Even
    | If of expr
    | Endif
    | Global of string
    | SectText | SectData | SectBss
    | Comm of string * expr


// element of statement
type statementElement =
    | Assign      of symbol * expr  // assignment of symbol
    | String      of string         // string
    | Instruction of code           // instruction
    | Pseudo      of pseudoOp       // pseudo-operation
    | Expression  of expr           // expression statement



