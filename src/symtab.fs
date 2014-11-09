
module Symtab

    [<Literal>]
    let TypeText = 2
    let TypeData = 3
    let TypeBss = 4
    let TypeAbsolute = 1
    let TypeRegister = 20

    let symType = function

    (* special variables *)

    | "."      ->  2
    | ".."     ->  1

    (* register *)

    | "r0"     -> 20
    | "r1"     -> 20
    | "r2"     -> 20
    | "r3"     -> 20
    | "r4"     -> 20
    | "r5"     -> 20
    | "sp"     -> 20
    | "pc"     -> 20

    (* system calls *)

    | "exit"   ->  1
    | "fork"   ->  1
    | "read"   ->  1
    | "write"  ->  1
    | "open"   ->  1
    | "close"  ->  1
    | "wait"   ->  1
    | "creat"  ->  1
    | "link"   ->  1
    | "unlink" ->  1
    | "exec"   ->  1
    | "chdir"  ->  1
    | "time"   ->  1
    | "makdir" ->  1
    | "chmod"  ->  1
    | "chown"  ->  1
    | "break"  ->  1
    | "stat"   ->  1
    | "seek"   ->  1
    | "tell"   ->  1
    | "mount"  ->  1
    | "umount" ->  1
    | "setuid" ->  1
    | "getuid" ->  1
    | "stime"  ->  1
    | "fstat"  ->  1
    | "mdate"  ->  1
    | "stty"   ->  1
    | "gtty"   ->  1
    | "nice"   ->  1
    | "signal" ->  1

    (* double operand *)

    | "mov"    -> 11
    | "movb"   -> 11
    | "cmp"    -> 11
    | "cmpb"   -> 11
    | "bit"    -> 11
    | "bitb"   -> 11
    | "bic"    -> 11
    | "bicb"   -> 11
    | "bis"    -> 11
    | "bisb"   -> 11
    | "add"    -> 11
    | "sub"    -> 11

    (* branch *)

    | "br"     ->  6
    | "bne"    ->  6
    | "beq"    ->  6
    | "bge"    ->  6
    | "blt"    ->  6
    | "bgt"    ->  6
    | "ble"    ->  6
    | "bpl"    ->  6
    | "bmi"    ->  6
    | "bhi"    ->  6
    | "blos"   ->  6
    | "bvc"    ->  6
    | "bvs"    ->  6
    | "bhis"   ->  6
    | "bec"    ->  6
    | "bcc"    ->  6
    | "blo"    ->  6
    | "bcs"    ->  6
    | "bes"    ->  6

    (* jump/branch type *)

    | "jbr"    -> 29
    | "jne"    -> 30
    | "jeq"    -> 30
    | "jge"    -> 30
    | "jlt"    -> 30
    | "jgt"    -> 30
    | "jle"    -> 30
    | "jpl"    -> 30
    | "jmi"    -> 30
    | "jhi"    -> 30
    | "jlos"   -> 30
    | "jvc"    -> 30
    | "jvs"    -> 30
    | "jhis"   -> 30
    | "jec"    -> 30
    | "jcc"    -> 30
    | "jlo"    -> 30
    | "jcs"    -> 30
    | "jes"    -> 30

    (* single operand *)

    | "clr"    -> 13
    | "clrb"   -> 13
    | "com"    -> 13
    | "comb"   -> 13
    | "inc"    -> 13
    | "incb"   -> 13
    | "dec"    -> 13
    | "decb"   -> 13
    | "neg"    -> 13
    | "negb"   -> 13
    | "adc"    -> 13
    | "adcb"   -> 13
    | "sbc"    -> 13
    | "sbcb"   -> 13
    | "tst"    -> 13
    | "tstb"   -> 13
    | "ror"    -> 13
    | "rorb"   -> 13
    | "rol"    -> 13
    | "rolb"   -> 13
    | "asr"    -> 13
    | "asrb"   -> 13
    | "asl"    -> 13
    | "aslb"   -> 13
    | "jmp"    -> 13
    | "swab"   -> 13

    (* jsr *)

    | "jsr"    ->  7

    (* rts *)

    | "rts"    ->  8

    (* simple operand *)

    | "sys"    ->  9

    (* flag-setting *)

    | "clc"    ->  1
    | "clv"    ->  1
    | "clz"    ->  1
    | "cln"    ->  1
    | "sec"    ->  1
    | "sev"    ->  1
    | "sez"    ->  1
    | "sen"    ->  1

    (* floating point ops *)

    | "cfcc"   ->  1
    | "setf"   ->  1
    | "setd"   ->  1
    | "seti"   ->  1
    | "setl"   ->  1
    | "clrf"   -> 13
    | "negf"   -> 13
    | "absf"   -> 13
    | "tstf"   -> 13
    | "movf"   -> 10
    | "movif"  -> 12
    | "movfi"  ->  5
    | "movof"  -> 12
    | "movfo"  ->  5
    | "addf"   -> 12
    | "subf"   -> 12
    | "mulf"   -> 12
    | "divf"   -> 12
    | "cmpf"   -> 12
    | "modf"   -> 12
    | "movie"  -> 12
    | "movei"  ->  5
    | "ldfps"  -> 13
    | "stfps"  -> 13
    | "fr0"    -> 20
    | "fr1"    -> 20
    | "fr2"    -> 20
    | "fr3"    -> 20
    | "fr4"    -> 20
    | "fr5"    -> 20

    (* 11/45 operations *)

    | "als"    -> 24
    | "alsc"   -> 24
    | "mpy"    -> 24
    | "mul"    -> 24
    | "div"    -> 24
    | "ash"    -> 24
    | "ashc"   -> 24
    | "dvd"    -> 24
    | "xor"    ->  7
    | "sxt"    -> 13
    | "mark"   ->  9
    | "sob"    -> 25

    (* specials *)

    | ".byte"  -> 14
    | ".even"  -> 16
    | ".if"    -> 17
    | ".endif" -> 18
    | ".globl" -> 19
    | ".text"  -> 21
    | ".data"  -> 22
    | ".bss"   -> 23
    | ".comm"  -> 26

    (* undef *)
    | _        ->  0
