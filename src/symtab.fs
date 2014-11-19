
module Symtab

    [<Literal>]
    let TypeText = 2
    let TypeData = 3
    let TypeBss = 4
    let TypeAbsolute = 1
    let TypeRegister = 20

    let symTypeVal = function

    (* special variables *)

    | "."      ->  2, 0
    | ".."     ->  1, 0

    (* register *)

    | "r0"     -> 20, 0
    | "r1"     -> 20, 1
    | "r2"     -> 20, 2
    | "r3"     -> 20, 3
    | "r4"     -> 20, 4
    | "r5"     -> 20, 5
    | "sp"     -> 20, 6
    | "pc"     -> 20, 7

    (* system calls *)

    | "exit"   ->  1, 0o1
    | "fork"   ->  1, 0o2
    | "read"   ->  1, 0o3
    | "write"  ->  1, 0o4
    | "open"   ->  1, 0o5
    | "close"  ->  1, 0o6
    | "wait"   ->  1, 0o7
    | "creat"  ->  1, 0o10
    | "link"   ->  1, 0o11
    | "unlink" ->  1, 0o12
    | "exec"   ->  1, 0o13
    | "chdir"  ->  1, 0o14
    | "time"   ->  1, 0o15
    | "makdir" ->  1, 0o16
    | "chmod"  ->  1, 0o17
    | "chown"  ->  1, 0o20
    | "break"  ->  1, 0o21
    | "stat"   ->  1, 0o22
    | "seek"   ->  1, 0o23
    | "tell"   ->  1, 0o24
    | "mount"  ->  1, 0o25
    | "umount" ->  1, 0o26
    | "setuid" ->  1, 0o27
    | "getuid" ->  1, 0o30
    | "stime"  ->  1, 0o31
    | "fstat"  ->  1, 0o34
    | "mdate"  ->  1, 0o36
    | "stty"   ->  1, 0o37
    | "gtty"   ->  1, 0o40
    | "nice"   ->  1, 0o42
    | "signal" ->  1, 0o60

    (* double operand *)

    | "mov"    -> 11, 0o10000
    | "movb"   -> 11, 0o110000
    | "cmp"    -> 11, 0o20000
    | "cmpb"   -> 11, 0o120000
    | "bit"    -> 11, 0o30000
    | "bitb"   -> 11, 0o130000
    | "bic"    -> 11, 0o40000
    | "bicb"   -> 11, 0o140000
    | "bis"    -> 11, 0o50000
    | "bisb"   -> 11, 0o150000
    | "add"    -> 11, 0o60000
    | "sub"    -> 11, 0o160000

    (* branch *)

    | "br"     ->  6, 0o400
    | "bne"    ->  6, 0o1000
    | "beq"    ->  6, 0o1400
    | "bge"    ->  6, 0o2000
    | "blt"    ->  6, 0o2400
    | "bgt"    ->  6, 0o3000
    | "ble"    ->  6, 0o3400
    | "bpl"    ->  6, 0o100000
    | "bmi"    ->  6, 0o100400
    | "bhi"    ->  6, 0o101000
    | "blos"   ->  6, 0o101400
    | "bvc"    ->  6, 0o102000
    | "bvs"    ->  6, 0o102400
    | "bhis"   ->  6, 0o103000
    | "bec"    ->  6, 0o103000
    | "bcc"    ->  6, 0o103000
    | "blo"    ->  6, 0o103400
    | "bcs"    ->  6, 0o103400
    | "bes"    ->  6, 0o103400

    (* jump/branch type *)

    | "jbr"    -> 29, 0o400
    | "jne"    -> 30, 0o1000
    | "jeq"    -> 30, 0o1400
    | "jge"    -> 30, 0o2000
    | "jlt"    -> 30, 0o2400
    | "jgt"    -> 30, 0o3000
    | "jle"    -> 30, 0o3400
    | "jpl"    -> 30, 0o100000
    | "jmi"    -> 30, 0o100400
    | "jhi"    -> 30, 0o101000
    | "jlos"   -> 30, 0o101400
    | "jvc"    -> 30, 0o102000
    | "jvs"    -> 30, 0o102400
    | "jhis"   -> 30, 0o103000
    | "jec"    -> 30, 0o103000
    | "jcc"    -> 30, 0o103000
    | "jlo"    -> 30, 0o103400
    | "jcs"    -> 30, 0o103400
    | "jes"    -> 30, 0o103400

    (* single operand *)

    | "clr"    -> 13, 0o5000
    | "clrb"   -> 13, 0o105000
    | "com"    -> 13, 0o5100
    | "comb"   -> 13, 0o105100
    | "inc"    -> 13, 0o5200
    | "incb"   -> 13, 0o105200
    | "dec"    -> 13, 0o5300
    | "decb"   -> 13, 0o105300
    | "neg"    -> 13, 0o5400
    | "negb"   -> 13, 0o105400
    | "adc"    -> 13, 0o5500
    | "adcb"   -> 13, 0o105500
    | "sbc"    -> 13, 0o5600
    | "sbcb"   -> 13, 0o105600
    | "tst"    -> 13, 0o5700
    | "tstb"   -> 13, 0o105700
    | "ror"    -> 13, 0o6000
    | "rorb"   -> 13, 0o106000
    | "rol"    -> 13, 0o6100
    | "rolb"   -> 13, 0o106100
    | "asr"    -> 13, 0o6200
    | "asrb"   -> 13, 0o106200
    | "asl"    -> 13, 0o6300
    | "aslb"   -> 13, 0o106300
    | "jmp"    -> 13, 0o100
    | "swab"   -> 13, 0o300

    (* jsr *)

    | "jsr"    ->  7, 0o4000

    (* rts *)

    | "rts"    ->  8, 0o200

    (* simple operand *)

    | "sys"    ->  9, 0o104400

    (* flag-setting *)

    | "clc"    ->  1, 0o241
    | "clv"    ->  1, 0o242
    | "clz"    ->  1, 0o244
    | "cln"    ->  1, 0o250
    | "sec"    ->  1, 0o261
    | "sev"    ->  1, 0o262
    | "sez"    ->  1, 0o264
    | "sen"    ->  1, 0o270

    (* floating point ops *)

    | "cfcc"   ->  1, 0o170000
    | "setf"   ->  1, 0o170001
    | "setd"   ->  1, 0o170011
    | "seti"   ->  1, 0o170002
    | "setl"   ->  1, 0o170012
    | "clrf"   -> 13, 0o170400
    | "negf"   -> 13, 0o170700
    | "absf"   -> 13, 0o170600
    | "tstf"   -> 13, 0o170500
    | "movf"   -> 10, 0o172400
    | "movif"  -> 12, 0o177000
    | "movfi"  ->  5, 0o175400
    | "movof"  -> 12, 0o177400
    | "movfo"  ->  5, 0o176000
    | "addf"   -> 12, 0o172000
    | "subf"   -> 12, 0o173000
    | "mulf"   -> 12, 0o171000
    | "divf"   -> 12, 0o174400
    | "cmpf"   -> 12, 0o173400
    | "modf"   -> 12, 0o171400
    | "movie"  -> 12, 0o176400
    | "movei"  ->  5, 0o175000
    | "ldfps"  -> 13, 0o170100
    | "stfps"  -> 13, 0o170200
    | "fr0"    -> 20, 0
    | "fr1"    -> 20, 1
    | "fr2"    -> 20, 2
    | "fr3"    -> 20, 3
    | "fr4"    -> 20, 4
    | "fr5"    -> 20, 5

    (* 11/45 operations *)

    | "als"    -> 24, 0o72000
    | "alsc"   -> 24, 0o73000
    | "mpy"    -> 24, 0o70000
    | "mul"    -> 24, 0o70000
    | "div"    -> 24, 0o71000
    | "ash"    -> 24, 0o72000
    | "ashc"   -> 24, 0o73000
    | "dvd"    -> 24, 0o71000
    | "xor"    ->  7, 0o74000
    | "sxt"    -> 13, 0o6700
    | "mark"   ->  9, 0o6400
    | "sob"    -> 25, 0o77000

    (* specials *)

    | ".byte"  -> 14, 0
    | ".even"  -> 16, 0
    | ".if"    -> 17, 0
    | ".endif" -> 18, 0
    | ".globl" -> 19, 0
    | ".text"  -> 21, 0
    | ".data"  -> 22, 0
    | ".bss"   -> 23, 0
    | ".comm"  -> 26, 0

    (* undef *)
    | _        ->  0, 0

    let symType = fst << symTypeVal

    let symVal = snd << symTypeVal


