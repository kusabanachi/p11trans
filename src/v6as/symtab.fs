namespace V6as

    module Symtab =

        [<Literal>]
        let TypeText = 2s
        let TypeData = 3s
        let TypeBss = 4s
        let TypeAbsolute = 1s
        let TypeRegister = 20s

        let symTypeVal = function

        (* special variables *)

        | "."      ->  2s, 0s
        | ".."     ->  1s, 0s

        (* register *)

        | "r0"     -> 20s, 0s
        | "r1"     -> 20s, 1s
        | "r2"     -> 20s, 2s
        | "r3"     -> 20s, 3s
        | "r4"     -> 20s, 4s
        | "r5"     -> 20s, 5s
        | "sp"     -> 20s, 6s
        | "pc"     -> 20s, 7s

        (* system calls *)

        | "exit"   ->  1s, 0o1s
        | "fork"   ->  1s, 0o2s
        | "read"   ->  1s, 0o3s
        | "write"  ->  1s, 0o4s
        | "open"   ->  1s, 0o5s
        | "close"  ->  1s, 0o6s
        | "wait"   ->  1s, 0o7s
        | "creat"  ->  1s, 0o10s
        | "link"   ->  1s, 0o11s
        | "unlink" ->  1s, 0o12s
        | "exec"   ->  1s, 0o13s
        | "chdir"  ->  1s, 0o14s
        | "time"   ->  1s, 0o15s
        | "makdir" ->  1s, 0o16s
        | "chmod"  ->  1s, 0o17s
        | "chown"  ->  1s, 0o20s
        | "break"  ->  1s, 0o21s
        | "stat"   ->  1s, 0o22s
        | "seek"   ->  1s, 0o23s
        | "tell"   ->  1s, 0o24s
        | "mount"  ->  1s, 0o25s
        | "umount" ->  1s, 0o26s
        | "setuid" ->  1s, 0o27s
        | "getuid" ->  1s, 0o30s
        | "stime"  ->  1s, 0o31s
        | "fstat"  ->  1s, 0o34s
        | "mdate"  ->  1s, 0o36s
        | "stty"   ->  1s, 0o37s
        | "gtty"   ->  1s, 0o40s
        | "nice"   ->  1s, 0o42s
        | "signal" ->  1s, 0o60s

        (* double operand *)

        | "mov"    -> 11s, 0o10000s
        | "movb"   -> 11s, 0o110000s
        | "cmp"    -> 11s, 0o20000s
        | "cmpb"   -> 11s, 0o120000s
        | "bit"    -> 11s, 0o30000s
        | "bitb"   -> 11s, 0o130000s
        | "bic"    -> 11s, 0o40000s
        | "bicb"   -> 11s, 0o140000s
        | "bis"    -> 11s, 0o50000s
        | "bisb"   -> 11s, 0o150000s
        | "add"    -> 11s, 0o60000s
        | "sub"    -> 11s, 0o160000s

        (* branch *)

        | "br"     ->  6s, 0o400s
        | "bne"    ->  6s, 0o1000s
        | "beq"    ->  6s, 0o1400s
        | "bge"    ->  6s, 0o2000s
        | "blt"    ->  6s, 0o2400s
        | "bgt"    ->  6s, 0o3000s
        | "ble"    ->  6s, 0o3400s
        | "bpl"    ->  6s, 0o100000s
        | "bmi"    ->  6s, 0o100400s
        | "bhi"    ->  6s, 0o101000s
        | "blos"   ->  6s, 0o101400s
        | "bvc"    ->  6s, 0o102000s
        | "bvs"    ->  6s, 0o102400s
        | "bhis"   ->  6s, 0o103000s
        | "bec"    ->  6s, 0o103000s
        | "bcc"    ->  6s, 0o103000s
        | "blo"    ->  6s, 0o103400s
        | "bcs"    ->  6s, 0o103400s
        | "bes"    ->  6s, 0o103400s

        (* jump/branch type *)

        | "jbr"    -> 29s, 0o400s
        | "jne"    -> 30s, 0o1000s
        | "jeq"    -> 30s, 0o1400s
        | "jge"    -> 30s, 0o2000s
        | "jlt"    -> 30s, 0o2400s
        | "jgt"    -> 30s, 0o3000s
        | "jle"    -> 30s, 0o3400s
        | "jpl"    -> 30s, 0o100000s
        | "jmi"    -> 30s, 0o100400s
        | "jhi"    -> 30s, 0o101000s
        | "jlos"   -> 30s, 0o101400s
        | "jvc"    -> 30s, 0o102000s
        | "jvs"    -> 30s, 0o102400s
        | "jhis"   -> 30s, 0o103000s
        | "jec"    -> 30s, 0o103000s
        | "jcc"    -> 30s, 0o103000s
        | "jlo"    -> 30s, 0o103400s
        | "jcs"    -> 30s, 0o103400s
        | "jes"    -> 30s, 0o103400s

        (* single operand *)

        | "clr"    -> 13s, 0o5000s
        | "clrb"   -> 13s, 0o105000s
        | "com"    -> 13s, 0o5100s
        | "comb"   -> 13s, 0o105100s
        | "inc"    -> 13s, 0o5200s
        | "incb"   -> 13s, 0o105200s
        | "dec"    -> 13s, 0o5300s
        | "decb"   -> 13s, 0o105300s
        | "neg"    -> 13s, 0o5400s
        | "negb"   -> 13s, 0o105400s
        | "adc"    -> 13s, 0o5500s
        | "adcb"   -> 13s, 0o105500s
        | "sbc"    -> 13s, 0o5600s
        | "sbcb"   -> 13s, 0o105600s
        | "tst"    -> 13s, 0o5700s
        | "tstb"   -> 13s, 0o105700s
        | "ror"    -> 13s, 0o6000s
        | "rorb"   -> 13s, 0o106000s
        | "rol"    -> 13s, 0o6100s
        | "rolb"   -> 13s, 0o106100s
        | "asr"    -> 13s, 0o6200s
        | "asrb"   -> 13s, 0o106200s
        | "asl"    -> 13s, 0o6300s
        | "aslb"   -> 13s, 0o106300s
        | "jmp"    -> 13s, 0o100s
        | "swab"   -> 13s, 0o300s

        (* jsr *)

        | "jsr"    ->  7s, 0o4000s

        (* rts *)

        | "rts"    ->  8s, 0o200s

        (* simple operand *)

        | "sys"    ->  9s, 0o104400s

        (* flag-setting *)

        | "clc"    ->  1s, 0o241s
        | "clv"    ->  1s, 0o242s
        | "clz"    ->  1s, 0o244s
        | "cln"    ->  1s, 0o250s
        | "sec"    ->  1s, 0o261s
        | "sev"    ->  1s, 0o262s
        | "sez"    ->  1s, 0o264s
        | "sen"    ->  1s, 0o270s

        (* floating point ops *)

        | "cfcc"   ->  1s, 0o170000s
        | "setf"   ->  1s, 0o170001s
        | "setd"   ->  1s, 0o170011s
        | "seti"   ->  1s, 0o170002s
        | "setl"   ->  1s, 0o170012s
        | "clrf"   -> 13s, 0o170400s
        | "negf"   -> 13s, 0o170700s
        | "absf"   -> 13s, 0o170600s
        | "tstf"   -> 13s, 0o170500s
        | "movf"   -> 10s, 0o172400s
        | "movif"  -> 12s, 0o177000s
        | "movfi"  ->  5s, 0o175400s
        | "movof"  -> 12s, 0o177400s
        | "movfo"  ->  5s, 0o176000s
        | "addf"   -> 12s, 0o172000s
        | "subf"   -> 12s, 0o173000s
        | "mulf"   -> 12s, 0o171000s
        | "divf"   -> 12s, 0o174400s
        | "cmpf"   -> 12s, 0o173400s
        | "modf"   -> 12s, 0o171400s
        | "movie"  -> 12s, 0o176400s
        | "movei"  ->  5s, 0o175000s
        | "ldfps"  -> 13s, 0o170100s
        | "stfps"  -> 13s, 0o170200s
        | "fr0"    -> 20s, 0s
        | "fr1"    -> 20s, 1s
        | "fr2"    -> 20s, 2s
        | "fr3"    -> 20s, 3s
        | "fr4"    -> 20s, 4s
        | "fr5"    -> 20s, 5s

        (* 11/45 operations *)

        | "als"    -> 24s, 0o72000s
        | "alsc"   -> 24s, 0o73000s
        | "mpy"    -> 24s, 0o70000s
        | "mul"    -> 24s, 0o70000s
        | "div"    -> 24s, 0o71000s
        | "ash"    -> 24s, 0o72000s
        | "ashc"   -> 24s, 0o73000s
        | "dvd"    -> 24s, 0o71000s
        | "xor"    ->  7s, 0o74000s
        | "sxt"    -> 13s, 0o6700s
        | "mark"   ->  9s, 0o6400s
        | "sob"    -> 25s, 0o77000s

        (* specials *)

        | ".byte"  -> 14s, 0s
        | ".even"  -> 16s, 0s
        | ".if"    -> 17s, 0s
        | ".endif" -> 18s, 0s
        | ".globl" -> 19s, 0s
        | ".text"  -> 21s, 0s
        | ".data"  -> 22s, 0s
        | ".bss"   -> 23s, 0s
        | ".comm"  -> 26s, 0s

        (* undef *)
        | _        ->  0s, 0s

        let symType = fst << symTypeVal

        let symVal = snd << symTypeVal


