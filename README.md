p11trans
========

* A translator that translates PDP-11 assembly code to 8086 ACK assembly code.
* This software is incomplete and some instructions have not implemented yet.

>
* PDP-11のアセンブリ言語から8086のACKのアセンブリ言語へ変換を行う、トランスレータです。
* このソフトウェアは未完成なので、いくつかの命令は未実装です。

## Usage
* If the '-i' option is specified, later words are treated as input assembly code.
* '/' is treated as line break.

>
* '-i' オプションを指定すると、後の言葉はアセンブリ言語の入力として扱います。
* '/' は改行として扱います。

```
$ mono p11trans.exe -i "mov #4, r0"
         mov ax, #4
$ mono p11trans.exe -i "mov r0, -(sp) / add r0, (r1)"
         push ax
         mov bx, dx;  add (bx), ax
```
　  

* Without the '-i' option, a following word is treated as input file name.

>
* '-i' オプションが無ければ、続く単語は入力ファイル名として扱います。

```
$ mono p11trans.exe write.s
.extern _write, cerror

_write:
         push bp
         mov bp, sp
...
```

