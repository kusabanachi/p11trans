p11trans
========

* A translator that translates PDP-11 assembly code to 8086 ACK assembly code.
* It is for learning purposes.
* This software is incomplete and some instructions have not implemented yet.

## Usage
* If the '-i' option is specified, later words are treated as input assembly code.
* '/' is treated as line break.

```
$ mono p11trans.exe -i "mov #4, r0"
         mov ax, #4
$ mono p11trans.exe -i "mov r0, -(sp) / add r0, (r1)"
         push ax
         mov bx, dx;  add (bx), ax
```


* Without the '-i' option, a following word is treated as input file name.

```
$ mono p11trans.exe write.s
         .extern _write, cerror
_write:
         push bp
         mov bp, sp
...
```

