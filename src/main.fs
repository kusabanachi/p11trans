module p11trans.Main

open p11trans.utility
open p11trans.readPdp11as
open p11trans.transTo8086as


let usage =
    "Usage: p11trans pdp11asmFileName\n" +
    "       p11trans -i \"pdp11command / pdp11command / ...\"\n" +
    " Translate pdp11 assembly into 8086 assembly."

let version = "p11trans 0.01"


let translate input =
    try
        let intermediate = readPdp11as input
        transTo8086Asm intermediate
    with
        | exn ->
            printfn "Error: %s" exn.Message
            Operators.exit 1


let isHelpOption = function
    | "-h" | "--help" -> true
    | _ -> false

let isVersionOption = function
    | "-v" | "--version" -> true
    | _ -> false

let isStdinOption = function
    | "-i" | "--stdin" -> true
    | _ -> false

let isInvalidOption (arg:string) =
    if arg.StartsWith("-") &&
          not (isHelpOption arg) &&
          not (isVersionOption arg) &&
          not (isStdinOption arg) then
       true
    else
       false

let printString = printfn "%s"


[<EntryPoint>]
let main args = 
    if args.Length = 0 ||
           (isStdinOption args.[0] && args.Length = 1) ||
           isHelpOption args.[0] || 
           isInvalidOption args.[0] then
        printString usage
        1
    elif isVersionOption args.[0] then
        printString version
        0
    elif isStdinOption args.[0] then
        let input = String.concat " " args.[1..]
        let input' = input.Split( [|"/"|], System.StringSplitOptions.RemoveEmptyEntries )
                     |> Array.toList
        let output = translate input'
        printString (String.concat "\n" output)
        0
    else
        let fileName = args.[0]
        if System.IO.File.Exists fileName then
            let input = Array.toList (System.IO.File.ReadAllLines fileName)
            let output = translate input
            printString (String.concat "\n" output)
            0
        else
            printfn "'%s': No such file" fileName
            1

