module p11trans.Main

(* entry point and parse arguments *)

open p11trans.utility
open p11trans.readPdp11as
open p11trans.transTo8086as


// usage text
// string
let usage =
    "Usage: p11trans pdp11asmFileName\n" +
    "       p11trans -i \"pdp11command / pdp11command / ...\"\n" +
    " Translate pdp11 assembly into 8086 assembly."


// version text
// string
let version = "p11trans 0.01"


// flow of translation
// string list -> string list
let translate input =
    try
        let intermediate = readPdp11as input    // read PDP-11 asm and convert to intermediate
        transTo8086Asm intermediate             // translate intermediate into 8086 asm
    with
        | exn ->
            printfn "Error: %s" exn.Message
            Operators.exit 1


// return if the string is help option or not.
// string -> bool
let isHelpOption = function
    | "-h" | "--help" -> true
    | _ -> false

// return if the string is version option or not.
// string -> bool
let isVersionOption = function
    | "-v" | "--version" -> true
    | _ -> false

// return if the string is stdin option or not.
// string -> bool
let isStdinOption = function
    | "-i" | "--stdin" -> true
    | _ -> false

// return if the string is a valid option or not.
// string -> bool
let isInvalidOption (arg:string) =
    if arg.StartsWith("-") &&
          not (isHelpOption arg) &&
          not (isVersionOption arg) &&
          not (isStdinOption arg) then
       true
    else
       false


// printout a string
// (string -> unit)
let printString = printfn "%s"


[<EntryPoint>]
let main args = 
    if args.Length = 0 ||
           (isStdinOption args.[0] && args.Length = 1) ||
           isHelpOption args.[0] || 
           isInvalidOption args.[0] then
        // help option or invalid args
        printString usage
        1
    elif isVersionOption args.[0] then
        // version option
        printString version
        0
    elif isStdinOption args.[0] then
        // stdin (inline) option
        let input = String.concat " " args.[1..]
        let input' = input.Split( [|"/"|], System.StringSplitOptions.RemoveEmptyEntries )
                     |> Array.toList                     // "/" is treated as a line break
        let output = translate input'                    // translate strings
        printString (String.concat "\n" output)
        0
    else
        // argument is PDP-11 asm file name
        let fileName = args.[0]
        if System.IO.File.Exists fileName then
            let input = Array.toList (System.IO.File.ReadAllLines fileName)  // file to strings
            let output = translate input                                     // translate strings
            printString (String.concat "\n" output)
            0
        else
            printfn "'%s': No such file" fileName
            1

