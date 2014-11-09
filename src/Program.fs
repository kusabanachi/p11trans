
// NOTE: If warnings appear, you may need to retarget this project to .NET 4.0. Show the Solution
// Pad, right-click on the project node, choose 'Options --> Build --> General' and change the target
// framework to .NET 4.0 or .NET 4.5.

module p11trans.Main

open System
open ReadOp
open Expres
open Opline
open Assem

open System.IO

[<EntryPoint>]
let main args =
    let fileName = args.[0]
    //let fileName = "printf.s"
    if File.Exists fileName then
        let srcLines = File.ReadAllLines fileName  // file to strings
        srcLines |> Array.iter (fun src ->
                                    printfn "%s" src
                                    if src.Length <> 0 then
                                           let dest = assem (src + "\n")
                                           printfn "    %A" dest)
//        srcLines |> Array.iteri (fun i src ->
//                                    printfn "%s" src
//                                    if src.Length <> 0 then
//                                       try
//                                           let dest = assem (src + "\n")
//                                           printfn "    %A" dest
//                                       with
//                                           | :? System.Exception -> eprintfn "%s, %d" fileName i
//                                                                    reraise())
        0
    else
        printfn "No such file."
        0

    //printfn "%A" (assem "pof: sys seek")
    //printfn "%A" (assem ";02020\n")
    //printfn "%A" (readOp "<\\n\\t\\e\\0\\r\\a\\p\\\\>")

