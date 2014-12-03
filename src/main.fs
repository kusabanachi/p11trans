
module p11trans.Main

open System
open System.IO
open V6as.Assem
open Ack_i86.Ack_i86_trans


[<EntryPoint>]
let main args =

    let translate fileName =
        let inputLines = File.ReadAllLines fileName
        inputLines |>
            Array.iter (fun src ->
                            if src.Length = 0 then
                                printfn ""
                            if src.Length <> 0 then
                                let output = ack_i86_asm (assem (src + "\n"))
                                printf "%s" output
                                if src.[0] <> '/' then
                                    printfn "        ! %s" src
                       )

    if args.Length = 0 then
        let fileName = "write.s"
        translate fileName
        0
    elif args.[0] <> "-i" then
        let fileName = args.[0]
        if File.Exists fileName then
            translate fileName
            0
        else
            printfn "'%s': No such file" fileName
            1
    else
        let input = String.concat " " args.[1..]
        let output = ack_i86_asm (assem input)
        printfn "%s" output
        0


(*    //let fileName = args.[0]
    let fileName = "write.s"
    if File.Exists fileName then
        let srcLines = File.ReadAllLines fileName  // file to strings
        srcLines |> Array.iter (fun src ->
                                    if src.Length <> 0 then
                                           let dest = assem (src + "\n")
                                           //printfn "    %A" dest
                                           let trans = ack_i86_asm dest
                                           printfn "    %A" trans
                                           )
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
*)
    (*printfn "%A" ( ack_i86_asm (assem "mov r0, r1"))
    printfn "%A" ( ack_i86_asm (assem "mov (r1), r1"))
    printfn "%A" ( ack_i86_asm (assem "mov -(r4), r1"))
    printfn "%A" ( ack_i86_asm (assem "mov -(r1), r1"))
    printfn "%A" ( ack_i86_asm (assem "mov (r1)+, r0"))
    printfn "%A" ( ack_i86_asm (assem "mov (r1)+, -(sp)"))
    printfn "%A" ( ack_i86_asm (assem "mov (r1)+, (sp)"))
    printfn "%A" ( ack_i86_asm (assem "mov (sp)+, (r2)"))
    printfn "%A" ( ack_i86_asm (assem "mov *(r1)+, (r4)"))
    printfn "%A" ( ack_i86_asm (assem "mov *(r1)+, *-(r2)"))
    printfn "%A" ( ack_i86_asm (assem "mov *(r1)+, *-(sp)"))

    let f s =
        let (a,_,_) = expres s
        Ack_i86.Express.expr a
    printfn "%A" ( f " 1 2 3 ^ 7 * 8 + 0 * 2")
    printfn "%A" ( f "1 * 91. - 2f")
    printfn "%A" ( f "1\%1")
    printfn "%A" ( f "1!0")
    printfn "%A" ( f "5 6 5")
    printfn "%A" ( f "3[2.*[3 2.]]")
    printfn "%A" ( ack_i86_asm (assem "tst *(r1)+"))
    printfn "%A" ( ack_i86_asm (assem "cmp *(r1)+, *-(sp)"))
    printfn "%A" ( ack_i86_asm (assem "bisb r0, *(r1)"))
    printfn "%A" ( ack_i86_asm (assem "bisb *(r1), r0"))
    printfn "%A" ( ack_i86_asm (assem "bisb *(r1), r2"))
    printfn "%A" ( ack_i86_asm (assem "bisb *(r1)+, *-(sp)"))
    //printfn "%A" (readOp "<\\n\\t\\e\\0\\r\\a\\p\\\\>")
    0
    *)

