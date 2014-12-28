namespace Ack_i86

open Address

module TransStatus =

    type arg =
        | ArgSrc
        | ArgDest
        | ArgTempReg
        | ArgTempMem
        | ArgAddr of addr
        | ArgReg of reg

    type instructionType = | Word | Byte

    type transState = {
         iType: instructionType
         srcAddress: addr
         destAddress: addr
         tempReg: reg option
         preProcess: (transState -> string * transState) list;
         postProcess: (transState -> string * transState) list;
    }


    let extractCodeText status =
        let preCodeList, status' =
            List.fold (fun (codes, s) x -> let (code, s') = x s in code :: codes, s')
                      ([], status)
                      status.preProcess
        let postCodeList, _ =
            List.fold (fun (codes, s) x -> let (code, s') = x s in code :: codes, s')
                      ([], status')
                      status'.postProcess
        String.concat ";  " (List.rev (postCodeList @ preCodeList))


