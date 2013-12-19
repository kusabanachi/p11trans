module p11trans.ackTempMemText

(* resolve temp memory declaration *)

open p11trans.intermediate
open p11trans.ackInstructionText


// return if there is temp mem description or not.
// string list -> bool
let isThereTempMem textList =
        List.exists (fun (line:string) -> line.Contains tempValMem) textList


// get last section from the information of intermediate expression.
// ((string * statementElement option) list * string option) list -> statementElement option
let getLastSection intermediate =
    let getSectionPseudo (_, statement) =
        match statement with
        | Some(Pseudo(SectText)) | Some(Pseudo(SectData)) | Some(Pseudo(SectBss))
            -> statement
        | _
            -> None
    List.tryPick
        (fun (exComm,_) -> (List.tryPick getSectionPseudo (List.rev exComm)))
        (List.rev intermediate)


// get pseudo-op text of designating data section.
// string
let getDataSectionText =
    ackPseudo.getPseudoText SectData


// get temp memory's data declaration text.
// string
let getTempMemText =
    let tempMemExpr = Expr(tempValMem)
    "         .data2 " + ackExpression.getExpressionStr tempMemExpr


// resolve temp memory's data declaration.
// string list
//     -> ((string * statementElement option) list * string option) list
//     -> string list
let resolveTempMemData outTextLines intermediate =
    if isThereTempMem outTextLines then
        if getLastSection intermediate <> Some(Pseudo(SectData)) then
            outTextLines @ [getDataSectionText; getTempMemText]
        else
            outTextLines @ [getTempMemText]
    else
        outTextLines

