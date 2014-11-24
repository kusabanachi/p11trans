namespace Ack_i86

module InstructionAsm =

    open Address
    open Expres

    let tempMem = "tmpMem"

    let inline (+!!+) (i1:string) (i2:string) =
        if i1.Length <> 0 && i2.Length <> 0 then
            i1 + ";  " + i2
        else
            i1 + i2

    let private movText (a1:addr) (a2:addr) =
        "mov " + a1.text + ", " + a2.text

    let private leaText (a1:addr) (a2:addr) =
        "lea " + a1.text + ", " + a2.text

    let private incText r1 r2 num =
        let index = Some (Expr_Dec (int16 num))
        leaText (Reg r1) (Dfr (r2, index))



    let moveRef (dReg:reg) sAddr =
        let midReg = if dReg.isMemoryAccessible then dReg else utilReg

        match sAddr with
        | IncDfr sReg ->
            if sReg.isMemoryAccessible then
                (movText (Reg dReg) (Reg sReg)
                   +!!+ incText sReg sReg 2,
                 Dfr (dReg, None))
            elif dReg.isMemoryAccessible then
                (movText (Reg dReg) (Reg sReg)
                   +!!+ incText sReg dReg 2,
                 Dfr (dReg, None))
            else
                (movText (Reg dReg) (Reg sReg)
                   +!!+ movText (Reg utilReg) (Reg sReg)
                   +!!+ incText sReg utilReg 2,
                 Dfr (dReg, None))
        | DecDfr sReg ->
            if sReg.isMemoryAccessible then
                (incText sReg sReg -2
                   +!!+ movText (Reg dReg) (Reg sReg),
                 Dfr (dReg, None))
            else
                (movText (Reg utilReg) (Reg sReg)
                   +!!+ incText sReg utilReg -2
                   +!!+ movText (Reg dReg) (Reg sReg),
                 Dfr (dReg, None))
        | Dfr (sReg, expr) ->
            (movText (Reg dReg) (Reg sReg),
             Dfr (dReg, expr))
        | IncDDfr sReg ->
            if sReg.isMemoryAccessible then
                (movText (Reg dReg) (Dfr (sReg, None))
                   +!!+ incText sReg sReg 2,
                 Dfr (dReg, None))
            else
                (movText (Reg midReg) (Reg sReg)
                   +!!+ incText sReg midReg 2
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Dfr (dReg, None))
        | DecDDfr sReg ->
            if sReg.isMemoryAccessible then
                (incText sReg sReg -2
                   +!!+ movText (Reg dReg) (Dfr (sReg, None)),
                 Dfr (dReg, None))
            else
                let decIndex = Some (Expr_Oct -2s)
                (movText (Reg midReg) (Reg sReg)
                   +!!+ incText sReg midReg -2
                   +!!+ movText (Reg dReg) (Dfr (midReg, decIndex)),
                 Dfr (dReg, None))
        | DDfr (sReg, expr) ->
            if sReg.isMemoryAccessible then
                (movText (Reg dReg) (Dfr (sReg, expr)),
                 Dfr (dReg, None))
            else
                (movText (Reg midReg) (Reg sReg)
                   +!!+ movText (Reg dReg) (Dfr (midReg, expr)),
                 Dfr (dReg, None))
        | Rel expr | Abs expr ->
            (movText (Reg dReg) (Imm expr),
             Dfr (dReg, None))
        | RelDfr expr ->
            (movText (Reg dReg) (Rel expr),
             Dfr (dReg, None))
        | _ ->
            failwithf "Invalid address"


    let moveVal (dReg:reg) sAddr =
        let midReg = if dReg.isMemoryAccessible then dReg else utilReg

        match sAddr with
        | IncDfr sReg ->
            if sReg.isMemoryAccessible then
                (movText (Reg dReg) (Dfr (sReg, None))
                   +!!+ incText sReg sReg 2,
                 Reg dReg)
            else
                (movText (Reg midReg) (Reg sReg)
                   +!!+ incText sReg midReg 2
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)
        | DecDfr sReg ->
            if sReg.isMemoryAccessible then
                (incText sReg sReg -2
                   +!!+ movText (Reg dReg) (Dfr (sReg, None)),
                 Reg dReg)
            else
                let decIndex = Some (Expr_Oct -2s)
                (movText (Reg midReg) (Reg sReg)
                   +!!+ incText sReg midReg -2
                   +!!+ movText (Reg dReg) (Dfr (midReg, decIndex)),
                 Reg dReg)
        | Dfr (sReg, expr) ->
            if sReg.isMemoryAccessible then
                (movText (Reg dReg) sAddr,
                 Reg dReg)
            else
                (movText (Reg midReg) (Reg sReg)
                   +!!+ movText (Reg dReg) (Dfr (midReg, expr)),
                 Reg dReg)
        | IncDDfr sReg ->
            if sReg.isMemoryAccessible then
                (movText (Reg midReg) (Dfr (sReg, None))
                   +!!+ incText sReg sReg 2
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)
            else
                (movText (Reg midReg) (Reg sReg)
                   +!!+ incText sReg midReg 2
                   +!!+ movText (Reg midReg) (Dfr (midReg, None))
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)
        | DecDDfr sReg ->
            if sReg.isMemoryAccessible then
                (incText sReg sReg -2
                   +!!+ movText (Reg midReg) (Dfr (sReg, None))
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)
            else
                let decIndex = Some (Expr_Oct -2s)
                (movText (Reg midReg) (Reg sReg)
                   +!!+ incText sReg midReg -2
                   +!!+ movText (Reg midReg) (Dfr (midReg, decIndex))
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)
        | DDfr (sReg, expr) ->
            if sReg.isMemoryAccessible then
                (movText (Reg midReg) (Dfr (sReg, expr))
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)
            else
                (movText (Reg midReg) (Reg sReg)
                   +!!+ movText (Reg midReg) (Dfr (midReg, expr))
                   +!!+ movText (Reg dReg) (Dfr (midReg, None)),
                 Reg dReg)
        | Reg _ | Rel _ | Abs _ | Imm _ ->
            (movText (Reg dReg) sAddr,
             Reg dReg)
        | RelDfr expr ->
            (movText (Reg midReg) (Rel expr)
               +!!+ movText (Reg dReg) (Dfr (midReg, None)),
             Reg dReg)


    let moveValToMem symbol sAddr =
        match sAddr with
        | IncDfr sReg ->
            if sReg.isMemoryAccessible then
                (movText (Reg utilReg) (Dfr (sReg, None))
                   +!!+ incText sReg sReg 2
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            else
                (movText (Reg utilReg) (Reg sReg)
                   +!!+ incText sReg utilReg 2
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
        | DecDfr sReg ->
            if sReg.isMemoryAccessible then
                (incText sReg sReg -2
                   +!!+ movText (Reg utilReg) (Dfr (sReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            else
                let decIndex = Some (Expr_Oct -2s)
                (movText (Reg utilReg) (Reg sReg)
                   +!!+ incText sReg utilReg -2
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, decIndex))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
        | Dfr (sReg, expr) ->
            if sReg.isMemoryAccessible then
                (movText (Reg utilReg) sAddr
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            else
                (movText (Reg utilReg) (Reg sReg)
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
        | IncDDfr sReg ->
            if sReg.isMemoryAccessible then
                (movText (Reg utilReg) (Dfr (sReg, None))
                   +!!+ incText sReg sReg 2
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            else
                (movText (Reg utilReg) (Reg sReg)
                   +!!+ incText sReg utilReg 2
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
        | DecDDfr sReg ->
            if sReg.isMemoryAccessible then
                (incText sReg sReg -2
                   +!!+ movText (Reg utilReg) (Dfr (sReg, None))
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            else
                let decIndex = Some (Expr_Oct -2s)
                (movText (Reg utilReg) (Reg sReg)
                   +!!+ incText sReg utilReg -2
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, decIndex))
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
        | DDfr (sReg, expr) ->
            if sReg.isMemoryAccessible then
                (movText (Reg utilReg) (Dfr (sReg, expr))
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
            else
                (movText (Reg utilReg) (Reg sReg)
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                   +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                   +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
                 Abs (Expr_Sym symbol))
        | Rel _ | Abs _ ->
            (movText (Reg utilReg) sAddr
               +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
             Abs (Expr_Sym symbol))
        | RelDfr expr ->
            (movText (Reg utilReg) (Rel expr)
               +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
               +!!+ movText (Abs (Expr_Sym symbol)) (Reg utilReg),
             Abs (Expr_Sym symbol))
        | Reg _ | Imm _ ->
            (movText (Abs (Expr_Sym symbol)) sAddr,
             Abs (Expr_Sym symbol))


    let pushVal sAddr =
        let pushText (a:addr) = "push " + a.text

        match sAddr with
        | IncDfr sReg ->
            if sReg.isMemoryAccessible then
                pushText (Dfr (sReg, None))
                  +!!+ incText sReg sReg 2
            else
                movText (Reg utilReg) (Reg sReg)
                  +!!+ incText sReg utilReg 2
                  +!!+ pushText (Dfr (utilReg, None))
        | DecDfr sReg ->
            if sReg.isMemoryAccessible then
                incText sReg sReg -2
                  +!!+ pushText (Dfr (sReg, None))
            else
                let decIndex = Some (Expr_Oct -2s)
                movText (Reg utilReg) (Reg sReg)
                  +!!+ incText sReg utilReg -2
                  +!!+ pushText (Dfr (utilReg, decIndex))
        | Dfr (sReg, expr) ->
            if sReg.isMemoryAccessible then
                pushText sAddr
            else
                movText (Reg utilReg) (Reg sReg)
                  +!!+ pushText (Dfr (utilReg, expr))
        | IncDDfr sReg ->
            if sReg.isMemoryAccessible then
                movText (Reg utilReg) (Dfr (sReg, None))
                  +!!+ pushText (Dfr (utilReg, None))
                  +!!+ incText sReg sReg 2
            else
                movText (Reg utilReg) (Reg sReg)
                  +!!+ incText sReg utilReg 2
                  +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                  +!!+ pushText (Dfr (utilReg, None))
        | DecDDfr sReg ->
            if sReg.isMemoryAccessible then
                incText sReg sReg -2
                  +!!+ movText (Reg utilReg) (Dfr (sReg, None))
                  +!!+ pushText (Dfr (utilReg, None))
            else
                let decIndex = Some (Expr_Oct -2s)
                movText (Reg utilReg) (Reg sReg)
                  +!!+ incText sReg utilReg -2
                  +!!+ movText (Reg utilReg) (Dfr (utilReg, decIndex))
                  +!!+ pushText (Dfr (utilReg, None))
        | DDfr (sReg, expr) ->
            if sReg.isMemoryAccessible then
                movText (Reg utilReg) sAddr
                  +!!+ pushText (Dfr (utilReg, None))
            else
                movText (Reg utilReg) (Reg sReg)
                  +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                  +!!+ pushText (Dfr (utilReg, None))
        | Reg _ | Rel _ | Abs _ ->
            pushText sAddr
        | RelDfr expr ->
            movText (Reg utilReg) (Rel expr)
              +!!+ pushText (Dfr (utilReg, None))
        | Imm _ ->
            movText (Reg utilReg) sAddr
              +!!+ pushText (Dfr (utilReg, None))


    let popValTo dAddr =
        let popText (a:addr) = "pop " + a.text

        match dAddr with
        | IncDfr dReg ->
            if dReg.isMemoryAccessible then
                popText (Dfr (dReg, None))
                  +!!+ incText dReg dReg 2
            else
                movText (Reg utilReg) (Reg dReg)
                  +!!+ incText dReg utilReg 2
                  +!!+ popText (Dfr (utilReg, None))
        | DecDfr dReg ->
            if dReg.isMemoryAccessible then
                incText dReg dReg -2
                  +!!+ popText (Dfr (dReg, None))
            else
                let decIndex = Some (Expr_Oct -2s)
                movText (Reg utilReg) (Reg dReg)
                  +!!+ incText dReg utilReg -2
                  +!!+ popText (Dfr (utilReg, decIndex))
        | Dfr (dReg, expr) ->
            if dReg.isMemoryAccessible then
                popText dAddr
            else
                movText (Reg utilReg) (Reg dReg)
                  +!!+ popText (Dfr (utilReg, expr))
        | IncDDfr dReg ->
            if dReg.isMemoryAccessible then
                movText (Reg utilReg) (Dfr (dReg, None))
                  +!!+ popText (Dfr (utilReg, None))
                  +!!+ incText dReg dReg 2
            else
                movText (Reg utilReg) (Reg dReg)
                  +!!+ incText dReg utilReg 2
                  +!!+ movText (Reg utilReg) (Dfr (utilReg, None))
                  +!!+ popText (Dfr (utilReg, None))
        | DecDDfr dReg ->
            if dReg.isMemoryAccessible then
                incText dReg dReg -2
                  +!!+ movText (Reg utilReg) (Dfr (dReg, None))
                  +!!+ popText (Dfr (utilReg, None))
            else
                let decIndex = Some (Expr_Oct -2s)
                movText (Reg utilReg) (Reg dReg)
                  +!!+ incText dReg utilReg -2
                  +!!+ movText (Reg utilReg) (Dfr (utilReg, decIndex))
                  +!!+ popText (Dfr (utilReg, None))
        | DDfr (dReg, expr) ->
            if dReg.isMemoryAccessible then
                movText (Reg utilReg) dAddr
                  +!!+ popText (Dfr (utilReg, None))
            else
                movText (Reg utilReg) (Reg dReg)
                  +!!+ movText (Reg utilReg) (Dfr (utilReg, expr))
                  +!!+ popText (Dfr (utilReg, None))
        | Reg _ | Rel _ | Abs _ ->
            popText dAddr
        | RelDfr expr ->
            movText (Reg utilReg) (Rel expr)
              +!!+ popText (Dfr (utilReg, None))
        | _ ->
            failwithf "Invalid address: pop to %A" dAddr


    let incrementReg (reg:reg) num =
        if reg.isMemoryAccessible then
            incText reg reg num
        else
            movText (Reg utilReg) (Reg reg)
              +!!+ incText reg utilReg num


    let binaryCalc codeStr (dAddr:addr) (sAddr:addr) =
        let codeText (a1:addr) (a2:addr) =
            codeStr + " " + a1.text + ", " + a2.text

        match sAddr with
        | IncDfr sReg ->
            codeText dAddr (Dfr (sReg, None))
              +!!+ incText sReg sReg 2
        | DecDfr sReg ->
            incText sReg sReg -2
              +!!+ codeText dAddr (Dfr (sReg, None))
        | _ ->
            match dAddr with
            | IncDfr dReg ->
                codeText (Dfr (dReg, None)) sAddr
                  +!!+ incText dReg dReg 2
            | DecDfr dReg ->
                incText dReg dReg -2
                  +!!+ codeText (Dfr (dReg, None)) sAddr
            | _ ->
                codeText dAddr sAddr


    let unaryCalc codeStr addr =
        let codeText (a:addr) =
            codeStr + " " + a.text

        match addr with
        | IncDfr reg ->
            codeText (Dfr (reg, None))
              +!!+ incText reg reg 2
        | DecDfr reg ->
            incText reg reg -2
              +!!+ codeText (Dfr (reg, None))
        | _ ->
            codeText addr


    let storeRegVal reg =
        movText (Abs (Expr_Sym tempMem)) (Reg reg)

    let restoreRegVal reg =
        movText (Reg reg) (Abs (Expr_Sym tempMem))


