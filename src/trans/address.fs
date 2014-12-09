namespace Ack_i86

open ExpressionType
open Express
open V6as

module Address =

    type reg =
        | AX | DX | CX | SI | DI | BP | SP | IP | BX

        member this.isMemoryAccessible =
            match this with
            | SI | DI | BP | BX -> true
            | _                 -> false

        member this.has8bitPart =
            match this with
            | AX | BX | CX | DX -> true
            | _                 -> false

        member this.text =
            match this with
            | AX -> "ax"
            | DX -> "dx"
            | CX -> "cx"
            | SI -> "si"
            | DI -> "di"
            | BP -> "bp"
            | SP -> "sp"
            | IP -> "ip"
            | BX -> "bx"

        member this.l8bitText =
            match this with
            | AX -> "al"
            | BX -> "bl"
            | CX -> "cl"
            | DX -> "dl"
            | _ -> failwithf "Byte Address error %A" this


    type addr =
        | Reg of reg
        | IncDfr of reg
        | DecDfr of reg
        | Dfr of reg * expression option
        | IncDDfr of reg
        | DecDDfr of reg
        | DDfr of reg * expression option
        | Rel of expression
        | Imm of expression
        | RelDfr of expression
        | Abs of expression

        member this.isAccessible =
            match this with
            | IncDfr r | DecDfr r | Dfr (r, _)
                -> r.isMemoryAccessible
            | Reg _ | Rel _ | Imm _ | Abs _
                -> true
            | _
                -> false

        member this.isByteAccessible =
            match this with
            | Reg r
                -> r.has8bitPart
            | IncDfr r | DecDfr r | Dfr (r, _)
                -> r.isMemoryAccessible
            | Rel _ | Imm _ | Abs _
                -> true
            | _
                -> false

        member this.isMemory =
            match this with
            | Reg _ | Imm _ -> false
            | _             -> true

        member this.isRegValue =
            match this with
            | Reg _ -> true
            | _     -> false

        member this.isImmediate =
            match this with
            | Imm _ -> true
            | _     -> false

        member this.isUsing (reg:reg) =
            match this with
            | Reg r
            | IncDfr  r | DecDfr  r | Dfr  (r, _)
            | IncDDfr r | DecDDfr r | DDfr (r, _)
                when r = reg -> true
            | _              -> false

        member this.isIncrement =
            match this with
            | IncDfr  _ | IncDDfr _ -> true
            | _                     -> false

        member this.isDecrement =
            match this with
            | DecDfr  _ | DecDDfr _ -> true
            | _                     -> false

        member this.text =
            match this with
            | Reg r           -> r.text
            | Dfr (r, Some e) -> expr e + "(" + r.text + ")"
            | Dfr (r, None)   -> "(" + r.text + ")"
            | Rel e | Abs e   -> expr e
            | Imm e           -> "#" + expr e
            | _               -> failwithf "Address error %A" this

        member this.byteText =
            match this with
            | Reg r -> r.l8bitText
            | _     -> this.text



    let utilReg = BX


    let i86Addr pdp11Addr =
        let i86Reg = function
            | Addres.R0 -> AX
            | Addres.R1 -> DX
            | Addres.R2 -> CX
            | Addres.R3 -> SI
            | Addres.R4 -> DI
            | Addres.R5 -> BP
            | Addres.SP -> SP
            | Addres.PC -> IP

        match pdp11Addr with
        | Addres.Reg r         -> Reg     (i86Reg r)
        | Addres.IncDfr r      -> IncDfr  (i86Reg r)
        | Addres.DecDfr r      -> DecDfr  (i86Reg r)
        | Addres.IdxDfr (r,i)  -> Dfr     (i86Reg r, Some i)
        | Addres.Dfr r         -> Dfr     (i86Reg r, None)
        | Addres.IncDDfr r     -> IncDDfr (i86Reg r)
        | Addres.DecDDfr r     -> DecDDfr (i86Reg r)
        | Addres.DDfr r        -> DDfr    (i86Reg r, None)
        | Addres.IdxDDfr (r,i) -> DDfr (i86Reg r, Some i)
        | Addres.Rel e         -> Rel e
        | Addres.Imm e         -> Imm e
        | Addres.RelDfr e      -> RelDfr e
        | Addres.Abs e         -> Abs e


    let nextReg = function
        | AX -> DX
        | DX -> CX
        | CX -> SI
        | SI -> DI
        | DI -> BP
        | _  -> failwith "Invalid register"


    let dfr reg      = Dfr (reg, None)
    let idfr reg num = Dfr (reg, Some (Expr_Dec (int16 num)))
    let ddfr reg     = DDfr (reg, None)
    let namedMem name = Abs (Expr_Sym name)

