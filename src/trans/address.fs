namespace Ack_i86

open Expres

type reg =
    | AX | DX | CX | SI | DI | BP | SP | IP | BX

    member this.isMemoryAccessible =
        match this with
        | SI | DI | BP | BX -> true
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

type addr =
    | Reg of reg
    | IncDfr of reg
    | DecDfr of reg
    | Dfr of reg * expr option
    | IncDDfr of reg
    | DecDDfr of reg
    | DDfr of reg * expr option
    | Rel of expr
    | Imm of expr
    | RelDfr of expr
    | Abs of expr

    member this.isAccessible =
        match this with
        | IncDfr r | DecDfr r | Dfr (r, _)
            -> r.isMemoryAccessible
        | Reg _ | Rel _ | Imm _ | Abs _
            -> true
        | _
            -> false

    member this.isMemory =
        match this with
        | Reg _ | Imm _ -> false
        | _             -> true

    member this.isUsing (reg:reg) =
        match this with
        | Reg r
        | IncDfr  r | DecDfr  r | Dfr  (r, _)
        | IncDDfr r | DecDDfr r | DDfr (r, _)
            when r = reg -> true
        | _              -> false

    member this.text =
        match this with
        | Reg r           -> r.text
        | Dfr (r, Some e) -> sprintf "%A" e + "(" + r.text + ")"
        | Dfr (r, None)   -> "(" + r.text + ")"
        | Rel e | Abs e   -> sprintf "%A" e
        | Imm e           -> "#" + sprintf "%A" e
        | _               -> failwithf "Address error %A" this


module Address =

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



