namespace Ack_i86

open Express
open V6as.Expres

module Assign =

    let inline (<|>) o1 o2 =
        match o1 with
        | None -> o2
        | _    -> o1

    let rec private tryEraseLocationCounter = function
        | Expr_Op ('+', Expr_Sym ".", rhs)
        | Expr_Op ('+', Expr_Group (Expr_Sym "."), rhs) ->
            Some rhs
        | Expr_Op ('+', lhs, Expr_Sym ".")
        | Expr_Op ('+', lhs, Expr_Group (Expr_Sym ".")) ->
            Some lhs
        | Expr_Op ('-', Expr_Sym ".", rhs)
        | Expr_Op ('-', Expr_Group (Expr_Sym "."), rhs) ->
            Some (Expr_Op ('-', Expr_Dec 0s, rhs))
        | Expr_Op ('+', lhs, rhs) ->
            Option.bind (fun lhs' -> Some (Expr_Op ('+', lhs', rhs)))
                        (tryEraseLocationCounter lhs)
            <|>
            Option.bind (fun rhs' -> Some (Expr_Op ('+', lhs, rhs')))
                        (tryEraseLocationCounter rhs)
        | Expr_Op ('-', lhs, rhs) ->
            Option.bind (fun lhs' -> Some (Expr_Op ('-', lhs', rhs)))
                        (tryEraseLocationCounter lhs)
        | Expr_Group ex ->
            Option.bind (fun ex' -> Some (Expr_Group ex'))
                        (tryEraseLocationCounter ex)
        | _ ->
            None

    let assign symStr ex =
        if symStr <> "." then
            symStr + " = " + expr ex
        else
            match tryEraseLocationCounter ex with
            | Some ex' -> ".space " + expr ex'
            | _ -> failwith "the assignment of location counter was not able to translated"

