module p11trans.utility

open System.Text.RegularExpressions     // use regular expression


// debug print
let dPrint a = printfn "Debug: %A" a


// try to get regular expression match.
// string -> string -> System.Text.RegularExpressions.Match option 
let (|RegexMatch|_|) pattern str =
    let rMatch = Regex(pattern).Match(str)
    if rMatch.Success then
        Some rMatch
    else
        None


// check if the string is blank string.
// string -> bool
let isBlankString = System.String.IsNullOrWhiteSpace


// get 2 words of argument.
// string -> (string * string) option
let getTwoArgs str =
    let twoWordPtn = "^\s*(\S+)\s*,\s*(\S+)\s*$" // xxx , xxx
    match str with
    | RegexMatch twoWordPtn rMatch ->
        Some( rMatch.Groups.[1].Value, rMatch.Groups.[2].Value )
    | _ ->
        None


// adapt arguments to function.
// ('a -> 'b) -> 'a option -> 'b option
let adaptArgs f = function
    | Some args -> Some(f(args))
    | _ -> None


// concatenate two instruction texts.
// string -> string -> string
let inline (+!!+) (i1:string) (i2:string) =
    if i1.Length <> 0 && i2.Length <> 0 then
        i1 + ";  " + i2
    else
        i1 + i2


// maybe monad
type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | Some y -> f y
        | None -> None

    member this.Return(x) =
        Some x

let maybe = MaybeBuilder()


