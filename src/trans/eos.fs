namespace Ack_i86

module Eos =

    let eos = function
        | '\n' -> "\n"
        | ';'  -> ";  "
        | _    -> ""

