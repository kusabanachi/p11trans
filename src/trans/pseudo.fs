namespace Ack_i86

open Express

module Pseudo =

    let ascii str = ".ascii \"" + str + "\""

    let data1 byteExprs =
        let exprsStr = String.concat ", " (List.map expr byteExprs)
        ".data1 " + exprsStr

    let even = ".align 2"

    let globalSym names =
        let namesStr = String.concat ", " names
        ".extern " + namesStr

    let text = ".sect .text"
    let data = ".sect .data"
    let bss  = ".sect .bss"

    let common name ex =
        ".comm " + name + ", " + expr ex

    let data2 ex =
        ".data2 " + expr ex

