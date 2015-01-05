namespace Ack_i86

open System

module Label =

    let mutable private _uniqNum = 0
    let uniqName (prefix:string) =
        _uniqNum <- _uniqNum + 1
        sprintf "%s__%03d" prefix _uniqNum

    let nameLabel (name: string) =
        let labelName =
            if name.[0] = '~' then
                uniqName (name.Replace('~', '.'))
            else
                name
        labelName + ":"

    let numLabel (num: int16) =
        Convert.ToString num + ":"

