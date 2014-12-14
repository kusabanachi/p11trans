namespace Ack_i86

open System

module Label =

    let mutable private _uniqNum = 999
    let uniqName (prefix:string) =
        let num: int32 = _uniqNum
        _uniqNum <- _uniqNum - 1
        sprintf "%s__%03d" prefix num

    let nameLabel (name: string) =
        let labelName =
            if name.[0] = '~' then
                uniqName (name.Replace('~', '.'))
            else
                name
        labelName + ":"

    let numLabel (num: int16) =
        Convert.ToString num + ":"

