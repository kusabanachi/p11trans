namespace Ack_i86

open System

module Label =

    let nameLabel name =
        name + ":"

    let numLabel (num: int16) =
        Convert.ToString num + ":"

