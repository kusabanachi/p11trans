namespace Ack_i86

module Label =

    open System

    let nameLabel name =
        name + ":"

    let numLabel (num: int16) =
        Convert.ToString num + ":"

