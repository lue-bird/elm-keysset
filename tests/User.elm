module User exposing (ByEmail, User, byEmail)

import Char.Order
import Keys exposing (Keys)
import Map exposing (Mapping)
import N exposing (N0, To, Up)
import Order
import String.Order
import Typed


type alias User =
    { username : String
    , priority : Int
    , email : String
    }


type alias ByEmail =
    { email :
        Keys.Key
            User
            (Order.By
                Email
                (String.Order.Earlier (Char.Order.Alphabetically Char.Order.LowerUpper))
            )
            String
            (Up N0 To N0)
    }


type Email
    = Email


byEmail : Keys User ByEmail N0
byEmail =
    Keys.for (\email_ -> { email = email_ })
        |> Keys.by ( .email, email )
            (String.Order.earlier
                (Char.Order.alphabetically Char.Order.lowerUpper)
            )


email : Mapping User Email String
email =
    Typed.tag Email .email
