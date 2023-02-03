module User exposing (ByEmail, User, byEmail)

import Char.Order
import Keys exposing (Keys)
import Map exposing (Mapping)
import N exposing (N0, N1, N2, To, Up, Up0, Up1)
import Order
import String.Order
import Typed


type alias User =
    { username : String
    , priority : Int
    , email : String
    }


type alias ByEmail =
    ( ()
    , Order.By
        Email
        (String.Order.Earlier (Char.Order.Alphabetically Char.Order.LowerUpper))
    )


type Email
    = Email


email : Mapping User Email String
email =
    Typed.tag Email .email


byEmail :
    Keys
        User
        ByEmail
        { email : Keys.Key User String (Up N0 To N0) }
        N0
byEmail =
    Keys.for (\email_ -> { email = email_ })
        |> Keys.by ( .email, email )
            (String.Order.earlier
                (Char.Order.alphabetically Char.Order.lowerUpper)
            )
