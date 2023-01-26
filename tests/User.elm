module User exposing (ByEmail, User, byEmail)

import Char.Order
import Keys exposing (Keys)
import N exposing (N0, N1, N2, To, Up, Up0, Up1)
import Order
import Record.Map
import String.Order


type alias User =
    { username : String
    , priority : Int
    , email : String
    }


type alias ByEmail =
    ( ()
    , Order.By
        Record.Map.Email
        ( String.Order.GreaterEarlier
        , ( Char.Order.Alphabetically, Char.Order.LowerUpper )
        )
    )


byEmail :
    Keys
        User
        ByEmail
        { email : Keys.Key User String (Up N0 To N0) }
        N0
byEmail =
    Keys.for (\email -> { email = email })
        |> Keys.and .email
            ( Record.Map.email
            , String.Order.greaterEarlier
                (Char.Order.alphabetically
                    Char.Order.lowerUpper
                )
            )
