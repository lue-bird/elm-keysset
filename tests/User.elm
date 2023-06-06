module User exposing (ByEmail, User, byEmail)

import Char.Order
import Keys exposing (Keys)
import Map exposing (Mapping)
import N exposing (N1)
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
            (Order.By Email (String.Order.Earlier (Char.Order.AToZ Char.Order.LowerUpper)))
            String
            N1
    }


type Email
    = Email


byEmail : Keys User ByEmail N1
byEmail =
    Keys.for (\email_ -> { email = email_ })
        |> Keys.by ( .email, email )
            (String.Order.earlier
                (Char.Order.aToZ Char.Order.lowerUpper)
            )


email : Mapping User Email String
email =
    Typed.tag Email .email
