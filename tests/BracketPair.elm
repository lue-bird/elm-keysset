module BracketPair exposing (BracketPair, Keys, keys)

import Char.Order
import Keys
import Map exposing (Mapping)
import N exposing (N2)
import Order
import Typed


type alias Keys =
    { open :
        Keys.Key
            BracketPair
            (Order.By
                Open
                (Char.Order.AToZ Char.Order.LowerUpper)
            )
            Char
            N2
    , closed :
        Keys.Key
            BracketPair
            (Order.By
                Closed
                (Char.Order.AToZ Char.Order.LowerUpper)
            )
            Char
            N2
    }


type alias BracketPair =
    { open : Char, closed : Char }


type Open
    = Open


type Closed
    = Closed


keys : Keys.Keys BracketPair Keys N2
keys =
    Keys.for (\open_ closed_ -> { open = open_, closed = closed_ })
        |> Keys.by ( .open, open )
            (Char.Order.aToZ Char.Order.lowerUpper)
        |> Keys.by ( .closed, closed )
            (Char.Order.aToZ Char.Order.lowerUpper)


open : Mapping BracketPair Open Char
open =
    Typed.tag Open .open


closed : Mapping BracketPair Closed Char
closed =
    Typed.tag Closed .closed
