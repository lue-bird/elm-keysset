module Bracket exposing (Bracket, Keys, keys)

import Char.Order
import Keys exposing (Key)
import Map exposing (Mapping)
import N exposing (N0, N1, N2, To, Up)
import Order


type alias Bracket =
    { open : Char, closed : Char }


type alias Keys =
    { open : Key Bracket (Order.By Open Char.Order.Unicode) Char (Up N1 To N1)
    , closed : Key Bracket (Order.By Closed Char.Order.Unicode) Char (Up N0 To N1)
    }


keys : Keys.Keys Bracket Keys N2
keys =
    Keys.for (\open_ closed_ -> { open = open_, closed = closed_ })
        |> Keys.by ( .open, open ) Char.Order.unicode
        |> Keys.by ( .closed, closed ) Char.Order.unicode


type Open
    = Open


open : Mapping Bracket Open Char
open =
    Map.tag Open .open


type Closed
    = Closed


closed : Mapping Bracket Closed Char
closed =
    Map.tag Closed .closed
