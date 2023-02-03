module Character exposing (Character, Keys, fuzz, keys)

import Char.Order
import Fuzz exposing (Fuzzer)
import Int.Order
import Keys exposing (Keys)
import Map exposing (Mapping)
import N exposing (N0, N1, To, Up)
import Order
import Typed


type alias Character =
    { id : Int
    , char : Char
    }


type alias Keys =
    { id : Keys.Key Character (Order.By Id Int.Order.Increasing) Int (Up N1 To N1)
    , char : Keys.Key Character (Order.By CharTag (Char.Order.Alphabetically Char.Order.LowerUpper)) Char (Up N0 To N1)
    }


type Id
    = Id


type CharTag
    = CharTag


keys :
    Keys.Keys
        Character
        Keys
        N1
keys =
    Keys.for (\id_ char_ -> { id = id_, char = char_ })
        |> Keys.by ( .id, id ) Int.Order.increasing
        |> Keys.by ( .char, char )
            (Char.Order.alphabetically Char.Order.lowerUpper)


id : Mapping Character Id Int
id =
    Typed.tag Id .id


char : Mapping Character CharTag Char
char =
    Typed.tag CharTag .char


fuzz : Fuzzer Character
fuzz =
    Fuzz.constant (\id_ char_ -> { id = id_, char = char_ })
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.char
