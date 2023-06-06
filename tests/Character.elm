module Character exposing (Character, Keys, fuzz, keys)

import Char.Order
import Fuzz exposing (Fuzzer)
import Int.Order
import Keys exposing (Keys)
import Map exposing (Mapping)
import N exposing (N2)
import Order
import Typed


type alias Character =
    { id : Int
    , char : Char
    }


type alias Keys =
    { id : Keys.Key Character (Order.By Id Int.Order.Up) Int N2
    , char : Keys.Key Character (Order.By CharTag (Char.Order.AToZ Char.Order.LowerUpper)) Char N2
    }


type Id
    = Id


type CharTag
    = CharTag


keys : Keys.Keys Character Keys N2
keys =
    Keys.for (\id_ char_ -> { id = id_, char = char_ })
        |> Keys.by ( .id, id ) Int.Order.up
        |> Keys.by ( .char, char )
            (Char.Order.aToZ Char.Order.lowerUpper)


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
