module Character exposing (ById, Character, byId, fuzz)

import Fuzz exposing (Fuzzer)
import Int.Order
import KeySet


type alias Character =
    { id : Int, char : Char }


type ById
    = ById


byId : KeySet.Sorting Character Int ById
byId =
    KeySet.sortingKey .id { tag = ById, order = Int.Order.increasing }


fuzz : Fuzzer Character
fuzz =
    Fuzz.constant (\id char -> { id = id, char = char })
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.char
