module Character exposing (ByIdOrChar, Character, byIdOrChar, fuzz)

import Char.Order
import Fuzz exposing (Fuzzer)
import Int.Order
import Keys exposing (Keys)
import N exposing (N0, N1, N2, To, Up, Up0, Up1, Up2)
import Order
import Record.Map


type alias Character =
    { id : Int
    , char : Char
    }


type alias ByIdOrChar =
    ( ( ()
      , Order.By Record.Map.Id Int.Order.Increasing
      )
    , Order.By
        Record.Map.Char
        ( Char.Order.Alphabetically, Char.Order.LowerUpper )
    )


byIdOrChar :
    Keys
        Character
        ByIdOrChar
        { id : Keys.Key Character Int (Up N1 To N1)
        , char : Keys.Key Character Char (Up N0 To N1)
        }
        N1
byIdOrChar =
    Keys.for (\id char -> { id = id, char = char })
        |> Keys.and .id ( Record.Map.id, Int.Order.increasing )
        |> Keys.and .char
            ( Record.Map.char
            , Char.Order.alphabetically
                Char.Order.lowerUpper
            )


fuzz : Fuzzer Character
fuzz =
    Fuzz.constant (\id char -> { id = id, char = char })
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.char
