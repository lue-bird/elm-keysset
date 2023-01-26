module Atom exposing (Atom, ByNumberOrSymbol, byNumberOrSymbol)

import Char.Order
import Int.Order
import Keys exposing (Key, Keys)
import N exposing (N0, N1, N2, To, Up, Up0, Up1, Up2)
import Order
import Record.Map
import String.Order


type alias ByNumberOrSymbol =
    ( ( ()
      , Order.By
            Record.Map.Symbol
            ( String.Order.GreaterEarlier
            , ( Char.Order.Alphabetically, Char.Order.LowerUpper )
            )
      )
    , Order.By
        Record.Map.AtomicNumber
        Int.Order.Increasing
    )


type alias Atom =
    { symbol : String
    , name : String
    , atomicNumber : Int
    }


byNumberOrSymbol :
    Keys
        Atom
        ByNumberOrSymbol
        { symbol : Key Atom String (Up N1 To N1)
        , atomicNumber : Key Atom Int (Up N0 To N1)
        }
        N1
byNumberOrSymbol =
    Keys.for
        (\symbol atomicNumber ->
            { symbol = symbol, atomicNumber = atomicNumber }
        )
        |> Keys.and .symbol
            ( Record.Map.symbol
            , String.Order.greaterEarlier
                (Char.Order.alphabetically
                    Char.Order.lowerUpper
                )
            )
        |> Keys.and .atomicNumber
            ( Record.Map.atomicNumber
            , Int.Order.increasing
            )
