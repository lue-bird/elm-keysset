module Atom exposing (Atom, Keys, keys)

import Char.Order
import Int.Order
import Keys exposing (Key, Keys)
import Map exposing (Mapping)
import N exposing (N0, N1, N2, To, Up)
import Order
import String.Order
import Typed


type alias Keys =
    { symbol :
        Key
            Atom
            (Order.By
                Symbol
                (String.Order.Earlier
                    (Char.Order.Alphabetically Char.Order.LowerUpper)
                )
            )
            String
            (Up N1 To N1)
    , atomicNumber :
        Key
            Atom
            (Order.By AtomicNumber Int.Order.Increasing)
            Int
            (Up N0 To N1)
    }


type alias Atom =
    { symbol : String
    , name : String
    , atomicNumber : Int
    }


type Symbol
    = Symbol


type AtomicNumber
    = AtomicNumber


keys : Keys.Keys Atom Keys N2
keys =
    Keys.for
        (\symbol_ atomicNumber_ ->
            { symbol = symbol_, atomicNumber = atomicNumber_ }
        )
        |> Keys.by ( .symbol, symbol )
            (String.Order.earlier
                (Char.Order.alphabetically
                    Char.Order.lowerUpper
                )
            )
        |> Keys.by ( .atomicNumber, atomicNumber )
            Int.Order.increasing


symbol : Mapping Atom Symbol String
symbol =
    Typed.tag Symbol .symbol


atomicNumber : Mapping Atom AtomicNumber Int
atomicNumber =
    Typed.tag AtomicNumber .atomicNumber
