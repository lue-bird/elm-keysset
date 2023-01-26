module BracketPair exposing (BracketPair, ByOpenClosed, byOpenClosed)

import Char.Order
import Keys
import N exposing (N0, N1, N2, To, Up, Up0, Up1, Up2)
import Order
import Record.Map


type alias ByOpenClosed =
    ( ( ()
      , Order.By
            Record.Map.Open
            ( Char.Order.Alphabetically, Char.Order.LowerUpper )
      )
    , Order.By
        Record.Map.Closed
        ( Char.Order.Alphabetically, Char.Order.LowerUpper )
    )


type alias BracketPair =
    { open : Char, closed : Char }


byOpenClosed :
    Keys.Keys
        BracketPair
        ByOpenClosed
        { open : Keys.Key BracketPair Char (Up N1 To N1)
        , closed : Keys.Key BracketPair Char (Up N0 To N1)
        }
        N1
byOpenClosed =
    Keys.for (\open closed -> { open = open, closed = closed })
        |> Keys.and .open
            ( Record.Map.open
            , Char.Order.alphabetically Char.Order.lowerUpper
            )
        |> Keys.and .closed
            ( Record.Map.closed
            , Char.Order.alphabetically Char.Order.lowerUpper
            )
