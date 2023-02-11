module LetterInfo exposing (Keys, LetterInfo, keys)

import Char.Order
import Int.Order
import Keys
import Map exposing (Mapping)
import N exposing (N0, N1, N2, N3, To, Up)
import Order


type alias LetterInfo =
    { lowercase : Char
    , uppercase : Char
    , inAlphabet : Int
    }


type alias Keys =
    { lowercase : Keys.Key LetterInfo (Order.By Lowercase (Char.Order.Alphabetically Order.Tie)) Char (Up N2 To N2)
    , uppercase : Keys.Key LetterInfo (Order.By Uppercase (Char.Order.Alphabetically Order.Tie)) Char (Up N1 To N2)
    , inAlphabet : Keys.Key LetterInfo (Order.By InAlphabet Int.Order.Increasing) Int (Up N0 To N2)
    }


keys : Keys.Keys LetterInfo Keys N3
keys =
    Keys.for
        (\lowercase_ uppercase_ inAlphabet_ ->
            { lowercase = lowercase_, uppercase = uppercase_, inAlphabet = inAlphabet_ }
        )
        |> Keys.by ( .lowercase, lowercase ) (Char.Order.alphabetically Order.tie)
        |> Keys.by ( .uppercase, uppercase ) (Char.Order.alphabetically Order.tie)
        |> Keys.by ( .inAlphabet, inAlphabet ) Int.Order.increasing


type Lowercase
    = Lowercase


lowercase : Mapping LetterInfo Lowercase Char
lowercase =
    Map.tag Lowercase .lowercase


type Uppercase
    = Uppercase


uppercase : Mapping LetterInfo Uppercase Char
uppercase =
    Map.tag Uppercase .uppercase


type InAlphabet
    = InAlphabet


inAlphabet : Mapping LetterInfo InAlphabet Int
inAlphabet =
    Map.tag InAlphabet .inAlphabet
