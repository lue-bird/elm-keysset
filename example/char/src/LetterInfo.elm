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
    { lowercase : Keys.Key LetterInfo (Order.By Lowercase (Char.Order.AToZ Order.Tie)) Char N3
    , uppercase : Keys.Key LetterInfo (Order.By Uppercase (Char.Order.AToZ Order.Tie)) Char N3
    , inAlphabet : Keys.Key LetterInfo (Order.By InAlphabet Int.Order.Up) Int N3
    }


keys : Keys.Keys LetterInfo Keys N3
keys =
    Keys.for
        (\lowercase_ uppercase_ inAlphabet_ ->
            { lowercase = lowercase_, uppercase = uppercase_, inAlphabet = inAlphabet_ }
        )
        |> Keys.by ( .lowercase, lowercase ) (Char.Order.aToZ Order.tie)
        |> Keys.by ( .uppercase, uppercase ) (Char.Order.aToZ Order.tie)
        |> Keys.by ( .inAlphabet, inAlphabet ) Int.Order.up


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
