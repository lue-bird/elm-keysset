module KeysSet.Tag exposing (For, for, forIdentity)

{-| [`for`](#for) ensures that the keys type argument in [`For`](#For)
matches with an actual [`Keys`](#Keys) record

@docs For, for, forIdentity

-}

import Keys exposing (Key, Keys)
import Map
import N exposing (N0, To, Up)
import Order


type For keys
    = KeysSet


for : Keys element keys lastIndex -> For keys
for _ =
    KeysSet


forIdentity :
    ( Keys element keys lastIndex
    , keys -> Key element (Order.By toKeyTag_ orderTag) key index_
    )
    -> For (Key key (Order.By Map.Identity orderTag) key (Up N0 To N0))
forIdentity _ =
    KeysSet
