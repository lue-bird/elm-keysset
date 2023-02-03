module KeysSet.Tag exposing (For, for)

{-| [`for`](#for) ensures that the keys type argument in [`For`](#For)
matches with an actual [`Keys`](#Keys) record
-}

import Keys exposing (Keys)


type For keys
    = KeysSet


for : Keys element keys lastIndex -> For keys
for _ =
    KeysSet
