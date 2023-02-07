module KeysSet.Internal exposing (KeysSet(..), KeysSetTag, Multiple, fromMultiple, one, size, tagFor, tagForIdentity, toMultiple, treeForIndex)

{-| Helps keeping the scope of unsafe operations like creating a tag narrow.
This setup also improves testability (accessing internal trees for validation for example)
-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable, fill, filled)
import Keys exposing (Key, Keys)
import Linear exposing (Direction(..))
import Map
import N exposing (Add1, Exactly, In, N, N0, To, Up, n0)
import Order
import Tree2
import Typed exposing (Checked, Public, Typed)


type KeysSet element keys lastIndex
    = One element
    | Multiple (Multiple element keys lastIndex)


{-| Underlying representation of a [`KeysSet`](#KeysSet) with some elements
-}
type alias Multiple element keys lastIndex =
    Typed
        Checked
        (KeysSetTag keys)
        Public
        { size : Int
        , byKeys :
            ArraySized (Tree2.Branch element) (Exactly (Add1 lastIndex))
        }


{-| Tag that verifies a [`KeysSet`](#KeysSet) is created by this module.
You don't need to know more but I you're interested,
check [`Typed.Checked`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/Typed#Checked)
-}
type KeysSetTag keys
    = KeysSet


treeForIndex :
    N (In min_ (Up maxToLastIndex_ To lastIndex))
    ->
        (KeysSet element keys_ lastIndex
         -> Emptiable (Tree2.Branch element) never_
        )
treeForIndex index =
    \keysSet ->
        case keysSet of
            One singleElement ->
                singleElement |> Tree2.one

            Multiple multiple ->
                multiple
                    |> Typed.untag
                    |> .byKeys
                    |> ArraySized.inToOn
                    |> ArraySized.element ( Up, index )
                    |> Emptiable.filled


tagFor : Keys element_ keys lastIndex_ -> KeysSetTag keys
tagFor _ =
    KeysSet


tagForIdentity :
    ( Keys element keys lastIndex_
    , keys -> Key element (Order.By toKeyTag_ orderTag) key index_
    )
    -> KeysSetTag (Key key (Order.By Map.Identity orderTag) key (Up N0 To N0))
tagForIdentity _ =
    KeysSet


toMultiple :
    Keys element keys lastIndex
    ->
        (KeysSet element keys lastIndex
         -> Multiple element keys lastIndex
        )
toMultiple keys =
    \keysSet ->
        case keysSet of
            One singleElement ->
                Typed.tag (tagFor keys)
                    { size = 1
                    , byKeys =
                        keys
                            |> Keys.toArray
                            |> ArraySized.inToNumber
                            |> ArraySized.map
                                (\_ -> singleElement |> Tree2.one |> fill)
                    }

            Multiple multiple ->
                multiple


one : element -> KeysSet element keys_ lastIndex_
one singleElement =
    One singleElement


fromMultiple : Multiple element keys lastIndex -> KeysSet element keys lastIndex
fromMultiple =
    \multipleLike ->
        case multipleLike |> Typed.untag |> .size of
            1 ->
                multipleLike
                    |> Typed.untag
                    |> .byKeys
                    |> ArraySized.inToOn
                    |> ArraySized.element ( Up, n0 )
                    |> filled
                    |> Tree2.trunk
                    |> One

            _ ->
                multipleLike |> Multiple


size : KeysSet element_ keys_ lastIndex_ -> Int
size =
    \keysSet ->
        case keysSet of
            One _ ->
                1

            Multiple multiple ->
                multiple |> Typed.untag |> .size
