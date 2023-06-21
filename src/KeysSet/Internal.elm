module KeysSet.Internal exposing (KeysSet(..), KeysSetTag, Multiple, fromMultiple, one, size, tagFor, tagForIdentity, toMultiple, treeForAnyElementTry, treeForElement)

{-| Helps keeping the scope of unsafe operations like creating a tag narrow.
This setup also improves testability (accessing internal trees for validation for example)
-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable, fill)
import Keys exposing (IdentityKeys, Key, Keys, KeysWithFocus)
import Linear
import N exposing (Exactly, On, n1)
import Order
import Possibly exposing (Possibly(..))
import Tree2
import Typed exposing (Checked, Public, Typed)


type KeysSet element keys keyCount
    = One element
    | -- no guarantee that there will be >= 2 elements. Just >= 1 is guaranteed
      Multiple (Multiple element keys keyCount)


{-| Underlying representation of a [`KeysSet`](#KeysSet) with some elements
-}
type alias Multiple element keys keyCount =
    Typed
        Checked
        (KeysSetTag keys)
        Public
        { size : Int
        , byKeys :
            ArraySized (Tree2.Branch element) (Exactly keyCount)
        }


{-| Tag that verifies a [`KeysSet`](#KeysSet) is created by this module.
You don't need to know more but I you're interested,
check [`Typed.Checked`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/Typed#Checked)
-}
type KeysSetTag keys
    = KeysSet


treeForElement :
    (ArraySized (Tree2.Branch element) (Exactly (On keyCount))
     -> Tree2.Branch element
    )
    ->
        (KeysSet element keys_ keyCount
         -> Emptiable (Tree2.Branch element) never_
        )
treeForElement element =
    \keysSet ->
        case keysSet of
            One singleElement ->
                singleElement |> Tree2.one

            Multiple multiple ->
                multiple
                    |> Typed.untag
                    |> .byKeys
                    |> ArraySized.inToOn
                    |> element
                    |> Emptiable.filled


treeForAnyElementTry :
    KeysSet element keys_ keyCount_
    -> Emptiable (Tree2.Branch element) Possibly
treeForAnyElementTry =
    \keysSetFill ->
        case keysSetFill of
            One singleElement ->
                singleElement |> Tree2.one

            Multiple multiple ->
                multiple
                    |> Typed.untag
                    |> .byKeys
                    |> ArraySized.inToOn
                    |> ArraySized.elementTry ( Linear.Up, n1 )
                    |> Emptiable.map Emptiable.filled
                    |> Emptiable.fillElseOnEmpty (\Possible -> Emptiable.empty)


tagFor : KeysWithFocus element_ keys focus_ keyCount_ -> KeysSetTag keys
tagFor _ =
    KeysSet


tagForIdentity :
    KeysWithFocus element keys (Key element (Order.By toKeyTag_ orderTag) key index_) keyCount_
    -> KeysSetTag (IdentityKeys key orderTag)
tagForIdentity _ =
    KeysSet


toMultiple :
    KeysWithFocus element keys focus_ keyCount
    ->
        (KeysSet element keys keyCount
         -> Multiple element keys keyCount
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


one : element -> KeysSet element keys_ keyCount_
one singleElement =
    One singleElement


fromMultiple : Multiple element keys keyCount -> KeysSet element keys keyCount
fromMultiple =
    \multipleLike ->
        -- case multipleLike |> Typed.untag |> .size of
        --     1 ->
        --         multipleLike
        --             |> Typed.untag
        --             |> .byKeys
        --             |> ArraySized.inToOn
        --             |> ArraySized.element ( Up, n0 )
        --             |> filled
        --             |> Tree2.trunk
        --             |> One
        --
        --     _ ->
        multipleLike |> Multiple


size : KeysSet element_ keys_ lastIndex_ -> Int
size =
    \keysSet ->
        case keysSet of
            One _ ->
                1

            Multiple multiple ->
                multiple |> Typed.untag |> .size
