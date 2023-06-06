module Keys.Internal exposing (Key, KeysBeingBuilt, KeysTag, by, for, keyElement, keyInfo, toArray)

import ArraySized exposing (ArraySized)
import Linear exposing (Direction(..))
import Map exposing (Mapping)
import N exposing (Add1, Exactly, In, N, N1, On, To, Up, Up0, n1)
import Order exposing (Ordering)
import Tree2
import Typed exposing (Checked, Internal, Public, Typed)


type KeysTag
    = Keys


{-| By which aspect = key and in which key `Order` elements should be sorted
-}
type alias Key element orderByTag key keyCount =
    Typed
        Checked
        ( KeysTag, orderByTag )
        Public
        { element :
            ArraySized (Tree2.Branch element) (Exactly (On keyCount))
            -> Tree2.Branch element
        , toKey : element -> key
        , keyOrder : ( key, key ) -> Order
        }


keyInfo :
    ( Keys element keys keyCount
    , keys -> Key element by_ key keyCount
    )
    ->
        { element :
            ArraySized (Tree2.Branch element) (Exactly (On keyCount))
            -> Tree2.Branch element
        , toKey : element -> key
        , keyOrder : ( key, key ) -> Order
        }
keyInfo ( keys, field ) =
    let
        info =
            keys |> Typed.internal Keys
    in
    info |> .keys |> field |> Typed.untag


{-| A [`Key`](#Key)'s distance from the first [`by`](#by) in the [`Keys` builder](#KeysBeingBuilt)
-}
keyElement :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    ->
        (ArraySized (Tree2.Branch element) (Exactly (On keyCount))
         -> Tree2.Branch element
        )
keyElement ( keys, key ) =
    ( keys, key ) |> keyInfo |> .element


type alias KeysBeingBuilt element keysComplete keysConstructor keyCount =
    Typed
        Checked
        KeysTag
        Internal
        { keys : keysConstructor
        , toArray :
            ArraySized
                (keysComplete -> (( element, element ) -> Order))
                (Exactly keyCount)
        }


type alias Keys element keys keyCount =
    KeysBeingBuilt element keys keys (On keyCount)


toArray :
    Keys element keys_ keyCount
    -> ArraySized (( element, element ) -> Order) (Exactly (On keyCount))
toArray =
    \keysInfoTagged ->
        let
            keysInfo =
                keysInfoTagged |> Typed.internal Keys
        in
        keysInfo
            |> .toArray
            |> ArraySized.map (\toOrder -> toOrder keysInfo.keys)


for :
    keysConstructor
    -> KeysBeingBuilt element_ completeKeys_ keysConstructor (Up0 keyCount_)
for keysConstructor =
    { keys = keysConstructor
    , toArray = ArraySized.empty
    }
        |> Typed.tag Keys


by :
    ( keysComplete
      -> Key element (Order.By toKeyTag keyOrderTag) key (Add1 keyCountFrom1)
    , Mapping element toKeyTag key
    )
    -> Ordering key keyOrderTag
    ->
        (KeysBeingBuilt
            element
            keysComplete
            (Key element (Order.By toKeyTag keyOrderTag) key (Add1 keyCountFrom1)
             -> keysConstructedPartially
            )
            (Up (Add1 toKeyCountFrom1) To (Add1 keyCountFrom1))
         ->
            KeysBeingBuilt
                element
                keysComplete
                keysConstructedPartially
                (Up toKeyCountFrom1 To (Add1 keyCountFrom1))
        )
by ( keysAccessKey, toKey ) keyOrder =
    let
        keysInfoPush :
            { keys :
                Key
                    element
                    (Order.By toKeyTag keyOrderTag)
                    key
                    (Add1 keyCountFrom1)
                -> keysConstructedPartially
            , toArray :
                ArraySized
                    (keysComplete -> (( element, element ) -> Order))
                    (Exactly (Up (Add1 toKeyCountFrom1) To (Add1 keyCountFrom1)))
            }
            ->
                { keys : keysConstructedPartially
                , toArray :
                    ArraySized
                        (keysComplete -> (( element, element ) -> Order))
                        (Exactly (Up toKeyCountFrom1 To (Add1 keyCountFrom1)))
                }
        keysInfoPush keysInfo =
            let
                index : N (In (On N1) (Up toKeyCountFrom1 To (Add1 keyCountFrom1)))
                index =
                    keysInfo.toArray
                        |> ArraySized.length
                        |> N.minTo0
                        |> N.add n1
                        |> N.minEndsSubtract n1
                        |> N.maxEndsSubtract n1

                keyField :
                    Key
                        element
                        (Order.By toKeyTag keyOrderTag)
                        key
                        (Add1 keyCountFrom1)
                keyField =
                    Typed.mapToWrap Keys
                        (\_ ->
                            { element = ArraySized.element ( Up, index )
                            , toKey = Map.with toKey
                            , keyOrder = keyOrder |> Typed.untag
                            }
                        )
                        (Order.by toKey keyOrder)

                toArrayPushed :
                    ArraySized
                        (keysComplete
                         -> (( element, element ) -> Order)
                        )
                        (Exactly (Up toKeyCountFrom1 To (Add1 keyCountFrom1)))
                toArrayPushed =
                    keysInfo.toArray
                        |> ArraySized.push
                            (\keysComplete ->
                                keysComplete
                                    |> keysAccessKey
                                    |> orderWithKey
                            )
                        |> ArraySized.maxEndsSubtract n1
                        |> ArraySized.minEndsSubtract n1
            in
            { keys = keysInfo.keys keyField
            , toArray = toArrayPushed
            }
    in
    \keysInfoTagged ->
        keysInfoTagged
            |> Typed.internal Keys
            |> keysInfoPush
            |> Typed.tag Keys


orderWithKey :
    Key element key_ by_ index_
    -> (( element, element ) -> Order)
orderWithKey =
    \key_ ->
        let
            keyInfo_ =
                key_ |> Typed.untag
        in
        \( a, b ) ->
            keyInfo_.keyOrder
                ( a |> keyInfo_.toKey, b |> keyInfo_.toKey )
