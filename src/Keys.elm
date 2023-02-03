module Keys exposing
    ( Keys, KeysTag
    , for, KeysBeingBuilt
    , by
    , Key
    , toKeyWith, keyOrderWith, keyIndex
    , toArray
    )

{-| Multiple key-[`Ordering`](Order#Ordering) pairs

Configure what's considered a key inside a [`KeysSet`](KeysSet#KeysSet)

Create using [`Keys.for`](#for), as shown in

    - [`KeysSet` example](KeysSet#KeysSet)
    - [readme examples](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/)

You can just ignore the `Typed` thing but if you're curious → [`typed-value`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/)

@docs Keys, KeysTag


## create

@docs for, KeysBeingBuilt
@docs by


## single key

@docs Key
@docs toKeyWith, keyOrderWith, keyIndex


## transform

@docs toArray

-}

import ArraySized exposing (ArraySized)
import Map exposing (Mapping)
import N exposing (Add1, Exactly, N, On, To, Up, Up0, n0, n1)
import Order exposing (Ordering)
import Typed exposing (Checked, Internal, Public, Tagged, Typed)


{-| How to annotate a fully constructed [`Keys`](#Keys) builder
ready to use.
It preserves knowledge that each index is less than the whole count.

"infer types" is your friend here

    import Emptiable

    -- ↓ and tag should be in a separate module
    userKeys :
        Keys
        User
        (And ... Done)
        { name : Key User String (Up0 x)
        , email : Key User String (Up1 x)
        }
        (Up2 x)
    userKeys =
        Keys.for (\name email -> { name = name, email = email } )
            |> Keys.by .username
                ( Record.Map.username, String.Order... )
            |> Keys.by .email
                ( Record.Map.email, String.Order... )

    Emptiable.empty
        |> KeysSet.insert userKeys
            { username = "ben", email = "ben10@gmx.de" }
        |> KeysSet.insert userKeys
            { username = "mai", email = "ben10@gmx.de" }
        -- not inserted
        -- There's already an element where .email is "ben10@gmx.de"

-}
type alias Keys element tags keys lastIndex =
    KeysBeingBuilt element tags keys keys (On (Add1 lastIndex))


{-| Once you supply all the necessary key-[`Ordering`](Order#Ordering) pairs with [`and`](#and),
a [`KeysBeingBuilt`](#KeysBeingBuilt) automatically becomes a [`Keys`](#Keys)

It's opaque because you shouldn't be able to

  - retrieve it's keys to use in different [`Keys`](#Keys)
  - throw it's array representation out of sync

-}
type alias KeysBeingBuilt element tags keysConstructor completeKeys keyCount =
    Typed
        Checked
        KeysTag
        Internal
        (Typed
            Tagged
            tags
            Public
            { keys : keysConstructor
            , toArray :
                ArraySized
                    (completeKeys
                     -> (( element, element ) -> Order)
                    )
                    (Exactly keyCount)
            }
        )


{-| Tags correctly constructed [`Keys`](#Keys) (and each [`Key`](#Key))
-}
type KeysTag
    = Keys


{-| List all keys in the `keys` record as functions
determining the `Order` of 2 elements
-}
toArray :
    Keys element tags keys_ lastKeyIndex
    ->
        Typed
            Tagged
            tags
            Public
            (ArraySized
                (( element, element ) -> Order)
                (Exactly (On (Add1 lastKeyIndex)))
            )
toArray =
    \keysInfoTagged ->
        let
            keysInfo =
                keysInfoTagged |> Typed.internal Keys
        in
        keysInfo
            |> Typed.map
                (\info ->
                    info
                        |> .toArray
                        |> ArraySized.map (\toOrder -> toOrder info.keys)
                )


{-| Start a [keys builder](#KeysBeingBuilt) by putting all keys coming in as arguments
in a record

    userKeys :
        Keys
            User
            (And (Key.By Record.Map.Email Equal.Structurally) Done)
            { email : Key User (Up0 x) Email }
            (Up1 x)
    userKeys =
        Keys.for (\email -> { email = email })
            |> Keys.by .email ( Record.Map.email, Email.order )

in [`KeysSet`](KeysSet#KeysSet)

    Emptiable.empty
        |> KeysSet.insert userKeys
            { name = "ben", email = "ben10@gmx.de" }
        |> KeysSet.insert userKeys
            { name = "mai", email = "ben10@gmx.de" }
        -- not inserted
        -- There's already an element where .email is "ben10@gmx.de"

-}
for :
    keysConstructor
    -> KeysBeingBuilt element_ () keysConstructor completeKeys_ (Up0 x_)
for keysConstructor =
    { keys = keysConstructor
    , toArray = ArraySized.empty
    }
        |> Typed.tag ()
        |> Typed.tag Keys


{-| Add a key

  - what name does the key have? (same as in the initial record from [`Keys.for`](#for))
  - as a tuple
      - which aspect should be sorted by? → [`Map`](#Map)
      - how should we compare these aspects? [`Ordering`](#Ordering)

-}
by :
    (completeKeys
     -> Key element key (Up keyCountToCompleteFrom1 To lastIndex)
    )
    ->
        ( Mapping element elementToKeyTag key
        , Ordering key elementKeyOrderTag
        )
    ->
        (KeysBeingBuilt
            element
            tags
            (Key element key (Up keyCountToCompleteFrom1 To lastIndex)
             -> keysConstructedPartially
            )
            completeKeys
            (Up (Add1 keyCountToCompleteFrom1) To (Add1 lastIndex))
         ->
            KeysBeingBuilt
                element
                ( tags, Order.By elementToKeyTag elementKeyOrderTag )
                keysConstructedPartially
                completeKeys
                (Up keyCountToCompleteFrom1 To (Add1 lastIndex))
        )
by keysAccessKey ( toKey, keyOrder ) =
    let
        keysInfoPush :
            { keys :
                Key element key (Up keyCountToCompleteFrom1 To lastIndex)
                -> keysConstructedPartially
            , toArray :
                ArraySized
                    (completeKeys -> (( element, element ) -> Order))
                    (Exactly (Up (Add1 keyCountToCompleteFrom1) To (Add1 lastIndex)))
            }
            ->
                { keys : keysConstructedPartially
                , toArray :
                    ArraySized
                        (completeKeys -> (( element, element ) -> Order))
                        (Exactly (Up keyCountToCompleteFrom1 To (Add1 lastIndex)))
                }
        keysInfoPush keysInfo =
            let
                index : N (Exactly (Up keyCountToCompleteFrom1 To lastIndex))
                index =
                    keysInfo.toArray
                        |> ArraySized.length
                        |> N.minEndsSubtract n1
                        |> N.maxEndsSubtract n1

                keyField : Key element key (Up keyCountToCompleteFrom1 To lastIndex)
                keyField =
                    Typed.tag Keys
                        { index = index
                        , toKey = Map.with toKey
                        , keyOrder = keyOrder |> Typed.untag
                        }

                toArrayPushed :
                    ArraySized
                        (completeKeys
                         -> (( element, element ) -> Order)
                        )
                        (Exactly (Up keyCountToCompleteFrom1 To (Add1 lastIndex)))
                toArrayPushed =
                    keysInfo.toArray
                        |> ArraySized.push
                            (\completeKeys ->
                                completeKeys
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
            |> Typed.wrapAnd (Order.by toKey keyOrder)
            |> Typed.map
                (\( keysInfo, _ ) -> keysInfoPush keysInfo)
            |> Typed.tag Keys


orderWithKey :
    Key element index_ key_
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


keyInfo :
    ( Keys element tags_ keys lastIndex
    , keys -> Key element key (Up indexToLast To lastIndex)
    )
    ->
        { index : N (Exactly (Up indexToLast To lastIndex))
        , toKey : element -> key
        , keyOrder : ( key, key ) -> Order
        }
keyInfo ( keys, field ) =
    let
        info =
            keys
                |> Typed.internal Keys
                |> Typed.untag
    in
    info |> .keys |> field |> Typed.untag


{-| A [`Key`](#Key)'s distance from the first [`and`](#and) in the [`Keys` builder](#KeysBeingBuilt)
-}
keyIndex :
    ( Keys element tags_ keys lastIndex
    , keys -> Key element key_ (Up indexToLast To lastIndex)
    )
    -> N (Exactly (Up indexToLast To lastIndex))
keyIndex ( keys, key ) =
    ( keys, key ) |> keyInfo |> .index


{-| How to turn the element into the specified key
-}
toKeyWith :
    ( Keys element tags_ keys lastIndex
    , keys -> Key element key (Up indexToLast_ To lastIndex)
    )
    -> (element -> key)
toKeyWith ( keys, key ) =
    ( keys, key ) |> keyInfo |> .toKey


{-| How to order by the specified key
-}
keyOrderWith :
    ( Keys element tags_ keys lastIndex
    , keys -> Key element key (Up indexToLast_ To lastIndex)
    )
    -> (( key, key ) -> Order)
keyOrderWith ( keys, key ) =
    ( keys, key ) |> keyInfo |> .keyOrder



-- key


{-| By which aspect = key and in which key `Order` elements should be sorted
-}
type alias Key element key index =
    Typed
        Checked
        KeysTag
        Public
        { index : N (Exactly index)
        , toKey : element -> key
        , keyOrder : ( key, key ) -> Order
        }
