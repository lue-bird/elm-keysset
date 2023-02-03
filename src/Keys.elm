module Keys exposing
    ( Keys, KeysTag
    , identity, Identity
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

@docs identity, Identity
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
import N exposing (Add1, Exactly, N, N0, On, To, Up, Up0, n1)
import Order exposing (Ordering)
import Typed exposing (Checked, Internal, Public, Typed)


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
type alias Keys element keys lastIndex =
    KeysBeingBuilt element keys keys (On (Add1 lastIndex))


{-| Once you supply all the necessary key-[`Ordering`](Order#Ordering) pairs with [`by`](#by),
a [`KeysBeingBuilt`](#KeysBeingBuilt) automatically becomes a [`Keys`](#Keys)

It's opaque because you shouldn't be able to

  - retrieve it's keys to use in different [`Keys`](#Keys)
  - throw it's array representation out of sync

-}
type alias KeysBeingBuilt element keysComplete keysConstructor keyCount =
    Typed
        Checked
        KeysTag
        Internal
        { keys : keysConstructor
        , toArray :
            ArraySized
                (keysComplete
                 -> (( element, element ) -> Order)
                )
                (Exactly keyCount)
        }


{-| Tags correctly constructed [`Keys`](#Keys) (and each [`Key`](#Key))
-}
type KeysTag
    = Keys


{-| List all keys in the `keys` record as functions
determining the `Order` of 2 elements
-}
toArray :
    Keys element keys_ lastKeyIndex
    ->
        ArraySized
            (( element, element ) -> Order)
            (Exactly (On (Add1 lastKeyIndex)))
toArray =
    \keysInfoTagged ->
        let
            keysInfo =
                keysInfoTagged |> Typed.internal Keys
        in
        keysInfo
            |> .toArray
            |> ArraySized.map (\toOrder -> toOrder keysInfo.keys)


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
    -> KeysBeingBuilt element_ completeKeys_ keysConstructor (Up0 x_)
for keysConstructor =
    { keys = keysConstructor
    , toArray = ArraySized.empty
    }
        |> Typed.tag Keys


{-| Ordering by the element itself.

Short for

    Keys.identity order =
        for identity |> by ( identity, Map.identity ) order

in [`KeysSet`](KeysSet#KeysSet)

    import Map
    import Order
    import Int.Order
    import N exposing (Up, To, N0)
    import KeysSet

    intIncreasing : Keys.Identity Int Int.Order.Increasing
    intIncreasing =
        Keys.identity Int.Order.increasing

    KeysSet.fromList intIncreasing [ -1, 5, 5, 8, 7 ]
        |> KeysSet.toList ( intIncreasing, Basics.identity )
    --> [ -1, 5, 7, 8 ]

-}
identity : Ordering element elementOrderTag -> Identity element elementOrderTag
identity order =
    for Basics.identity |> by ( Basics.identity, Map.identity ) order


{-| Resulting type of [`Keys.identity`](#identity).

    import Map
    import Order
    import Float.Order
    import N exposing (Up, To, N0)
    import KeysSet

    floatIncreasing : Keys.Identity Float Float.Order.Increasing
    floatIncreasing =
        Keys.identity Float.Order.increasing

    KeysSet.fromList floatIncreasing [ -1.1, 5, 5, 8.7, 7.8 ]
        |> KeysSet.toList ( floatIncreasing, Basics.identity )
    --> [ -1.1, 5, 7.8, 8.7 ]

-}
type alias Identity element elementOrderTag =
    Keys element (Key element (Order.By Map.Identity elementOrderTag) element (Up N0 To N0)) N0


{-| Add a key

  - what name does the key have? (same as in the initial record from [`Keys.for`](#for))
  - which aspect should be sorted by? → [`Mapping`](Map#Mapping)
  - how should we compare these aspects? [`Ordering`](Order#Ordering)

-}
by :
    ( keysComplete
      -> Key element (Order.By toKeyTag keyOrderTag) key (Up indexToLast To lastIndex)
    , Mapping element toKeyTag key
    )
    -> Ordering key keyOrderTag
    ->
        (KeysBeingBuilt
            element
            keysComplete
            (Key element (Order.By toKeyTag keyOrderTag) key (Up indexToLast To lastIndex)
             -> keysConstructedPartially
            )
            (Up (Add1 indexToLast) To (Add1 lastIndex))
         ->
            KeysBeingBuilt
                element
                keysComplete
                keysConstructedPartially
                (Up indexToLast To (Add1 lastIndex))
        )
by ( keysAccessKey, toKey ) keyOrder =
    let
        keysInfoPush :
            { keys :
                Key
                    element
                    (Order.By toKeyTag keyOrderTag)
                    key
                    (Up indexToLast To lastIndex)
                -> keysConstructedPartially
            , toArray :
                ArraySized
                    (keysComplete -> (( element, element ) -> Order))
                    (Exactly (Up (Add1 indexToLast) To (Add1 lastIndex)))
            }
            ->
                { keys : keysConstructedPartially
                , toArray :
                    ArraySized
                        (keysComplete -> (( element, element ) -> Order))
                        (Exactly (Up indexToLast To (Add1 lastIndex)))
                }
        keysInfoPush keysInfo =
            let
                index : N (Exactly (Up indexToLast To lastIndex))
                index =
                    keysInfo.toArray
                        |> ArraySized.length
                        |> N.minEndsSubtract n1
                        |> N.maxEndsSubtract n1

                keyField :
                    Key
                        element
                        (Order.By toKeyTag keyOrderTag)
                        key
                        (Up indexToLast To lastIndex)
                keyField =
                    Typed.mapToWrap Keys
                        (\_ ->
                            { index = index
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
                        (Exactly (Up indexToLast To (Add1 lastIndex)))
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


keyInfo :
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast To lastIndex)
    )
    ->
        { index : N (Exactly (Up indexToLast To lastIndex))
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
keyIndex :
    ( Keys element keys lastIndex
    , keys -> Key element key_ by_ (Up indexToLast To lastIndex)
    )
    -> N (Exactly (Up indexToLast To lastIndex))
keyIndex ( keys, key ) =
    ( keys, key ) |> keyInfo |> .index


{-| How to turn the element into the specified key
-}
toKeyWith :
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> (element -> key)
toKeyWith ( keys, key ) =
    ( keys, key ) |> keyInfo |> .toKey


{-| How to order by the specified key
-}
keyOrderWith :
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> (( key, key ) -> Order)
keyOrderWith ( keys, key ) =
    ( keys, key ) |> keyInfo |> .keyOrder



-- key


{-| By which aspect = key and in which key `Order` elements should be sorted
-}
type alias Key element orderByTag key index =
    Typed
        Checked
        ( KeysTag, orderByTag )
        Public
        { index : N (Exactly index)
        , toKey : element -> key
        , keyOrder : ( key, key ) -> Order
        }
