module Keys exposing
    ( Keys, KeysTag, Key
    , identity, Identity
    , for, KeysBeingBuilt
    , by
    , toArray
    , toKeyWith, keyOrderWith, keyIndex
    )

{-| Multiple key [`Ordering`](Order#Ordering)s

Configure what's considered a key inside a [`KeysSet`](KeysSet#KeysSet)

Create starting with [`Keys.for`](#for) and building with [`|> Keys.by`](#by) as shown in
the [readme examples](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/)

You can just ignore the `Typed` thing but if you're curious → [`typed-value`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/)

@docs Keys, KeysTag, Key


## create

@docs identity, Identity
@docs for, KeysBeingBuilt
@docs by


# safe internals

You won't need them if you just want to use [`KeysSet`](KeysSet#KeysSet).
... It's safe to expose this information, tho, so why not make it available :)


## transform

@docs toArray


## single key

@docs toKeyWith, keyOrderWith, keyIndex

-}

import ArraySized exposing (ArraySized)
import Map exposing (Mapping)
import N exposing (Add1, Exactly, N, N0, N1, On, To, Up, Up0, n1)
import Order exposing (Ordering)
import Typed exposing (Checked, Internal, Public, Typed)


{-| The type of fully constructed [`KeysBeingBuilt`](#KeysBeingBuilt)
ready to use.

"infer types" is your friend here

    import Keys exposing (Keys, Key)
    import Order
    import Char.Order
    import String.Order
    import Emptiable
    import N exposing (Up, To, N0, N1, N2)

    userKeys :
        -- just infer this
        Keys
            User
            { name :
                Key
                    User
                    (Order.By
                        User.Name
                        (String.Order.Earlier (Char.Order.Alphabetically Order.Tie))
                    )
                    String
                    (Up N1 To N1)
            , email : Key User (Order.By User.Email Email.DefaultOrder) String (Up N0 To N1)
            }
            N2
    userKeys =
        Keys.for (\name email -> { name = name, email = email } )
            |> Keys.by ( .name, User.name )
                (String.Order.earlier (Char.Order.alphabetically Order.tie))
            |> Keys.by ( .email, User.email )
                Email.defaultOrder

    Emptiable.empty
        |> KeysSet.insert userKeys
            { username = "ben", email = ..ben10@gmx.de.. }
        |> KeysSet.insert userKeys
            { username = "mai", email = ..ben10@gmx.de.. }
        -- not inserted
        -- There's already an element where .email is ..ben10@gmx.de..

What's with those `Up N.. To N..`?
_Internally_, each key will be assigned an index.
that type from [bounded-nat](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)
preserves the knowledge that each key's index is less than the whole count.

-}
type alias Keys element keys keyCount =
    KeysBeingBuilt element keys keys (On keyCount)


{-| Once you supply all the necessary key-[`Ordering`](Order#Ordering)s with [`by`](#by),
a [`KeysBeingBuilt`](#KeysBeingBuilt) is automatically of type [`Keys`](#Keys).

So if you for example infer the type

    KeysBeingBuilt element { yourKeys } { yourKeys } keyCount

you can replace it by

    Keys element { yourKeys } keyCount

It's data is unaccessible because you shouldn't be able to

  - retrieve it's keys to use in different [`Keys`](#Keys)
  - have an out of sync [array](#toArray) representation

-}
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


{-| Tags correctly constructed [`Keys`](#Keys) (and each [`Key`](#Key))
-}
type KeysTag
    = Keys


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


{-| List all keys in the `keys` record as an [array](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)
of functions
determining the `Order` of 2 elements
-}
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


{-| Start a [keys builder](#KeysBeingBuilt) by giving names to the individual keys as arguments
using a record

    userKeys :
        Keys
            User
            { email : Key User (Order.By User.Email Email.Order) Email (Up N0 To N0) }
            N1
    userKeys =
        Keys.for (\email -> { email = email })
            |> Keys.by ( .email, User.email, Email.order )

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
    Keys element (Key element (Order.By Map.Identity elementOrderTag) element (Up N0 To N0)) N1


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
    ( Keys element keys (Add1 lastIndex)
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
    ( Keys element keys (Add1 lastIndex)
    , keys -> Key element key_ by_ (Up indexToLast To lastIndex)
    )
    -> N (Exactly (Up indexToLast To lastIndex))
keyIndex ( keys, key ) =
    ( keys, key ) |> keyInfo |> .index


{-| How to turn the element into the specified key
-}
toKeyWith :
    ( Keys element keys (Add1 lastIndex)
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> (element -> key)
toKeyWith ( keys, key ) =
    ( keys, key ) |> keyInfo |> .toKey


{-| How to order by the specified key
-}
keyOrderWith :
    ( Keys element keys (Add1 lastIndex)
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> (( key, key ) -> Order)
keyOrderWith ( keys, key ) =
    ( keys, key ) |> keyInfo |> .keyOrder
