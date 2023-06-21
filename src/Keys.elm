module Keys exposing
    ( oneBy
    , identity
    , for, KeysBeingBuilt
    , by
    , Keys, KeysWithFocus
    , key
    , Key
    , toArray
    , toKeyWith, keyOrderWith
    , IdentityKeys
    )

{-| Multiple key [`Ordering`](Order#Ordering)s

Configure what's considered a key inside a [`KeysSet`](KeysSet#KeysSet)

Create starting with [`Keys.for`](#for) and building with [`|> Keys.by`](#by) as shown in
the [readme examples](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/)

You can just ignore the `Typed` thing but if you're curious → [`typed-value`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/)


## create


### one key

@docs oneBy
@docs identity, Identity


### multiple keys

@docs for, KeysBeingBuilt
@docs by
@docs Keys, KeysWithFocus


## alter

@docs key


# safe internals

You won't need them if you just want to use [`KeysSet`](KeysSet#KeysSet).
... It's safe to expose this information, tho, so why not make it available :)

@docs Key


## transform

@docs toArray


## single key

@docs toKeyWith, keyOrderWith

-}

import ArraySized exposing (ArraySized)
import Keys.Internal
import Map exposing (Mapping)
import N exposing (Add1, Exactly, N1, On, To, Up, Up0)
import Order exposing (Ordering)


{-| The type of fully constructed [`KeysBeingBuilt`](#KeysBeingBuilt)
ready to use.

"infer types" is your friend here

    import Keys exposing (Keys, Key)
    import Order
    import Char.Order
    import String.Order
    import Emptiable
    import N exposing (N2)

    userKeys :
        -- just infer this
        Keys
            User
            { name :
                Key
                    User
                    (Order.By
                        User.Name
                        (String.Order.Earlier (Char.Order.AToZ Order.Tie))
                    )
                    String
                    N2
            , email : Key User (Order.By User.Email Email.DefaultOrder) String N2
            }
            N2
    userKeys =
        Keys.for (\name email -> { name = name, email = email } )
            |> Keys.by ( .name, User.name )
                (String.Order.earlier (Char.Order.aToZ Order.tie))
            |> Keys.by ( .email, User.email )
                Email.defaultOrder

    Emptiable.empty
        |> KeysSet.insertIfNoCollision userKeys
            { username = "ben", email = ..ben10@gmx.de.. }
        |> KeysSet.insertIfNoCollision userKeys
            { username = "mai", email = ..ben10@gmx.de.. }
            -- not inserted
            -- There's already an element where .email is ..ben10@gmx.de..

What's with those `Up N.. To N..`?
_Internally_, each key will be assigned an index.
that type from [bounded-nat](https://dark.elm.dmy.fr/packages/lue-bird/elm-bounded-nat/latest/)
preserves the knowledge that each key's index is less than the whole count.

-}
type alias Keys element keys keyCount =
    KeysWithFocus element keys keys keyCount


type alias KeysWithFocus element keys focus keyCount =
    KeysBeingBuiltWithFocus element keys keys focus (On keyCount)


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
    KeysBeingBuiltWithFocus element keysComplete keysConstructor keysConstructor keyCount


{-| The basic building block for keys, possibly incomplete, possibly focused on a specific key. See

  - [`KeysBeingBuilt`](#KeysBeingBuilt)
  - [`Keys`](#Keys)
  - [`KeysWithFocus`](#KeysWithFocus)

-}
type alias KeysBeingBuiltWithFocus element keysComplete keysConstructor focus keyCount =
    Keys.Internal.KeysBeingBuiltWithFocus element keysComplete keysConstructor focus keyCount


{-| Tags correctly constructed [`Keys`](#Keys) (and each [`Key`](#Key)).
You will never use this.
-}
type alias KeysTag =
    Keys.Internal.KeysTag


{-| By which aspect = key and in which key `Order` elements should be sorted
-}
type alias Key element orderByTag key keyCount =
    Keys.Internal.Key element orderByTag key keyCount


{-| List all keys in the `keys` record as an [array](https://dark.elm.dmy.fr/packages/lue-bird/elm-typesafe-array/latest/)
of functions
determining the `Order` of 2 elements
-}
toArray :
    KeysWithFocus element keys_ focus_ keyCount
    -> ArraySized (( element, element ) -> Order) (Exactly (On keyCount))
toArray =
    Keys.Internal.toArray


{-| Start a [keys builder](#KeysBeingBuilt) by giving names to the individual keys as arguments
using a record

    userKeys :
        Keys
            User
            { name : Key User (Order.By User.Name Username.Order) Username N2
            , email : Key User (Order.By User.Email Email.Order) Email N2
            }
            N2
    userKeys =
        Keys.for (\email name -> { email = email, name = name })
            |> Keys.by ( .email, User.email, Email.order )
            |> Keys.by ( .name, User.name, Username.order )

    Emptiable.empty
        |> KeysSet.insertIfNoCollision userKeys
            { name = Username "ben", email = "ben10@gmx.de" }
        |> KeysSet.insertIfNoCollision userKeys
            { name = Username "mai", email = "ben10@gmx.de" }
            -- not inserted
            -- There's already an element where .email is "ben10@gmx.de"
        |> KeysSet.insertIfNoCollision userKeys
            { name = Username "ben", email = "ben11@gmx.de" }
            -- not inserted
            -- There's already an element where .name is "ben"

-}
for :
    keysConstructor
    -> KeysBeingBuilt element_ completeKeys_ keysConstructor (Up0 keyCount_)
for keysConstructor =
    Keys.Internal.for keysConstructor


{-| Create [`Keys`](#Keys) for single key
in case you don't plan on adding more keys.

See also [`Keys.identity`](#identity)
if you want to use the complete element as the key.

FYI, this is equivalent to

    Keys.for identity
        |> Keys.by ( identity, mapping ) ordering

-}
oneBy :
    Mapping element toKeyTag key
    -> Ordering key keyOrderTag
    -> Keys element (Key element (Order.By toKeyTag keyOrderTag) key N1) N1
oneBy mapping ordering =
    for Basics.identity
        |> by ( Basics.identity, mapping ) ordering


{-| Ordering by the element itself.

Short for

    Keys.identity order =
        Keys.for identity |> Keys.by ( identity, Map.identity ) order

in [`KeysSet`](KeysSet#KeysSet)

    import Map
    import Order
    import Int.Order
    import Keys
    import KeysSet

    intUp : IdentityKeys Int Int.Order.Up
    intUp =
        Keys.identity Int.Order.up

    KeysSet.fromList intUp [ -1, 5, 5, 8, 7 ]
        --: Emptiable (IdentitySet Int Int.Order.Up) Possibly
        |> KeysSet.toList intUp
    --> [ -1, 5, 7, 8 ]

-}
identity : Ordering element elementOrderTag -> IdentityKeys element elementOrderTag
identity order =
    oneBy Map.identity order


{-| Resulting type of [`Keys.identity`](#identity).

    import Map
    import Order
    import Float.Order
    import KeysSet exposing (IdentitySet)
    import Keys

    floatUp : IdentityKeys Float Float.Order.Up
    floatUp =
        Keys.identity Float.Order.up

    KeysSet.fromList floatUp [ -1.1, 5, 5, 8.7, 7.8 ]
        --: Emptiable (IdentitySet Float Float.Order.Up) Possibly
        |> KeysSet.toList floatUp
    --> [ -1.1, 5, 7.8, 8.7 ]

-}
type alias IdentityKeys element elementOrderTag =
    Keys
        element
        (Key
            element
            (Order.By Map.Identity elementOrderTag)
            element
            N1
        )
        N1


{-| Add a key

  - what name does the key have? (same as in the initial record from [`Keys.for`](#for))
  - which aspect should be sorted by? → [`Mapping`](Map#Mapping)
  - how should we compare these aspects? [`Ordering`](Order#Ordering)

-}
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
    \keysSoFar ->
        keysSoFar |> Keys.Internal.by ( keysAccessKey, toKey ) keyOrder


key :
    (keys -> focusNew)
    ->
        (KeysWithFocus element keys focusOld_ keyCount
         -> KeysWithFocus element keys focusNew keyCount
        )
key accessKey =
    \keys ->
        keys |> Keys.Internal.key accessKey


{-| How to turn the element into the specified key
-}
toKeyWith :
    KeysWithFocus element keys_ (Key element orderByTag key keyCount) keyCount
    -> (element -> key)
toKeyWith key_ =
    key_ |> Keys.Internal.keyInfo |> .toKey


{-| How to order by the specified key
-}
keyOrderWith :
    KeysWithFocus element keys_ (Key element orderByTag key keyCount) keyCount
    -> (( key, key ) -> Order)
keyOrderWith key_ =
    key_ |> Keys.Internal.keyInfo |> .keyOrder
