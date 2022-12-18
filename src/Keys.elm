module Keys exposing
    ( Keys
    , keys, and, And
    )

{-| TODO rename module

@docs Keys


## create

@docs keys, and, And

-}

import Equal exposing (Equaling, Equality)
import Typed exposing (Checked, Public, Tagged, Typed)


{-| Promises that given aspects are unique across all elements.
See [`Uniqueness`](#Uniqueness)

    import Emptiable

    -- ↓ and tag should be in a separate module
    user : KeysSet.Keys User { username : User -> String, email : User -> String } { user : () }
    user =
        KeysSet.keys
            { tag = { user = () }
            , keys =
                -- no need for -Key in practice. Just for clarity
                { usernameKey = .username, emailKey = .email }
            , unique =
                -- make sure to not forget one
                [ unique .usernameKey, unique .emailKey ]
            }

    Emptiable.empty
        |> KeysSet.insert user
            { username = "ben", email = "ben10@gmx.de" }
        |> KeysSet.insert user
            { username = "mai", email = "ben10@gmx.de" }
        -- not inserted
        -- There's already an element where .email is "ben10@gmx.de"

Elements that are inserted must **not** contain **functions, json or regexes**.
Elm will crash trying to see if they are equal

-}
type alias Keys element tags keys =
    Typed
        Checked
        tags
        Public
        { record : keys
        , toList : keys -> List (( element, element ) -> Equality)
        }


type alias KeysInProgress element tags keysComplete keysHungry =
    Typed
        Checked
        tags
        Public
        { record : keysHungry
        , toList : keysComplete -> List (( element, element ) -> Equality)
        }


{-| Tag for [`and`](#and)
-}
type And aspectEqualTag otherAspectEqualTag
    = And



-- TODO builder


{-| Check values on whether a given aspect is the same structurally

    unique .name
        { name = "smile", symbol = '😊' }
        { symbol = '🙂', name = "smile" }
    --> { areUnique = False }

    unique .symbol
        { name = "smile", symbol = '😊' }
        { symbol = '🙂', name = "smile" }
    --> { areUnique = True }

    unique (\person -> ( person.firstName, person.lastName ))
        { lastName = "jimmy", firstName = "petter", status = "Absent" }
        { lastName = "jimmy", firstName = "greg", status = "Absent" }
    --> { areUnique = True }

in `KeysSet` TODO

    userKeys :
        Uniqueness.Keys
            User
            (And () (Key.By Record.Map.Email Equal.Structurally))
            { email : Key User (Key.By Record.Map.Email Equal.Structurally) Email Equality
            }
    userKeys =
        Uniqueness.keys (\email -> { email = email })
            |> Uniqueness.and .email (Equal.by Record.Map.email Equal.structurally)

    Emptiable.empty
        |> KeysSet.insert userKeys
            { name = "ben", email = "ben10@gmx.de" }
        |> KeysSet.insert userKeys
            { name = "mai", email = "ben10@gmx.de" }
        -- not inserted
        -- There's already an element where .email is "ben10@gmx.de"

-}
keys :
    keysHungry
    -> KeysInProgress element () keysComplete_ keysHungry
keys keysFeed =
    { record = keysFeed
    , toList = \_ -> []
    }
        |> Typed.tag ()


and :
    (keysComplete -> Equal.Key element byTag key)
    -> Equal.Key element byTag key
    ->
        (KeysInProgress
            element
            tags
            keysComplete
            (Equal.Key element byTag key -> keysFedThoMaybeStillHungry)
         ->
            KeysInProgress
                element
                (And tags byTag)
                keysComplete
                keysFedThoMaybeStillHungry
        )
and keysAccessKey key =
    \keysBuild ->
        Typed.mapTo And
            (\info ->
                { record = info.record key
                , toList =
                    \keysComplete ->
                        info.toList keysComplete
                            |> (::) (keysComplete |> keysAccessKey |> Equal.withKey)
                }
            )
            keysBuild
