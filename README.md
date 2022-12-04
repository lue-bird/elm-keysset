> lookups with arbitrary keys

  - 🗃️ [`KeySet`](#KeySet) for one key, `log n` runtime
  - 🦄 [`KeysSet`](#KeysSet) for multiple keys, `n` runtime
  - 🔭 [future ideas](#future-ideas)


# `KeySet`

Holds no functions.
Still, each [`Sorting`](KeySet#Sorting)
required to access/operate is enforced to be the same
(by attaching an opaque tag).
See ↓ example

→ Solves problems listed in [prior art](#prior-art)
alongside other [goodies](#goodies)

```elm
import Emptiable exposing (Emptiable)
import Stack
import KeySet exposing (KeySet)
import User exposing (User(..))

users : Emptiable (KeySet User User.ByEmailHostFirst) neverEmpty_
users =
    KeySet.fromStack User.byEmailHostFirst
        (Stack.topBelow
            (User { name = "Fred", email = ..@out.tech.. })
            [ User { name = "Ann", email = ..ann@mail.xyz.. }
            , User { name = "Annother", email = ..ann@mail.xyz.. }
            , User { name = "Bright", email = ..@snail.studio.. }
            ]
        )

users |> KeySet.size
--→ 3

users |> KeySet.element User.byEmailHostFirst ..ann@mail.xyz..
--→ Emptiable.filled { name = "Ann", email = ..ann@mail.xyz.. }

users |> KeySet.end Down -- minimum
--→ { name = "Ann", email = ..ann@mail.xyz.. } no Maybe
```
```elm
module User exposing (User(..), ByEmailHostFirst, byEmailHostFirst)

import KeySet
import Email

type User
    = User
        { email : Email
        , name : String
        }

type ByEmailHostFirst
    -- ! no exposing (..) → only constructable in this module
    = ByEmailHostFirst

emailHostFirst : KeySet.Sorting User Email ByEmailHostFirst
emailHostFirst =
    KeySet.sortingKey (\(User user) -> user.email)
        { tag = ByEmailHostFirst
        , order =
            Order.onTieNext
                [ Order.by Email.host
                    (String.Order.greaterEarlier (Char.Order.alphabetically Order.tie))
                , Order.by Email.label
                    (String.Order.greaterEarlier (Char.Order.alphabetically Order.tie))
                ]
        }
```
No typeclasses :)

Feel free to adapt this structure how you like it best,
for example separating [`Sorting`](KeySet#Sorting)s from data to each their own `module Data.By`

## goodies

  - ⚖ sorting by [`Ordering key = key -> key -> Order`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Order)
      - 👍 no reliance on `comparable`
      - 👍 no inconvenient `key -> String`
      - 🧩 [`linear-direction` `Order`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Order)
  - 🔑 `element -> key` function as part of a given [`Sorting`](KeySet#Sorting)
      - 👍 simpler type
      - 👍 simpler internals :)
  - 🗃 emptiability is part of the type
      - just use the same API with emptiable or non-empty conveniently
      - 👍 extra safety possible. Got enough elements? → `KeySet.end Up|Down`, `foldOnto`, `fold` don't need `Maybe`
      - 🧩 [`allowable-state`](https://dark.elm.dmy.fr/packages/lue-bird/elm-allowable-state/latest/)
      - 🧩 [`emptiness-typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/)
  - ↔ supply the direction as an argument
      - 🧩 [`linear-direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

## prior art

  - `Dict comparableKey value`
      - [`elm/core` `Dict`](https://dark.elm.dmy.fr/packages/elm/core/latest/Dict)
      - 👎 requires a new `Dict` wrapper for each custom `type` key.
        Often more a hindrance than helpful
  - custom functions (to `comparable` or `k -> k -> Order`)
      - 👎 no guarantee that the given functions are the same
        when trying to combine (`union`, `intersection`, ...)
      - `key -> key -> Order`
          - examples
              - [`owanturist/elm-avl-dict`](https://dark.elm.dmy.fr/packages/owanturist/elm-avl-dict/latest/)
          - 👍 simple to create
              - see for example [`Order` API](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Order)
          - 👍 simple type
          - 👍 not limited to `comparable` keys. Therefore simpler while not relying on magic
      - `key -> comparable`
          - examples
              - [`turboMaCk/any-dict`](https://dark.elm.dmy.fr/packages/turboMaCk/any-dict/latest/)
              - [`escherlies/elm-ix-dict`](https://package.elm-lang.org/packages/escherlies/elm-ix-dict/latest)
          - `key -> String`
              - examples (in no specific order)
                  - [`matzko/elm-opaque-dict`](https://dark.elm.dmy.fr/packages/matzko/elm-opaque-dict/latest/)
                  - [`timo-weike/generic-collections`](https://dark.elm.dmy.fr/packages/timo-weike/generic-collections/latest/)
                  - [`edkv/elm-generic-dict`](https://dark.elm.dmy.fr/packages/edkv/elm-generic-dict/latest/)
              - 👍 avoid having an extra type variable
              - 👎 requires more work
              - 👎 more prone to bugs in `toString` implementation not returning a unique `String` for all keys
              - 👎 slightly less performant when `toString` needs to do heavy work like sorting
      - create the complete API from a given function
          - examples
              - [`edkelly303/elm-any-type-collections`](https://dark.elm.dmy.fr/packages/edkelly303/elm-any-type-collections/latest/Any-Dict) with a `toComparable` function
          - using the constructed API is rather simple
          - 👎 dead code elimination and semantic versioning don't work
          - 👎 obscure API and interface type
          - 👍 functions aren't stored in the data structure
          - using for example `insert` from the wrong API "instance" with a different function is still possible but unlikely to happen in practice
      - just the function `key -> Maybe value` instead of a data structure
          - examples
              - [`jjant/elm-dict`](https://dark.elm.dmy.fr/packages/jjant/elm-dict/latest/AllDict)
          - 👎 `>= n` runtime
          - 👎 doesn't simplify it's structure. Every remove, insert, union, difference, _adds_ to the function logic
          - 👍 pretty easy to understand and build on with powerful features like assigning a specific value x whenever a condition is met
      - stored in the data structure
          - 👍 minimal clutter while still being explicit
          - 👎 needs to be stored in the type → `==` among other things will fail
          - 👎 slightly more cluttered API including `clear` to only remove all elements but keep the function
      - given on each access/operation
          - 👎 a tiny bit verbose
          - 👎 no guarantee that the given functions are the same
               (now doesn't only apply to when trying to combine)
      - given on every insertion/removal operation
          - 👎 no guarantee that the given functions are the same
  - association-list
      - examples
          - [`pzp1997/assoc-list`](https://dark.elm.dmy.fr/packages/pzp1997/assoc-list/latest/)
      - 👎 `n` runtime
      - 👍 no setup
      - 👍 simple type
  - tagging keys and the structure
      - examples
          - [`joneshf/elm-tagged` `Tagged.Set`, `Tagged.Dict`](https://dark.elm.dmy.fr/packages/joneshf/elm-tagged/latest/Tagged-Dict)
      - idea is quite similar to `KeySet` but
      - 👎 relies on `comparable`
      - 👎 everyone can tag without the tag name so only security by a bit more obscurity

# `KeysSet`
> look up elements by their unique aspects

For a `KeysSet` with some elements
```elm
{ flag = "🇦🇺", code = "AU", name = "Australia" }
{ flag = "🇦🇶", code = "AQ", name = "Antarctica" }
{ flag = "🇱🇧", code = "LB", name = "Lebanon" }
```

you can specify aspects that will be unique across all elements
```elm
KeysSet.promising
    [ unique .flag, unique .code ]
```

With a key and an aspect to check for matches, you can find the matching element:
```elm
|> KeysSet.element ( .flag, "🇦🇶" )
--→ Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

|> KeysSet.element ( .code, "LB" )
--→ Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }
```

&nbsp;


## 👍 How to

### Example: operators

```elm
operators =
    KeysSet.promising
        [ unique .symbol, unique .name ]
        |> KeysSet.insertList
            [ { symbol = ">", name = "gt", kind = Binary }
            , { symbol = "<", name = "lt", kind = Binary }
            , { symbol = "==", name = "eq", kind = Binary }
            , { symbol = "-", name = "negate", kind = Unary }
            ]

infixOperators =
    operators
        |> KeysSet.mapTry
            (\operator ->
                case operator.kind of
                    Binary ->
                        { symbol = operator.symbol, kind = operator.kind }
                            |> Just
                    
                    Unary ->
                        Nothing
            )
            [ unique .symbol, unique .name ]

nameOfOperatorSymbol operatorSymbol =
    operators
        |> KeysSet.element ( .symbol, operatorSymbol )
```

### example: users

```elm
-- https://dark.elm.dmy.fr/packages/lue-bird/elm-no-record-type-alias-constructor-function/latest/
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)
import KeysSet exposing (KeysSet, unique)

type alias Account =
    RecordWithoutConstructorFunction
        { username : String
        , email : String
        , settings : Settings
        }

type alias State =
    RecordWithoutConstructorFunction
        { accounts : KeysSet Account
        , currentUserName : String
        }


initialModel =
    { accounts =
        KeysSet.promising
            [ unique .username, unique .email ]
    }

reactTo event =
    case event of
        AccountSwitched username ->
            \state -> { state | currentUserName = username }
        
        SettingsChanged updateSettings ->
            \state ->
                { state
                    | accounts =
                        state.accounts
                            |> KeysSet.elementAlter
                                ( .username, state.currentUserName )
                                updateSettings
                }
        
        Registered username email ->
            \state ->
                if
                    state.accounts
                        |> KeysSet.any (\user -> user.username == username)
                then
                    -- username taken already
                
                else if
                    state.accounts
                        |> KeysSet.any (\user -> user.email == email)
                then
                    -- email taken already

                else
                    { state
                        | accounts =
                            state.accounts
                                |> KeysSet.insert
                                    { username = username
                                    , email = email
                                    , settings = defaultSettings
                                    }
                    }
```

&nbsp;


## 👎 How not to

## Example: automatic answers
```elm
answers =
    KeysSet.promising [ unique .youSay ]
        |> KeysSet.insertList
            [ { youSay = "Hi"
              , answer = "Hi there!"
              }
            , { youSay = "Bye"
              , answer = "Ok, have a nice day and spread some love."
              }
            , { youSay = "How are you"
              , answer = "I don't have feelings :("
              }
            , { youSay = "Are you a robot"
              , answer = "I think the most human answer is 'Haha... yes'"
              }
            ]
```
We will only ever lookup answers to what `youSay`
→ use a `Dict` where it is more appropriate: **`Dict`s are for one-way access**

## Example: translation, synonyms...
```elm
translationsEnDe =
    KeysSet.promising []
        |> KeysSet.insertList
            [ { english = "elm", german = "Ulme" }
            , { english = "git", german = "Schwachkopf" }
            , { german = "Rüste", english = "elm" }
            ]
```
A `KeysSet` is only effective when there is **only one matching key**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict) instead!

## Example: partners, opposites...

```elm
partners =
    KeysSet.promising
        [ unique .partner, unique .partnerOfPartner ]
        |> KeysSet.insertList
            [ { partner = "Ann", partnerOfPartner = "Alan" }
            , { partner = "Alex", partnerOfPartner = "Alastair" }
            , { partner = "Alan", partnerOfPartner = "Ann" }
            -- wait, this is no duplicate and is inserted
            ]
```
A `KeysSet` ony makes sense when the **keys describe something different**

# future ideas

  - integrate tag into `Order`, so that practically no manual opaque rules are needed
      - for example
        ```elm
        Int.Order.increasing
        --: Ordering Int Int.Order.Increasing

        Order.by Record.Typed.name Int.Order.increasing
            |> Order.onTie
                (Order.by Record.Typed.status
                    (String.Order.greaterEarlier
                        (Char.Order.alphabetically Case.lowerUpper)
                    )
                )
        --: Ordering
        --:     User
        --:     (Order.OnTieNext
        --:         (Order.By Record.Typed.Name Int.Order.Increasing)
        --:         (Order.By Record.Typed.Status
        --:             (String.Order.GreaterEarlier
        --:                 (Char.Order.Alphabetically Case.LowerUpper)
        --:             )
        --:         )
        --:     )
        ```
        with per project one
        ```elm
        module Record.Typed exposing (Name, name, Status, status)

        import Typed exposing (Typed, Internal, Public, tag, isChecked)

        type Name -- no (..)
            = Name
        
        type Status -- no (..)
            = Status
        
        name : Typed Internal Name Public ({ record | name : name } -> name)
        name =
            .name |> tag Name |> isChecked Name
        
        status : Typed Internal Status Public ({ record | status : status } -> status)
        status =
            .status |> tag Status |> isChecked Status
        ```
        I think that would come out neat!
          - needs an `elm-review` tool to auto-generate `Record.Typed`
          - 👎 chaining with `onTie` is slightly more verbose than `onTieNext [ ... ]`
          - 👍 chaining with `onTie` is more obvious and easier to read than `onTieNext [ ... ]`
  - require supplying `KeysSet.Uniqueness` from the user on each access/operation
      - using opaque tags like `KeySet`
      - only allow unique keys to be accessed
  - set with multiple elements per key (= multi-set/bag) add
  - `KeySet` functionality include in `KeysSet`?
      - 👍 `KeysSet` functionality while still providing `log n` for the most prominent key
      - 👎 minimally more complex API
      - only one prominent key? can we create a `KeySet` for each?
  - ✨ your idea
