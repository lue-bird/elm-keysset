> safe lookup for multiple arbitrary keys, log n

# 🗃️ `KeysSet`

Holds no functions.
Still, the [`Keys`](Keys#Keys)
required to access/operate are enforced to be the same
(by attaching an opaque tag).
See ↓ example

→ Solves problems listed in [prior art](#prior-art)
alongside other [goodies](#goodies)

```elm
import Emptiable exposing (Emptiable)
import Stack
import KeysSet exposing (KeysSet)
import User exposing (User(..))

users : Emptiable (KeysSet User User.ByEmailHostFirst N1) neverEmpty_
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

type EmailTag
    -- ! no exposing (..) → only constructable in this module
    = Email

email : Map User EmailTag Email
email =
    Typed.tag Email (\(User userData) -> userData.email)

emailHostFirst : Keys User ByEmailHostFirst N1
emailHostFirst =
    Keys.for (\email -> { email = email })
       |> Keys.by .email ( email, emailOrder )

type alias ByEmailHostFirst =
    ...

type alias ByHostFirst =
    Order.OnTieNext
        (Order.By Email.HostTag ...)
        (Order.By Email.LabelTag ...)

emailByHostFirst : Ordering Email ByHostFirst 
emailByHostFirst =
    Order.by Email.host
        (String.Order.earlier (Char.Order.alphabetically Order.tie))
        |> Order.onTie
            (Order.by Email.label
                (String.Order.earlier (Char.Order.alphabetically Order.tie))
            )
```
No typeclasses :)

Feel free to adapt this structure how you like it best,
for example separating [`Ordering`](Order#Ordering)s from data to each their own `module Data.By`

## goodies

  - ⚖ orderKey by [`Ordering key = ... key, key -> Order`](Order#Ordering)
      - 👍 no reliance on `comparable`
      - 👍 no inconvenient `key -> String`
  - 🔑 `element -> key` function as part of a given [`Key`](Keys#Key)
      - 👍 simpler type
      - 👍 simpler internals :)
      - same idea is also implemented in
          - [`escherlies/elm-ix-dict`: `IxDict`](https://package.elm-lang.org/packages/escherlies/elm-ix-dict/latest/IxDict)
          - [`Orasund/elm-bag` `Bag`](https://package.elm-lang.org/packages/Orasund/elm-bag/latest/Bag))
  - 🗃 emptiability is part of the type
      - just use the same API with emptiable or non-empty conveniently
      - 👍 extra safety possible. Got enough elements? → `KeySet.end Up|Down`, `foldFromOne`, `fold` don't need `Maybe`
      - 🧩 [`allowable-state`](https://dark.elm.dmy.fr/packages/lue-bird/elm-allowable-state/latest/)
      - 🧩 [`emptiness-typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/)
  - ↔ supply the direction as an argument
      - 🧩 [`linear-direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/)

## prior art

  - `comparableKey`
      - examples
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
      - `... -> comparable`
          - examples
              - [`timo-weike/generic-collections`](https://dark.elm.dmy.fr/packages/timo-weike/generic-collections/latest/)
              - [`turboMaCk/any-dict`](https://dark.elm.dmy.fr/packages/turboMaCk/any-dict/latest/)
              - [`Orasund/elm-bag` `Bag`](https://package.elm-lang.org/packages/Orasund/elm-bag/latest/Bag)
              - [`escherlies/elm-ix-dict`: `IxDict`](https://package.elm-lang.org/packages/escherlies/elm-ix-dict/latest/)
          - `key -> String`
              - examples (in no specific order)
                  - [`matzko/elm-opaque-dict`](https://dark.elm.dmy.fr/packages/matzko/elm-opaque-dict/latest/)
                  - [`edkv/elm-generic-dict`](https://dark.elm.dmy.fr/packages/edkv/elm-generic-dict/latest/)
              - 👍 avoid having an extra type variable
              - 👎 requires more work
              - 👎 more prone to bugs in `toString` implementation not returning a unique `String` for all keys
              - 👎 slightly less performant when `toString` needs to do heavy work
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

## more examples!
Look up elements by their unique aspects

For a `KeysSet` with some elements
```elm
{ flag = "🇦🇺", code = "AU", name = "Australia" }
{ flag = "🇦🇶", code = "AQ", name = "Antarctica" }
{ flag = "🇱🇧", code = "LB", name = "Lebanon" }
```

you can specify aspects that will be unique across all elements
```elm
keys : Keys Country CountryKeys N2
keys =
    Keys.for (\flag_ code_ -> { flag = flag_, code = code_ })
        |> Keys.by ( .flag, flag )
            (String.Order.earlier Char.Order.unicode)
        |> Keys.by ( .code, code )
            (String.Order.earlier (Char.Order.alphabetically Order.tie))

type alias CountryKeys =
    { flag : Key Country (Order.By Flag (String.Order.Earlier Char.Order.Unicode)) (Up N0 To N1)
    , code : Key Country (Order.By Code (String.Order.Earlier (Char.Order.Alphabetically Order.Tie))) (Up N1 To N1)
    }
```

With a key and an aspect to check for matches, you can find the matching element:
```elm
|> KeysSet.element ( keys, .flag ) "🇦🇶"
--→ Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

|> KeysSet.element ( keys, .code ) "LB"
--→ Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }
```

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
import KeysSet exposing (KeysSet)

type alias Account =
    RecordWithoutConstructorFunction
        { name : String -- in reality, use a custom type
        , email : String -- in reality, use a custom type
        , settings : Settings
        }

type alias State =
    RecordWithoutConstructorFunction
        { accounts : Emptiable (KeysSet Account AccountKeys N2) Possibly
        , currentAccountName : String
        }

initialState : State
initialState =
    { accounts = Emptiable.empty }

accountKeys : Keys Account AccountKeys N2
accountKeys =
    Keys.for (\name_ email_ -> { name = name_, email = email_ })
        |> Keys.by ( .name, name )
            (String.Order.earlier (Char.Order.alphabetically Order.tie))
        |> Keys.by ( .email, email )
            (String.Order.earlier (Char.Order.alphabetically Order.tie))

type alias AccountKeys =
    { name : Key Account (String.Order.Earlier (Char.Order.Alphabetically Order.Tie)) (Up N0 To N0)
    , email : Key Account (String.Order.Earlier (Char.Order.Alphabetically Order.Tie)) (Up N0 To N1)
    }

reactTo event =
    case event of
        AccountSwitched name ->
            \state -> { state | currentAccountName = name }
        
        SettingsChanged updateSettings ->
            \state ->
                { state
                    | accounts =
                        state.accounts
                            |> KeysSet.elementAlter
                                ( .name, state.currentAccountName )
                                updateSettings
                }
        
        Registered name email ->
            \state ->
                case
                    state.accounts
                        |> KeysSet.element ( accountKeys, .name ) name
                of
                    Just _ ->
                        -- name taken already
                
                    Nothing ->
                        case
                            state.accounts
                                |> KeysSet.element ( accountKeys, .email ) email
                        of
                            Just _ ->
                                -- email taken already

                            Nothing ->
                                { state
                                    | accounts =
                                        state.accounts
                                            |> KeysSet.insert accountKeys
                                                { name = name
                                                , email = email
                                                , settings = defaultSettings
                                                }
                                }
```

&nbsp;

## Example: automatic answers
```elm
type alias ConversationStep =
    { youSay : String, answer : String }

type alias ByYouSay =
    { youSay : Key ConversationStep (Order.By YouSay (String.Order.Earlier (Char.Order.Alphabetically Order.Tie))) String (Up N0 To N0) }

youSayKey : Keys ConversationStep ByYouSay N1
youSayKey =
    Keys.for (\youSay_ -> { youSay = youSay_ })
        |> Keys.by ( .youSay, youSay )
            (String.Order.earlier (Char.Order.alphabetically Order.tie))

answers : Emptiable (KeysSet ConversationStep ByYouSay N1) Possibly
answers =
    KeysSet.fromList youSayKey
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


## 👎 How not to

## Example: translation, synonyms...
```elm
translationsEnDe =
    KeysSet.fromList ???
        [ { english = "elm", german = "Ulme" }
        , { english = "git", german = "Schwachkopf" }
        , { german = "Rüste", english = "elm" }
        ]
```
A `KeysSet` is only effective when there is **only one matching key**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict) instead!

## Example: partners, opposites...

```elm
partnerKeys =
    Keys.for
        (\partner partnerOfPartner ->
            { partner = partner, partnerForPartner = partnerForPartner }
        )
        |> Keys.by .partner
            ( Record.Map.partner, String.Order... )
        |> Keys.by .partnerOfPartner
            ( Record.Map.partnerOfPartner, String.Order... )

partners =
    KeysSet.fromList partnerKeys
        [ { partner = "Ann", partnerOfPartner = "Alan" }
        , { partner = "Alex", partnerOfPartner = "Alastair" }
        , { partner = "Alan", partnerOfPartner = "Ann" }
        -- wait, this is no duplicate and is inserted
        ]
```
A `KeysSet` ony makes sense when the **keys describe something different**

# future ideas

  - set with multiple elements per key (= multi-set/bag) add?
    or is this already covered good enough
  - ✨ your idea
