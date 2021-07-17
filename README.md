# `MultiSet`
> Lookup elements by their unique aspects.

For a `MultiSet` with some elements
```elm
{ flag = "🇦🇺", code = "AU", name = "Australia" }
{ flag = "🇦🇶", code = "AQ", name = "Antarctica" }
{ flag = "🇱🇧", code = "LB", name = "Lebanon" }
```
you can specify aspects that will be unique across all elements.
```elm
MultiSet.promising
    [ unique .flag, unique .code ]
```
If you have a key and the aspect to check if it matches, you can find the matching element:

```elm
MultiSet.at .flag "🇦🇶"
--> Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

MultiSet.at .code "LB"
--> Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }
```

&nbsp;


## 👍 How to

### example: users

```elm
import MultiSet.Uniqueness exposing (unique)
import MultiSet exposing (MultiSet)

type alias Account =
    { username : String
    , email : String
    , settings : Settings
    }

type alias Model =
    { accounts : MultiSet Account }


initialModel =
    { accounts =
        MultiSet.promising
            [ unique .atomicNumber, unique .symbol ]
    }

update msg model =
    case msg of
        LoggedIn username ->
            { model | selectedUsername = username }
        
        Registered username email ->
            if
                model.accounts
                    |> MultiSet.any (.username >> (==) username)
            then
                -- username already taken
            
            else if
                model.accounts
                    |> MultiSet.any (.email >> (==) email)
            then
                -- email already taken

            else
                { model
                  | accounts =
                      model.accounts
                          |> MultiSet.insert
                              { username = username
                              , email = email
                              , settings = defaultSettings
                              }
                }
            
        ChangedSettings updateSettings username ->
            { model
              | accounts =
                  model.accounts
                      |> MultiSet.update .username username
                          updateSettings
            }
```

### Example: operators

```elm
operators =
    MultiSet.promising
        [ unique .symbol, unique .name ]
        |> MultiSet.insertAll
            [ { symbol = ">", name = "gt", kind = Infix }
            , { symbol = "<", name = "lt", kind = Infix }
            , { symbol = "==", name = "eq", kind = Infix }
            , { symbol = "-", name = "negate", kind = Prefix }
            ]

infixOperators =
    operators
        |> MultiSet.when (.kind >> (==) Infix)

nameOfOperatorSymbol operatorSymbol =
    operators
        |> MultiSet.at .symbol operatorSymbol
```
&nbsp;


## 👎 How not to

## Example: automatic answers
```elm
answers =
    MultiSet.promising [ unique .youSay ]
        |> MultiSet.insertAll
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
→ use a `Dict` where it is more appropriate: **`Dict`s are for one-way access**.

## Example: translation, synonymes...
```elm
translationsEnDe =
    MultiSet.promising []
        |> MultiSet.insertAll
            [ { english = "elm", german = "Ulme" }
            , { english = "git", german = "Schwachkopf" }
            , { german = "Rüste", english = "elm" }
            ]
```
A `MultiSet` is only effective when there is **only one matching key**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict) instead!

## Example: partners, opposites...

```elm
partners =
    MultiSet.promising
        [ unique .partner, unique .partnerOfPartner ]
        |> MultiSet.insertAll
            [ { partner = "Ann", partnerOfPartner = "Alan" }
            , { partner = "Alex", partnerOfPartner = "Alastair" }
            , { partner = "Alan", partnerOfPartner = "Ann" }
            -- wait, this is no duplicate and is inserted
            ]
```
A `MultiSet` ony makes sense when the **keys describe something different**.
