# `KeysSet`
> Lookup elements by their unique aspects.

For a `KeysSet` with some elements
```elm
{ flag = "🇦🇺", code = "AU", name = "Australia" }
{ flag = "🇦🇶", code = "AQ", name = "Antarctica" }
{ flag = "🇱🇧", code = "LB", name = "Lebanon" }
```
you can specify aspects that will be unique across all elements.
```elm
KeysSet.promising
    [ unique .flag, unique .code ]
```
If you have a key and the aspect to check if it matches, you can find the matching element:

```elm
KeysSet.at .flag "🇦🇶"
--> Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

KeysSet.at .code "LB"
--> Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }
```

&nbsp;


## 👍 How to

### Example: operators

```elm
operators =
    KeysSet.promising
        [ unique .symbol, unique .name ]
        |> KeysSet.insertAll
            [ { symbol = ">", name = "gt", kind = Infix }
            , { symbol = "<", name = "lt", kind = Infix }
            , { symbol = "==", name = "eq", kind = Infix }
            , { symbol = "-", name = "negate", kind = Prefix }
            ]

infixOperators =
    operators
        |> KeysSet.when (.kind >> (==) Infix)

nameOfOperatorSymbol operatorSymbol =
    operators
        |> KeysSet.at .symbol operatorSymbol
```

### example: users

```elm
import KeysSet.Uniqueness exposing (unique)
import KeysSet exposing (KeysSet)

type alias Account =
    { username : String
    , email : String
    , settings : Settings
    }

type alias Model =
    { accounts : KeysSet Account
    , currentUserName : String
    }


initialModel =
    { accounts =
        KeysSet.promising
            [ unique .username, unique .email ]
    }

update msg model =
    case msg of
        AccountSwitched username ->
            { model | currentUserName = username }
        
        SettingsChanged updateSettings ->
            { model
              | accounts =
                  model.accounts
                      |> KeysSet.update .username   
                          model.currentUserName
                          updateSettings
            }
        
        Registered username email ->
            if
                model.accounts
                    |> KeysSet.any (.username >> (==) username)
            then
                -- username already taken
            
            else if
                model.accounts
                    |> KeysSet.any (.email >> (==) email)
            then
                -- email already taken

            else
                { model
                  | accounts =
                      model.accounts
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
        |> KeysSet.insertAll
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
    KeysSet.promising []
        |> KeysSet.insertAll
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
        |> KeysSet.insertAll
            [ { partner = "Ann", partnerOfPartner = "Alan" }
            , { partner = "Alex", partnerOfPartner = "Alastair" }
            , { partner = "Alan", partnerOfPartner = "Ann" }
            -- wait, this is no duplicate and is inserted
            ]
```
A `KeysSet` ony makes sense when the **keys describe something different**.
