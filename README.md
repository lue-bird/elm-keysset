# `KeysDict`
Lookup elements by their unique aspects.

- a "door" describes a unique aspect across all elements.
```elm
door .operator
```
So... If you have a key and the type of door it could match, you can find the only matching element.

> You want the element where `🗝️` is `1`?

```noformatingples
    🔑= 0, 🏠= 🏚, 🗝️= 2
    🔑= 2, 🏠= 🏡, 🗝️= 0
    🔑= 1, 🏠= 🏚, 🗝️= 1 <--

🔑, 🗝️: doors you can "open" with a key unique across all elements
```

> Going through while checking every element, if the `🗝️` matches.

        🔑= 1, 🏠= 🏚, 🗝️= 1  where 🗝️ is 1   
        
> You want the element where `🔑` is `0`?

```noformatingples
--> 🔑= 0, 🏠= 🏚, 🗝️= 2
    🔑= 2, 🏠= 🏡, 🗝️= 0
    🔑= 1, 🏠= 🏚, 🗝️= 1

🔑, 🗝️: doors you can "open" with a key unique across all elements
```

> Going through while checking every element, if the `🔑` matches.

```noformatingples
    🔑= 0, 🏠= 🏚, 🗝️= 2  where 🔑 is 0
```

&nbsp;


## 👍 How to `KeysDict`

### setup

```elm
import KeysDict.Uniqueness exposing (door)
import KeysDict exposing (KeysDict)
```

Try the [ellie for some examples](https://ellie-app.com/cHj9Fy9bpXMa1) (always a version behind).

### Example: cased letters
```elm
type alias CasedLetter=
    { lowercase : Char
    , uppercase : Char
    }

lowerUppercaseLetters: KeysDict CasedLetter
lowerUppercaseLetters=
    KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
        |> KeysDict.insert { lowercase = 'a', uppercase = 'A' }
        |> KeysDict.insert { lowercase = 'b', uppercase = 'B' }
        |> KeysDict.insert { lowercase = 'c', uppercase = 'C' }

uppercase char=
    lowerUppercaseLetters
        |> KeysDict.at { door = .lowercase, key = char }
        |> Maybe.map .uppercase
```

### Example: periodic table

```elm
type Element
    = Hydrogen
    | Helium

elementAtomicNumberMultiDict=
    KeysDict.enterableBy
        [ door .atomicNumber, door .element ]
        |> KeysDict.insert
            { element = Hydrogen, atomicNumber = 1 }
        |> KeysDict.insert
            { element = Helium, atomicNumber = 2 }

atomicNumberByElement=
    KeysDict.toDict
        { key = .element, value = .atomicNumber }
        elementAtomicNumberMultiDict
```

### Example: brackets

```elm
brackets=
    KeysDict.enterableBy
        [ door .opening, door .closing ]
        |> KeysDict.insert
            { opening = '(', closing = ')' }
        |> KeysDict.insert
            { opening = '{', closing = '}' }

typeChar character=
    case
        brackets
            |> MultiDict.at { door = .opening, key = character }
    of
        Just { closing }->
            String.fromValues [ character, closing ]

        Nothing->
            case
                brackets
                    |> MultiDict.at { door = .closing, key = character }
            of
                Just { opening }->
                    String.fromValues [ opening, character ]
                  
                Nothing->
                    String.fromChar character

"Typing (: " ++ typeChar '(' ++ ". Even }: " ++ typeChar '}'
```
&nbsp;


## 👎 How not to `KeysDict`

## Example: automatic answers
```elm
answers =
    KeysDict.enterableBy [ door .youSay ]
        |> KeysDict.insert
            { youSay = "Hi"
            , answer = "Hi there!"
            }
        |> KeysDict.insert
            { youSay = "Bye"
            , answer = "Ok, have a nice day and spread some love." }
        |> KeysDict.insert
            { youSay = "How are you"
            , answer = "I don't have feelings :("
            }
        |> KeysDict.insert
            { youSay = "Are you a robot"
            , answer = "I think the most human answer is 'Haha... yes'"
            }
```
We will only ever lookup answers to what `youSay`
→ use a `Dict` where it is more appropriate: **`Dict`s are for one-way access**.

## Example: translation, synonymes...
```elm
englishGerman =
    KeysDict.enterableBy []
        |> KeysDict.insert
            { english = "elm", german = "Ulme" }
        |> KeysDict.insert
            { english = "git", german = "Schwachkopf" }
        |> KeysDict.insert
            { german = "Rüste", english = "elm" }
```
A `KeysDict` is only effective, when there is **only one unique key for each door**.

Please take a look at [elm-bidict](https://github.com/Janiczek/elm-bidict) instead!

## Example: partners, opposites...

Similar to the previous example:
```elm
partners =
    KeysDict.enterableBy
        [ door .partner, door .partnerOfPartner ]
        |> KeysDict.insert
            { partner = "Ann", partnerOfPartner = "Alan" }
        |> KeysDict.insert
            { partner = "Alex", partnerOfPartner = "Alastair" }
        |> KeysDict.insert
            { partner = "Alan", partnerOfPartner = "Ann" }
        --wait, this is no duplicate and is inserted
```
A `KeysDict` ony makes sense, when the **keys describe something different**.
