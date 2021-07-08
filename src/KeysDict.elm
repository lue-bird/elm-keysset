module KeysDict exposing
    ( KeysDict
    , enterableBy
    , equal, at, size
    , insert, remove
    , fold, map, toList, toDict, toAssocList, serialize
    )

{-|


# KeysDict

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

@docs KeysDict


## create

@docs enterableBy


## scan

@docs equal, at, size


## modify

@docs insert, remove


## transform

@docs fold, map, toList, toDict, toAssocList, serialize

-}

import AssocList
import Dict
import KeysDict.Uniqueness as Uniqueness exposing (Uniqueness)
import Serialize exposing (Codec)
import Util
    exposing
        ( any
        , aspect
        , equalIgnoringOrder
        , firstWhere
        )


{-| Unsorted data structure, which promises `Uniqueness` for multiple doors.
Read the module doc for more!

    type alias CasedLetter =
        { lowercase : Char
        , uppercase : Char
        }

    casedLetters : KeysDict CasedLetter
    casedLetters =
        KeysDict.enterableBy
            [ door .lowercase, door .uppercase ]
            |> KeysDict.insert { lowercase = 'a', uppercase = 'A' }
            |> KeysDict.insert { lowercase = 'b', uppercase = 'B' }
            |> KeysDict.insert { lowercase = 'c', uppercase = 'C' }

    uppercase char =
        lowerUppercaseLetters
            |> KeysDict.at { door = .lowercase, key = char }
            |> Maybe.map .uppercase

-}
type KeysDict element
    = ElementsWithUniquenessPromises (List (Uniqueness element)) (List element)


{-| How can you know if each element in `aMultiDict` can also be found in `bMultiDict`?

    letterCodes=
      [ { letter= 'a', code= 97 }
      , { letter= 'b', code= 98 }
      ]
        |> List.foldl KeysDict.insert
            (KeysDict.enterableBy
                [ door .letter, door .code ]
            )

    fancyCompetingLetterCodes=
        KeysDict.enterableBy
            [ door .code, door .letter ]
            |> KeysDict.insert { code= 98, letter= 'b' }
            |> KeysDict.insert { code= 97, letter= 'a' }

    (==) letterCodes fancyCompetingLetterCodes
    --> elm crashes

Because a `KeysDict`'s `Uniqueness` is defined as functions.

    (==)
      (KeysDict.toList letterCodes)
      (KeysDict.toList fancyCompetingLetterCodes)
    --> False

Even though both contain the same elements but in a different order.


### take away

> Don't use `==` to compare `KeysDict`s

> With non-comparable types, thinking about order doesn't make much sense.
> You shouldn't rely on it when using functions like `fold` or `toList`.

Instead, you should use

    KeysDict.equal
        letterCodes
        fancyCompetingLetterCodes
    --> True

_Elements must not contain functions or json. Elm will crash trying to see if they are equal._

-}
equal :
    KeysDict element
    -> KeysDict element
    -> Bool
equal =
    aspect toList equalIgnoringOrder


{-| A `KeysDict` with no elements inside, promising that given aspects are unique across all elements (see `Uniqueness`).
-}
enterableBy : List (Uniqueness element) -> KeysDict element
enterableBy uniqueness =
    ElementsWithUniquenessPromises uniqueness []


{-| `Just` the element where `door` of the element matches the `key`;
if no such element is found, `Nothing`.

    casedLetters =
        KeysDict.enterableBy
            [ door .lowercase, door .uppercase ]
            |> KeysDict.insert { lowercase = 'a', uppercase = 'A' }
            |> KeysDict.insert { lowercase = 'b', uppercase = 'B' }

    lowercase char =
        casedLetters
            |> KeysDict.at { door = .uppercase, key = char }
            |> Maybe.map .lowercase

    uppercase char =
        casedLetters
            |> KeysDict.at { door = .lowercase, key = char }
            |> Maybe.map .uppercase

**Note**: If keys of `door` aren't promised to be unique,
`KeysDict.at` will find the most recently inserted element where `door` of the element matches the `key`.

    ratedCasedLetters=
        KeysDict.enterableBy
            [ door .lowercase, door .uppercase ]
            |> KeysDict.insert
                { rating= 0.5, lowercase= 'a', uppercase= 'A' }
            |> KeysDict.insert
                { rating= 0.5, lowercase= 'b', uppercase= 'B' }

    KeysDict.at { door= .rating, key= 0.5 } ratedCasedLetters
    --> { rating= 0.5, lowercase= 'b', uppercase= 'B' }

-}
at :
    { door : element -> key, key : key }
    -> KeysDict element
    -> Maybe element
at { door, key } =
    toList >> firstWhere (door >> (==) key)


{-| How many elements there are.

    List.foldl KeysDict.insert
        (KeysDict.enterableBy
            [ door .number, door .following ]
        )
        (List.map (\i-> { number= i, following= i +1 })
            (List.range 0 41)
        )
        |> KeysDict.size
    --> 42

-}
size : KeysDict element_ -> Int
size =
    toList >> List.length


isntUnique : element -> KeysDict element -> Bool
isntUnique element =
    \(ElementsWithUniquenessPromises keys elementList) ->
        keys
            |> List.map
                (\uniqueness ->
                    any
                        (\bElement ->
                            Uniqueness.violated element bElement uniqueness
                        )
                        elementList
                )
            |> List.foldl (||) False


{-| Put an element into `KeysDict`. _Keys must not contain functions or json. Elm will crash trying to see if they match._

If there is already an element with the same **key** is already **present**, (see `Uniqueness`), the `KeysDict` remains **unchanged**.

    KeysDict.enterableBy
        [ door .lowercase, door .uppercase ]
        |> KeysDict.insert { lowercase= 'b', uppercase= 'B', rating= 0.5 }
            -- put up
        |> KeysDict.insert { lowercase= 'a', uppercase= 'A', rating= 0.5 }
            -- put up, because rating is not a key
        |> KeysDict.insert { lowercase= 'b', uppercase= 'C', rating= 0 }
            -- ignored, the .lowercase already exists
        |> KeysDict.insert { lowercase= 'c', uppercase= 'A', rating= 0 }
            -- ignored, the .uppercase already exists
        |> KeysDict.insert { lowercase= 'c', uppercase= 'C', rating= 0.6 }
            --put up

-}
insert :
    element
    -> KeysDict element
    -> KeysDict element
insert element =
    \multiDict ->
        if isntUnique element multiDict then
            multiDict

        else
            updateElements
                (\elementList -> element :: elementList)
                multiDict


updateElements :
    (List element -> List element)
    -> KeysDict element
    -> KeysDict element
updateElements update =
    \(ElementsWithUniquenessPromises keys elementList) ->
        ElementsWithUniquenessPromises keys
            (update elementList)


{-| Reduce the elements from most recently to least recently inserted element.

> With non-comparable types, thinking about order doesn't make much sense.
> You shouldn't rely on it when using functions like folds or `toList`.


    brackets =
        KeysDict.enterableBy
            [ door .open, door .closed ]
            |> KeysDict.insert { open = '(', closed = ')' }
            |> KeysDict.insert { open = '{', closed = '}' }

    openingAndClosing =
        brackets
            |> KeysDict.fold
                (\{ open, closed } ->
                    (::) (String.fromList [ open, closed ])
                )
                []

    --> []
    -->     |> (::) (String.fromList [ '{' '}' ])
    -->     |> (::) (String.fromList [ '(' ')' ])

-}
fold :
    (element -> acc -> acc)
    -> acc
    -> KeysDict element
    -> acc
fold reduce initial =
    toList
        >> List.foldl reduce initial


{-| Remove the element where `door` of the element matches the `key`.
If **the key does not exist**, the `KeysDict` is **unchanged**

    openClosedBrackets=
        KeysDict.enterableBy
            [ door .open, door .closed ]
            |> KeysDict.insert { open= "(", closed= ")" }

    openClosedBrackets
        |>  KeysDict.remove { door= .open, key= ")" }
            -- unchanged, ")" does not exist as a .open key
        |>  KeysDict.remove { door= .open, key= "(" }
    --> KeysDict.enterableBy
    -->     [ door .open, door .closed ]

    openClosedBrackets
        |> KeysDict.remove { door= .closed, key= "(" }
            -- unchanged, "(" does not exist as a .closed key
        |> KeysDict.remove { door= .closed, key= ")" }
    --> KeysDict.enterableBy
    -->     [ door .open, door .closed ]

**Notice:** If there is no promise of `Uniqueness` for `door`, `remove` acts as a normal filter.

    KeysDict.enterableBy
        [ door .open, door .closed ]
        |> KeysDict.insert { open= "(", closed= ")", meaning= Nothing }
        |> KeysDict.insert { open= "[", closed= "]", meaning= Just List }
        |> KeysDict.insert { open= "<, closed= ">", meaning= Nothing }
        |> KeysDict.remove { door= .meaning, key= Nothing }

    --> KeysDict.enterableBy
    -->   [ door .open, door .closed ]
    --> |>KeysDict.insert { open= "[", closed= "]", meaning= Just List }

-}
remove :
    { door : element -> key, key : key }
    -> KeysDict element
    -> KeysDict element
remove { door, key } =
    updateElements
        (List.filter
            (\element -> key /= door element)
        )


{-| The `List` containing all elements from most recently (= head) to least recently inserted element.

> With non-comparable types, thinking about order doesn't make much sense.
> You shouldn't rely on it when using functions like folds or `toList`.

    isEmpty =
        List.isEmpty << KeysDict.toList

    mostRecentlyInserted =
        List.head << KeysDict.toList

    removeMostRecentlyInserted multiDict =
        case KeysDict.toList multiDict of
            _ :: rest ->
                rest

            [] ->
                KeysDict

-}
toList : KeysDict element -> List element
toList =
    \(ElementsWithUniquenessPromises _ elementList) ->
        elementList


{-| Map all elements.

    digitNames =
        KeysDict.enterableBy
            [ door .number, door .name ]
            |> KeysDict.insert { number = 0, name = "zero" }
            |> KeysDict.insert { number = 1, name = "one" }

    mathSymbolNames =
        digitNames
            |> KeysDict.map [ door .symbol, door .name ]
                (\{ number, name } ->
                    { symbol = String.fromInt number, name = name }
                )
            |> KeysDict.insert { symbol = "+", name = "plus" }

-}
map :
    (element -> resultElement)
    -> List (Uniqueness resultElement)
    -> KeysDict element
    -> KeysDict resultElement
map alter resultUniqueness =
    enterableBy resultUniqueness
        |> fold (alter >> insert)


{-| Convert a `KeysDict` to a `Dict`.

    casedLetters =
        KeysDict.enterableBy
            [ door .lowercase, door .uppercase ]
            |> KeysDict.insert { uppercase = 'A', lowercase = 'a' }
            |> KeysDict.insert { uppercase = 'B', lowercase = 'b' }

    lowerFromUppercase =
        KeysDict.toDict
            { key = .uppercase, value = .lowercase }
            casedLetters

-}
toDict :
    { key : element -> comparableKey, value : element -> value }
    -> KeysDict element
    -> AssocList.Dict comparableKey value
toDict { key, value } =
    fold
        (\element ->
            AssocList.insert (key element) (value element)
        )
        AssocList.empty


{-| Convert a `KeysDict` to an [association-`Dict`](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest/).

    elements =
        KeysDict.enterableBy
            [ door .lowercase, door .uppercase ]
            |> KeysDict.insert { element = H, protons = 1 }
            |> KeysDict.insert { element = He, protons = 2 }

    protonsOfElement =
        elements
            |> KeysDict.toAssocList
                { key = .element, value = .protons }

-}
toAssocList :
    { key : element -> key, value : element -> value }
    -> KeysDict element
    -> AssocList.Dict key value
toAssocList { key, value } =
    fold
        (\element ->
            AssocList.insert (key element) (value element)
        )
        AssocList.empty


{-| [Serialize](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/Serialize) a `KeysDict`.

    type alias NamedNumber=
      { name: String
      , number: Int
      }

    namedNumberCodec=
      Serialize.record NamedNumber
      |>Serialize.field .name Decode.string
      |>Serialize.field .number Decode.int
      |>Serialize.finishRecord

    namedNumberMultiDictCodec=
      KeysDict.codec namedNumberCodec
        [ door .number, door .name ]

    someMultiDict=
      KeysDict.enterableBy
        [ door .number, door .name ]
      |>KeysDict.insert { number= 1, name= "one" }
      |>KeysDict.insert { number= 2, name= "two" }

    Serialize.encodeToJson
      (NamedNumberKeyDictCodec someMultiDict)
    |>Json.Encode.encode 1

    --> """
    --> [
    -->  {
    -->   \"number\": 2,
    -->   \"name\": "two"
    -->  },
    -->  {
    -->   \"number\": 1,
    -->   \"name\": "one"
    -->  }
    --> ]
    --> """

    """
    [
     {
      \"number\": 2,
      \"name\": "two"
     },
     {
      \"number\": 1,
      \"name\": "one"
     }
    ]
    """
    |>Json.Decode.decodeString
        (Serialize.decodeFromJson
          namedNumberMultiDictCodec
        )

    --> Ok
    -->   (KeysDict.enterableBy
    -->     [ door .number, door .name ]
    -->   |>KeysDict.insert { number= 1, name= "one" }
    -->   |>KeysDict.insert { number= 2, name= "two" }
    -->   )

-}
serialize :
    Codec customError element
    -> List (Uniqueness element)
    -> Codec customError (KeysDict element)
serialize serializeElement uniqueness =
    Serialize.map
        (List.foldl insert (enterableBy uniqueness))
        toList
        (Serialize.list serializeElement)
