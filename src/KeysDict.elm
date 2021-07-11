module KeysDict exposing
    ( KeysDict
    , promising
    , Uniqueness, unique
    , equal, isEmpty, at, size, isUnique, all, any
    , insert, insertAll, remove, update, updateAll
    , when, dropWhen
    , fold, toList, serialize
    )

{-|

@docs KeysDict


## create

@docs promising


### uniqueness

@docs Uniqueness, unique


## scan

@docs equal, isEmpty, at, size, isUnique, all, any


## modify

@docs insert, insertAll, remove, update, updateAll


### filter

@docs when, dropWhen


## transform

@docs fold, toList, serialize

-}

import Serialize exposing (Codec)
import Typed exposing (Checked, Internal, Typed, internalVal, isChecked, tag)
import Util exposing (aspect, equalIgnoringOrder, firstWhere)


{-| Unsorted data structure where you can specify aspects that will be unique across all elements.

    countries =
        KeysDict.promising
            [ unique .flag, unique .code ]
            |> KeysDict.insertAll
                [ { flag = "🇦🇺", code = "AU", name = "Australia" }
                , { flag = "🇦🇶", code = "AQ", name = "Antarctica" }
                , { flag = "🇱🇧", code = "LB", name = "Lebanon" }
                ]

If you have a key and the aspect to check if it matches, you can find the matching element:

    KeysDict.at .flag "🇦🇶" countries
    --> Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

    KeysDict.at .code "LB" countries
    --> Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }

-}
type alias KeysDict element =
    Typed Checked ElementsWithUniquenessPromises Internal { uniqueness : List (Uniqueness element), elements : List element }


{-| **Should not be exposed.**
-}
type ElementsWithUniquenessPromises
    = KeysDict


{-| Check 2 elements if they are equal in a specified aspect. See [unique](KeysDict#unique)

    uniqueInCasedLetter =
        [ unique .inAlphabet
        , unique .lowercase
        , unique .uppercase
        ]

    KeysDict.promising uniqueInCasedLetter
        |> KeysDict.insert
            { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }
        |> KeysDict.insert
            { inAlphabet = 0, lowercase = 'b', uppercase = 'B' }

The second element isn't inserted. There's already an element where `.inAlphabet` is `0`.

-}
type alias Uniqueness element =
    element -> element -> { areUnique : Bool }


{-| Check elements if some aspect is not the same.

    unique .name
        { name = "smile", symbol = '😊' }
        { symbol = '🙂', name = "smile" }
    --> { areUnique = False }

    unique .symbol
        { name = "smile", symbol = '😊' }
        { symbol = '🙂', name = "smile" }
    --> { areUnique = True }

    unique (\person -> person.firstName ++ person.lastName)
        { lastName = "jimmy", firstName = "petter" }
        { lastName = "jimmy", firstName = "greg" }
    --> { areUnique = True }

in `KeysDict`

    KeysDict.promising [ unique .email ]
        |> KeysDict.insert
            { username = "ben", email = "ben10@gmx.de" }
        |> KeysDict.insert
            { username = "mai", email = "ben10@gmx.de" }

The second element isn't inserted. There's already an element where `.email` is `"ben10@gmx.de"`.

-}
unique : (element -> aspect_) -> Uniqueness element
unique aspect =
    \a b -> { areUnique = aspect a /= aspect b }


{-| A `KeysDict` with no elements inside,
promising that given aspects are unique across all elements.
See [`Uniqueness`](KeysDict#Uniqueness)

    KeysDict.promising [ unique .email ]
        |> KeysDict.insert
            { username = "ben", email = "ben10@gmx.de" }
        |> KeysDict.insert
            { username = "mai", email = "ben10@gmx.de" }

The second element isn't inserted.
There's already an element where `.email` is `"ben10@gmx.de"`.

Elements must **not** contain **functions, json or regexes**.
Elm will crash trying to see if they are equal.

-}
promising : List (Uniqueness element) -> KeysDict element
promising uniqueness_ =
    { uniqueness = uniqueness_, elements = [] }
        |> tag
        |> isChecked KeysDict


{-| How can you know if each element in `aKeysDict` can also be found in `bKeysDict`?

    letterCodes =
        KeysDict.insertAll
            [ { letter = 'a', code = 97 }
            , { letter = 'b', code = 98 }
            ]
            (KeysDict.promising
                [ unique .letter, unique .code ]
            )

    fancyCompetingLetterCodes =
        KeysDict.promising
            [ unique .code, unique .letter ]
            |> KeysDict.insert { code = 98, letter = 'b' }
            |> KeysDict.insert { code = 97, letter = 'a' }

    letterCodes == fancyCompetingLetterCodes
    --> elm crashes

Because a `KeysDict`'s `Uniqueness` is defined as functions.

    KeysDict.toList letterCodes
    == KeysDict.toList fancyCompetingLetterCodes
    --> False

Even though both contain the same elements but in a different order.


### take away

> Don't use `==` to compare `KeysDict`s

> The keys can be non-comparable. There is no obvious order.
> → You shouldn't rely on order when using functions like `fold` or `toList`.

Instead, use

    KeysDict.equal
        letterCodes
        fancyCompetingLetterCodes
    --> True

-}
equal :
    KeysDict element
    -> KeysDict element
    -> Bool
equal =
    aspect toList equalIgnoringOrder


{-| `Just` the element where `unique` of the element matches the `key`;
if no such element is found, `Nothing`.

    casedLetters =
        KeysDict.promising
            [ unique .lowercase, unique .uppercase ]
            |> KeysDict.insert { lowercase = 'a', uppercase = 'A' }
            |> KeysDict.insert { lowercase = 'b', uppercase = 'B' }

    lowercase char =
        casedLetters
            |> KeysDict.at .uppercase char
            |> Maybe.map .lowercase

    uppercase char =
        casedLetters
            |> KeysDict.at .lowercase char
            |> Maybe.map .uppercase

**Note**: If keys of `unique` aren't promised to be unique,
`KeysDict.at` will find the most recently inserted element where `unique` of the element matches the `key`.

    ratedCasedLetters =
        KeysDict.promising
            [ unique .lowercase, unique .uppercase ]
            |> KeysDict.insert
                { rating = 0.5, lowercase = 'a', uppercase = 'A' }
            |> KeysDict.insert
                { rating = 0.5, lowercase = 'b', uppercase = 'B' }

    KeysDict.at { unique = .rating, key = 0.5 } ratedCasedLetters
    --> { rating = 0.5, lowercase = 'b', uppercase = 'B' }

-}
at :
    (element -> key)
    -> key
    -> KeysDict element
    -> Maybe element
at accessAspect keyToFind =
    toList >> firstWhere (accessAspect >> (==) keyToFind)


{-| Conveniently insert a list of elements. See [insert](KeysDict#insert).

    KeysDict.promising
        [ unique .open, unique .closed ]
        |> KeysDict.insertAll
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]
    --> KeysDict.promising [ unique .open, unique .closed ]
    -->     |> KeysDict.insert { open = '(', closed = ')' }
    -->     |> KeysDict.insert { open = '{', closed = '}' }

-}
insertAll :
    List element
    -> KeysDict element
    -> KeysDict element
insertAll listOfElementsToInsert =
    \keysDict ->
        List.foldl insert
            keysDict
            listOfElementsToInsert


{-| How many elements there are.

    KeysDict.promising
        [ unique .number, unique .following ]
        |> KeysDict.insertAll
            (List.map
                (\i -> { number = i, following = i + 1 })
                (List.range 0 41)
            )
        |> KeysDict.size
    --> 42

-}
size : KeysDict element_ -> Int
size =
    toList >> List.length


{-| Are there no elements inside?

    KeysDict.promising [ unique .name ]
        |> KeysDict.isEmpty
    --> True

    KeysDict.promising [ unique .name ]
        |> KeysDict.insertAll []
        |> KeysDict.isEmpty
    --> True

    KeysDict.promising [ unique .name ]
        |> KeysDict.insert { name = "pete" }
        |> KeysDict.remove .name "pete"
        |> KeysDict.isEmpty
    --> True

-}
isEmpty : KeysDict element_ -> Bool
isEmpty =
    toList >> List.isEmpty


{-| Would this element get inserted / is it considered unique?

    letters =
        KeysDict.promising
            [ unique .lowercase, unique .uppercase ]
            |> KeysDict.insertAll
                [ { lowercase = 'a', uppercase = 'A' }
                , { lowercase = 'b', uppercase = 'B' }
                ]

    letters
        |> KeysDict.isUnique
            { lowercase = 'b', uppercase = 'C' }
    -- the .lowercase already exists
    --> False

    letters
        |> KeysDict.isUnique
            { lowercase = 'c', uppercase = 'A' }
    -- ignored, the .uppercase already exists
    --> False

    letters
        |> KeysDict.isUnique
            { lowercase = 'c', uppercase = 'C' }
    --> True

-}
isUnique : element -> KeysDict element -> Bool
isUnique element =
    \keysDict ->
        uniqueness keysDict
            |> List.any
                (\unique_ ->
                    keysDict
                        |> any
                            (\bElement ->
                                not (unique_ element bElement).areUnique
                            )
                )
            |> not


{-| Is there at least one element that passes a test?

    KeysDict.promising
        [ unique .username, unique .email ]
        |> KeysDict.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.com" }
            ]
        |> KeysDict.any (\user -> user.prioroty > 4)
    --> False

-}
any : (element -> Bool) -> KeysDict element -> Bool
any isOkay =
    toList >> List.any isOkay


{-| Do all elements pass a test?

    KeysDict.promising
        [ unique .username, unique .email ]
        |> KeysDict.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.com" }
            ]
        |> KeysDict.all (\user -> user.prioroty < 4)
    --> True

-}
all : (element -> Bool) -> KeysDict element -> Bool
all isOkay =
    toList >> List.all isOkay


{-| Put an element into `KeysDict`.

If there is already an element with the same **key** is already **present**, (see `Uniqueness`), the `KeysDict` remains **unchanged**.

    KeysDict.promising
        [ unique .lowercase, unique .uppercase ]
        |> KeysDict.insert
            { lowercase = 'b', uppercase = 'B', rating = 0.5 }
            -- put up
        |> KeysDict.insert
            { lowercase = 'a', uppercase = 'A', rating = 0.5 }
            -- put up because rating is not a key
        |> KeysDict.insert
            { lowercase = 'b', uppercase = 'C', rating = 0 }
            -- ignored, the .lowercase already exists
        |> KeysDict.insert
            { lowercase = 'c', uppercase = 'A', rating = 0 }
            -- ignored, the .uppercase already exists
        |> KeysDict.insert
            { lowercase = 'c', uppercase = 'C', rating = 0.6 }
            --put up

-}
insert :
    element
    -> KeysDict element
    -> KeysDict element
insert element =
    \keysDict ->
        if isUnique element keysDict then
            updateElements ((::) element) keysDict

        else
            keysDict


updateElements :
    (List element -> List element)
    -> KeysDict element
    -> KeysDict element
updateElements change =
    Typed.map
        (\keysDict ->
            { keysDict
                | elements =
                    change keysDict.elements
            }
        )
        >> isChecked KeysDict


{-| Change the element with the matching aspect based on its current value.

    KeysDict.promising [ unique .username, unique .email ]
        |> KeysDict.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.com" }
            ]
        |> KeysDict.update
            .username
            "fred"
            (\user -> { user | priority = p.priority + 3 })

If this aspect isn't unique, all elements with the matching aspect are updated.

    KeysDict.promising [ unique .email ]
        |> KeysDict.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "fred", priority = 3, email = "miggo@inlook.com" }
            ]
        |> KeysDict.update
            .username
            "fred"
            (\user -> { user | priority = p.priority + 3 })

Every fred gets a higher priority!

-}
update : (a -> b) -> b -> (a -> a) -> KeysDict a -> KeysDict a
update aspect match change =
    \keysDict ->
        keysDict
            |> updateAll
                (\el ->
                    if aspect el == match then
                        change el

                    else
                        el
                )


{-| Reduce the elements from most recently to least recently inserted element.

> With non-comparable types, thinking about order doesn't make much sense.

> You shouldn't rely on it when using functions like folds or `toList`.


    brackets =
        KeysDict.promising
            [ unique .open, unique .closed ]
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
    toList >> List.foldl reduce initial


{-| Remove the element where `unique` of the element matches the `key`.
If **the key does not exist**, the `KeysDict` is **unchanged**

    openClosedBrackets =
        KeysDict.promising
            [ unique .open, unique .closed ]
            |> KeysDict.insert { open = "(", closed = ")" }

    openClosedBrackets
        |> KeysDict.remove .open ")"
            -- unchanged, ")" does not exist as a .open key
        |> KeysDict.remove .open "("
    --> KeysDict.promising
    -->     [ unique .open, unique .closed ]

    openClosedBrackets
        |> KeysDict.remove .closed "("
            -- unchanged, "(" does not exist as a .closed key
        |> KeysDict.remove .closed ")"
    --> KeysDict.promising
    -->     [ unique .open, unique .closed ]

If there is no promise of `Uniqueness` for `unique`, `remove` acts as a filter.

    KeysDict.promising
        [ unique .open, unique .closed ]
        |> KeysDict.insert
            { open = "(", closed= ")", meaning = Nothing }
        |> KeysDict.insert
            { open = "[", closed = "]", meaning = Just List }
        |> KeysDict.insert
            { open = "<, closed = ">", meaning = Nothing }
        |> KeysDict.remove .meaning Nothing

    --> KeysDict.promising
    -->     [ unique .open, unique .closed ]
    -->     |> KeysDict.insert
    -->         { open = "[", closed = "]", meaning = Just List }

-}
remove :
    (element -> aspect)
    -> aspect
    -> KeysDict element
    -> KeysDict element
remove aspect key =
    updateElements
        (List.filter
            (\element -> key /= aspect element)
        )


{-| Only keep elements that satisfy a test.

    letters =
        KeysDict.promising
            [ unique .inAlphabet
            , unique .lowercase
            , unique .uppercase
            ]
            |> KeysDict.insertAll
                [ { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }
                , { inAlphabet = 1, lowercase = 'b', uppercase = 'B' }
                , ...
                ]

    aToD =
        letters
            |> KeysDict.when
                (\letter -> letter.inAlphabet <= 3)

-}
when : (element -> Bool) -> KeysDict element -> KeysDict element
when isGood =
    updateElements (List.filter isGood)


{-| Remove elements where a condition is met.

    letters =
        KeysDict.promising
            [ unique .inAlphabet
            , unique .lowercase
            , unique .uppercase
            ]
            |> KeysDict.insertAll
                [ { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }
                , { inAlphabet = 1, lowercase = 'b', uppercase = 'B' }
                , ...
                ]

    aToD =
        letters
            |> KeysDict.dropWhen
                (\letter -> letter.inAlphabet > 3)

See [`when`](KeysDict#when).

-}
dropWhen : (element -> Bool) -> KeysDict element -> KeysDict element
dropWhen isBad =
    when (not << isBad)


{-| The `List` containing all elements from most recently (= head) to least recently inserted element.

> The keys can be non-comparable. There is no obvious order.

> → You shouldn't rely on order when using functions like `fold` or `toList`.

    mostRecentlyInserted =
        List.head << KeysDict.toList

-}
toList : KeysDict element -> List element
toList =
    internalVal KeysDict >> .elements


uniqueness : KeysDict element_ -> List (Uniqueness element_)
uniqueness =
    internalVal KeysDict >> .uniqueness


{-| Alter every element based on its current value.

    digitNames =
        KeysDict.promising
            [ unique .number, unique .name ]
            |> KeysDict.insert { number = 0, name = "zero" }
            |> KeysDict.insert { number = 1, name = "one" }

    mathSymbolNames =
        digitNames
            |> KeysDict.map [ unique .symbol, unique .name ]
                (\{ number, name } ->
                    { symbol = String.fromInt number, name = name }
                )
            |> KeysDict.insert { symbol = "+", name = "plus" }

-}
updateAll :
    (element -> element)
    -> KeysDict element
    -> KeysDict element
updateAll alter =
    \keysDict ->
        keysDict
            |> fold (alter >> insert)
                (promising (uniqueness keysDict))


{-| A [Codec](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/Serialize) to serialize a `KeysDict`.

    serializeUserKeysDict =
        KeysDict.serialze serializeUser
            [ unique .number, unique .name ]

    type alias User =
        { username : String
        , userId : UserId
        , settings = Settings
        }

    serializeUser =
        Serialize.record User
            |> Serialize.field .username Decode.string
            |> Serialize.field ...
            |> Serialize.finishRecord

-}
serialize :
    Codec customError element
    -> List (Uniqueness element)
    -> Codec customError (KeysDict element)
serialize serializeElement uniqueness_ =
    Serialize.list serializeElement
        |> Serialize.map
            (List.foldl insert (promising uniqueness_))
            toList
