module KeysSet exposing
    ( KeysSet
    , promising
    , Uniqueness, unique
    , equal, isEmpty, at, size, isUnique, all, any
    , insert, insertAll, remove, update, updateAll
    , when
    , fold, toList, map
    )

{-|

@docs KeysSet


## create

@docs promising


### door

@docs Uniqueness, unique


## scan

@docs equal, isEmpty, at, size, isUnique, all, any


## alter

@docs insert, insertAll, remove, update, updateAll


### filter

@docs when


## transform

@docs fold, toList, map

-}

import List.Extra as List
import Typed exposing (Checked, Internal, Typed, internal, isChecked, tag)
import Util exposing (aspect, firstWhere)


{-| Unsorted data structure that lets you specify aspects that are checked to be unique across all elements

    countries : KeysSet { flag : String, code : String, name : String }
    countries =
        KeysSet.promising
            [ unique .flag, unique .code ]
            |> KeysSet.insertAll
                [ { flag = "🇦🇺", code = "AU", name = "Australia" }
                , { flag = "🇦🇶", code = "AQ", name = "Antarctica" }
                , { flag = "🇱🇧", code = "LB", name = "Lebanon" }
                ]

    -- aspect to check for matches + key → matching element

    KeysSet.at .flag "🇦🇶" countries
    --> Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

    KeysSet.at .code "LB" countries
    --> Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }

-}
type alias KeysSet element =
    Typed
        Checked
        KeysSetTag
        Internal
        (ElementsWithUniquenessPromises element)


type alias ElementsWithUniquenessPromises element =
    { uniqueness : List (Uniqueness element)
    , elements : List element
    }


{-| **Should not be exposed.**
-}
type KeysSetTag
    = KeysSet


{-| A check on whether 2 values are equal in some aspect.
See [`unique`](KeysSet#unique)

    uniqueInCasedLetter =
        [ unique .inAlphabet
        , unique .lowercase
        , unique .uppercase
        ]

    KeysSet.promising uniqueInCasedLetter
        |> KeysSet.insert
            { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }
        |> KeysSet.insert
            { inAlphabet = 0, lowercase = 'b', uppercase = 'B' }
        -- not inserted
        -- There's already an element where .inAlphabet is 0

-}
type alias Uniqueness value =
    value -> value -> { areUnique : Bool }


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
        { lastName = "jimmy", firstName = "petter", ... }
        { lastName = "jimmy", firstName = "greg", ... }
    --> { areUnique = True }

in `KeysSet`

    KeysSet.promising [ unique .email ]
        |> KeysSet.insert
            { username = "ben", email = "ben10@gmx.de" }
        |> KeysSet.insert
            { username = "mai", email = "ben10@gmx.de" }
        -- not inserted
        -- There's already an element where .email is "ben10@gmx.de"

-}
unique : (value -> aspectToCheckForStructuralEquality_) -> Uniqueness value
unique aspect =
    \a b -> { areUnique = (a |> aspect) /= (b |> aspect) }


{-| A `KeysSet` with no elements inside,
promising that given aspects are unique across all elements.
See [`Uniqueness`](KeysSet#Uniqueness)

    KeysSet.promising [ unique .email ]
        |> KeysSet.insert
            { username = "ben", email = "ben10@gmx.de" }
        |> KeysSet.insert
            { username = "mai", email = "ben10@gmx.de" }
        -- not inserted
        -- There's already an element where .email is "ben10@gmx.de"

Elements that are inserted must **not** contain **functions, json or regexes**.
Elm will crash trying to see if they are equal

-}
promising : List (Uniqueness element) -> KeysSet element
promising uniqueness_ =
    { uniqueness = uniqueness_, elements = [] }
        |> tag KeysSet


{-| How can you know if each element in `aKeysSet` can also be found in `bKeysSet`?

    letterCodes =
        KeysSet.insertAll
            [ { letter = 'a', code = 97 }
            , { letter = 'b', code = 98 }
            ]
            (KeysSet.promising
                [ unique .letter, unique .code ]
            )

    fancyCompetingLetterCodes =
        KeysSet.promising
            [ unique .code, unique .letter ]
            |> KeysSet.insert { code = 98, letter = 'b' }
            |> KeysSet.insert { code = 97, letter = 'a' }

    letterCodes == fancyCompetingLetterCodes
    -- elm crashes

Because a `KeysSet`'s `Uniqueness` is defined as functions

    (letterCodes |> KeysSet.toList)
    == (fancyCompetingLetterCodes |> KeysSet.toList)
    --> False

Even though both contain the same elements but in a different order

> → Don't use `==` to compare `KeysSet`s

> The keys can be non-comparable. There is no obvious order.
> → You shouldn't rely on order when using functions like `fold` or `toList`

Instead, use

    KeysSet.equal
        letterCodes
        fancyCompetingLetterCodes
    --> True

-}
equal :
    KeysSet element
    -> KeysSet element
    -> Bool
equal =
    aspect toList List.isPermutationOf


{-| Try to find an element where a given aspect matches a given value

    casedLetters =
        KeysSet.promising
            [ unique .lowercase, unique .uppercase ]
            |> KeysSet.insertAll
                [ { lowercase = 'a', uppercase = 'A' }
                , { lowercase = 'b', uppercase = 'B' }
                ]

    lowercase char =
        casedLetters
            |> KeysSet.at .uppercase char
            |> Maybe.map .lowercase

    uppercase char =
        casedLetters
            |> KeysSet.at .lowercase char
            |> Maybe.map .uppercase

If the given aspect isn't promised to be unique,
`at` will find the most recently inserted element where the given aspect matches the given value

    ratedOperators =
        KeysSet.promising
            [ unique .symbol, unique .name ]
            |> KeysSet.insertAll
                [ { rating = 0.5, symbol = "<", name = "lt" }
                , { rating = 0.5, symbol = ">", name = "gt" }
                ]

    KeysSet.at .rating 0.5 ratedOperators
    --> { rating = 0.5, symbol = ">", name = "gt" }
    -->     |> Just

-}
at :
    (element -> aspect)
    -> aspect
    ->
        (KeysSet element
         -> Maybe element
        )
at aspectAccess keyToFind =
    toList
        >> firstWhere
            (\element -> (element |> aspectAccess) == keyToFind)


{-| Conveniently [`insert`](KeysSet#insert) a `List` of elements

    KeysSet.promising
        [ unique .open, unique .closed ]
        |> KeysSet.insertAll
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]
    --> KeysSet.promising [ unique .open, unique .closed ]
    -->     |> KeysSet.insert { open = '(', closed = ')' }
    -->     |> KeysSet.insert { open = '{', closed = '}' }

-}
insertAll :
    List element
    -> KeysSet element
    -> KeysSet element
insertAll listOfElementsToInsert =
    \keysSet ->
        List.foldl insert
            keysSet
            listOfElementsToInsert


{-| How many elements there are

    KeysSet.promising [ unique identity ]
        |> KeysSet.insertAll (List.range 0 41)
        |> KeysSet.size
    --> 42

-}
size : KeysSet element_ -> Int
size =
    toList >> List.length


{-| Whether there are no elements inside

    KeysSet.promising [ unique .name ]
        |> KeysSet.isEmpty
    --> True

    KeysSet.promising [ unique .name ]
        |> KeysSet.insertAll []
        |> KeysSet.isEmpty
    --> True

    KeysSet.promising [ unique .name ]
        |> KeysSet.insert { name = "pete" }
        |> KeysSet.isEmpty
    --> False

-}
isEmpty : KeysSet element_ -> Bool
isEmpty =
    toList >> List.isEmpty


{-| Whether this element is considered unique / would it be [`insert`](#insert)ed

    letters =
        KeysSet.promising
            [ unique .lowercase, unique .uppercase ]
            |> KeysSet.insertAll
                [ { lowercase = 'a', uppercase = 'A' }
                , { lowercase = 'b', uppercase = 'B' }
                ]

    letters
        |> KeysSet.isUnique
            { lowercase = 'b', uppercase = 'C' }
    --> False
    -- .lowercase 'b' already exists

    letters
        |> KeysSet.isUnique
            { lowercase = 'c', uppercase = 'A' }
    --> False
    -- .uppercase 'A' already exists

    letters
        |> KeysSet.isUnique
            { lowercase = 'c', uppercase = 'C' }
    --> True

-}
isUnique : element -> KeysSet element -> Bool
isUnique element =
    \keysSet ->
        uniqueness keysSet
            |> List.any
                (\unique_ ->
                    keysSet
                        |> any
                            (\bElement ->
                                not (unique_ element bElement).areUnique
                            )
                )
            |> not


{-| Whether there are least some elements that pass a given test.

    KeysSet.promising
        [ unique .username, unique .email ]
        |> KeysSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.go" }
            ]
        |> KeysSet.any (\user -> user.priority > 4)
    --> False

    member needle =
        KeysSet.any ((==) needle)

-}
any : (element -> Bool) -> KeysSet element -> Bool
any isOkay =
    toList >> List.any isOkay


{-| Whether all elements pass a given test.

    KeysSet.promising
        [ unique .username, unique .email ]
        |> KeysSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.go" }
            ]
        |> KeysSet.all (\user -> user.priority < 4)
    --> True

-}
all : (element -> Bool) -> KeysSet element -> Bool
all isOkay =
    toList >> List.all isOkay


{-| Put an element into the `KeysSet`.

If there is already an element where some aspect that is promised to be unique is equal
(see [`Uniqueness`](KeysSet#Uniqueness)),
the [`KeysSet`](#KeysSet) remains **unchanged**

    KeysSet.promising
        [ unique .lowercase, unique .uppercase ]
        |> KeysSet.insert
            { lowercase = 'b', uppercase = 'B', rating = 0.5 }
            -- is inserted
        |> KeysSet.insert
            { lowercase = 'a', uppercase = 'A', rating = 0.5 }
            -- is inserted
            -- .rating is not specified as unique
        |> KeysSet.insert
            { lowercase = 'b', uppercase = 'C', rating = 0 }
            -- is ignored
            -- .lowercase 'b' already exists
        |> KeysSet.insert
            { lowercase = 'c', uppercase = 'A', rating = 0 }
            -- is ignored
            -- .uppercase 'A' already exists
        |> KeysSet.insert
            { lowercase = 'c', uppercase = 'C', rating = 0.6 }
            -- is inserted

-}
insert :
    element
    -> KeysSet element
    -> KeysSet element
insert element =
    \keysSet ->
        if isUnique element keysSet then
            updateElements ((::) element) keysSet

        else
            keysSet


updateElements :
    (List element -> List element)
    -> KeysSet element
    -> KeysSet element
updateElements change =
    Typed.map
        (\keysSet ->
            { keysSet
                | elements =
                    change keysSet.elements
            }
        )
        >> isChecked KeysSet


{-| Change the element with the matching aspect based on its current value

    KeysSet.promising
        [ unique .username, unique .email ]
        |> KeysSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.go" }
            ]
        |> KeysSet.update .username
            "fred"
            (\user -> { user | priority = p.priority + 3 })

If this aspect isn't unique, all elements with the matching aspect are updated

    KeysSet.promising [ unique .email ]
        |> KeysSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "fred", priority = 3, email = "miggo@inlook.go" }
            ]
        |> KeysSet.update .username
            "fred"
            (\user -> { user | priority = p.priority + 3 })

Every fred gets a higher priority!

-}
update : (a -> b) -> b -> (a -> a) -> KeysSet a -> KeysSet a
update aspect match change =
    \keysSet ->
        keysSet
            |> updateAll
                (\el ->
                    if aspect el == match then
                        change el

                    else
                        el
                )


{-| Reduce the elements from most recently to least recently inserted element

> The keys can be non-comparable. There is no obvious order.

> → You shouldn't rely on order when using functions like `fold` or `toList`


    brackets =
        KeysSet.promising
            [ unique .open, unique .closed ]
            |> KeysSet.insertAll
                [ { open = '(', closed = ')' }
                , { open = '{', closed = '}' }
                ]

    openingAndClosing =
        brackets
            |> KeysSet.fold
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
    -> KeysSet element
    -> acc
fold reduce initial =
    toList >> List.foldl reduce initial


{-| Remove the element where a given aspect of the element matches a given value

    openClosedBrackets =
        KeysSet.promising
            [ unique .open, unique .closed ]
            |> KeysSet.insert
                { open = "(", closed = ")" }

    openClosedBrackets
        |> KeysSet.remove .open ")"
    -- no change
    -- .open is never ")"

    openClosedBrackets
        |> KeysSet.remove .closed ")"
    -- removes { open = "(", closed = ")" }

If there the checked aspect isn't promised to be unique, `remove` acts as a filter

    KeysSet.promising
        [ unique .open, unique .closed ]
        |> KeysSet.insertAll
            [ { open = "[", closed = "]", meaning = List }
            , { open = "<", closed = ">", meaning = Custom }
            , { open = "\\", closed = "/", meaning = Custom }
            ]
        |> KeysSet.remove .meaning Custom
    -- only { open = "[", closed = "]", meaning = List } remains

If filtering is your intention, use [`KeysSet.when`](KeysSet#when)

-}
remove :
    (element -> aspect)
    -> aspect
    -> KeysSet element
    -> KeysSet element
remove aspect key =
    updateElements
        (List.filter
            (\element -> key /= aspect element)
        )


{-| Only keep elements that pass a given test

    operators : KeysSet { symbol : String, name : String }
    operators =
        KeysSet.promising
            [ unique .symbol, unique .name ]
            |> KeysSet.insertAll
                [ { symbol = ">", name = "gt" }
                , { symbol = "<", name = "lt" }
                , { symbol = "==", name = "eq" }
                ]

    -- singleCharOperators
    operators
        |> KeysSet.when
            (\operator ->
                (operator.symbol |> String.length) == 1
            )
        |> KeysSet.toList
    --> [ { symbol = ">", name = "gt" }
    --> , { symbol = "<", name = "lt" }
    --> ]

-}
when : (element -> Bool) -> KeysSet element -> KeysSet element
when isGood =
    updateElements (List.filter isGood)


{-| The `List` containing all elements from most recently (= head) to least recently inserted element

> The keys can be non-comparable. There is no obvious order.

> → You shouldn't rely on order when using functions like `fold` or `toList`

    mostRecentlyInserted =
        List.head << KeysSet.toList

    KeysSet.promising
        [ unique .open, unique .closed ]
        |> KeysSet.insertAll
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]
        |> KeysSet.toList
    --> [ { open = '{', closed = '}' }
    --> , { open = '(', closed = ')' }
    --> ]

-}
toList : KeysSet element -> List element
toList =
    internal KeysSet >> .elements


uniqueness : KeysSet element -> List (Uniqueness element)
uniqueness =
    internal KeysSet >> .uniqueness


{-| Alter every element based on its current value

    digitNames =
        KeysSet.promising
            [ unique .number, unique .name ]
            |> KeysSet.insertAll
                [ { number = 0, name = "zero" }
                , { number = 1, name = "one" }
                ]

    mathSymbolNames =
        digitNames
            |> KeysSet.map
                (\{ number, name } ->
                    { symbol = String.fromInt number, name = name }
                )
                [ unique .symbol, unique .name ]
            |> KeysSet.insert { symbol = "+", name = "plus" }

If the type doesn't change, use [`KeysSet.updateAll`](KeysSet#updateAll)

-}
map :
    (element -> mappedElement)
    -> List (Uniqueness mappedElement)
    -> KeysSet element
    -> KeysSet mappedElement
map alter uniquenessOfMappedElement =
    \keysSet ->
        keysSet
            |> fold (alter >> insert)
                (promising uniquenessOfMappedElement)


{-| Change every element based on its current value

    rankUpAllUsers =
        KeysSet.updateAll
            (\user -> { user | rank = user.rank + 1 })

If aspects that are promised to be unique become the same for 2 elements,
the more recently inserted element is chosen

Use [`map`](KeysSet#map) if your function changes the type of the element

-}
updateAll :
    (element -> element)
    -> KeysSet element
    -> KeysSet element
updateAll change =
    \keysSet ->
        keysSet |> map change (uniqueness keysSet)
