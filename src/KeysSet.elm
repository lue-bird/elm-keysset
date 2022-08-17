module KeysSet exposing
    ( KeysSet
    , promising
    , Uniqueness, unique
    , isEqualTo, isEmpty, element, size, isUniqueIn, all, any
    , uniqueness
    , insert, insertList, elementRemove, elementAlter, alter
    , foldFrom, toList, map, mapTry
    )

{-|

@docs KeysSet


## create

@docs promising


### unique or not?

@docs Uniqueness, unique


## scan

@docs isEqualTo, isEmpty, element, size, isUniqueIn, all, any
@docs uniqueness


## alter

@docs insert, insertList, elementRemove, elementAlter, alter


## transform

@docs foldFrom, toList, map, mapTry

-}

import List.Extra as List
import Typed exposing (Checked, Internal, Typed, internal, isChecked, tag)


{-| Unsorted data structure that lets you specify aspects that are checked to be unique across all elements

    countries : KeysSet { flag : String, code : String, name : String }
    countries =
        KeysSet.promising
            [ unique .flag, unique .code ]
            |> KeysSet.insertList
                [ { flag = "🇦🇺", code = "AU", name = "Australia" }
                , { flag = "🇦🇶", code = "AQ", name = "Antarctica" }
                , { flag = "🇱🇧", code = "LB", name = "Lebanon" }
                ]

    -- aspect to check for matches + key → matching element

    KeysSet.element ( .flag, "🇦🇶" ) countries
    --> Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

    KeysSet.element ( .code, "LB" ) countries
    --> Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }

-}
type KeysSet element
    = KeysSet
        { uniqueness : List (Uniqueness element)
        , elements : List element
        }


{-| A check on whether 2 values are equal in some aspect.
See [`unique`](#unique)

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
    value -> (value -> { areUnique : Bool })


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
        { lastName = "jimmy", firstName = "petter" }
        { lastName = "jimmy", firstName = "greg" }
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
See [`Uniqueness`](#Uniqueness)

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
        |> KeysSet


{-| How can you know if each element in `aKeysSet` can also be found in `bKeysSet`?

    letterCodes : KeysSet { letter : Char, code : Int }
    letterCodes =
        KeysSet.insertList
            [ { letter = 'a', code = 97 }
            , { letter = 'b', code = 98 }
            ]
            (KeysSet.promising
                [ unique .letter, unique .code ]
            )

    fancyCompetingLetterCodes : KeysSet { letter : Char, code : Int }
    fancyCompetingLetterCodes =
        KeysSet.promising
            [ unique .code, unique .letter ]
            |> KeysSet.insert { code = 98, letter = 'b' }
            |> KeysSet.insert { code = 97, letter = 'a' }

    letterCodes == fancyCompetingLetterCodes
    -- elm crashes
    -- because a `KeysSet`'s `Uniqueness` is defined as functions

    (letterCodes |> KeysSet.toList)
        == (fancyCompetingLetterCodes |> KeysSet.toList)
    -- both contain the same elements
    -- but in a different order
    --> False

    letterCodes
        |> KeysSet.isEqualTo fancyCompetingLetterCodes
    --> True

> → Don't use `==` to compare `KeysSet`s

> The keys can be non-comparable. There is no obvious order.
> → You shouldn't rely on order when using functions like `fold` or `toList`

runtime: `length^2`

-}
isEqualTo :
    KeysSet element
    ->
        (KeysSet element
         -> Bool
        )
isEqualTo keysSetToCheckAgainst =
    \keysSet ->
        (keysSet |> toList)
            |> List.isPermutationOf
                (keysSetToCheckAgainst |> toList)


{-| Try to find an element where a given aspect matches a given value

    casedLetters =
        KeysSet.promising
            [ unique .lowercase, unique .uppercase ]
            |> KeysSet.insertList
                [ { lowercase = 'a', uppercase = 'A' }
                , { lowercase = 'b', uppercase = 'B' }
                ]

    lowercase char =
        casedLetters
            |> KeysSet.element .uppercase char
            |> Maybe.map .lowercase

    uppercase char =
        casedLetters
            |> KeysSet.element .lowercase char
            |> Maybe.map .uppercase

If the given aspect isn't promised to be unique,
`at` will find the most recently inserted element where the given aspect matches the given value

    ratedOperators : KeysSet { rating : Float, symbol : String, name : String }
    ratedOperators =
        KeysSet.promising
            [ unique .symbol, unique .name ]
            |> KeysSet.insertList
                [ { rating = 0.5, symbol = "<", name = "lt" }
                , { rating = 0.5, symbol = ">", name = "gt" }
                ]

    ratedOperators
        |> KeysSet.element ( .rating, 0.5 )
    --> { rating = 0.5, symbol = ">", name = "gt" }
    -->     |> Just

runtime: `length`

-}
element :
    ( element -> aspect, aspect )
    ->
        (KeysSet element
         -> Maybe element
        )
element ( aspectAccess, keyToFind ) =
    toList
        >> List.find
            (\el -> (el |> aspectAccess) == keyToFind)


{-| Conveniently [`insert`](#insert) a `List` of elements

    KeysSet.promising
        [ unique .open, unique .closed ]
        |> KeysSet.insertList
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]
        |> KeysSet.isEqualTo
            (KeysSet.promising [ unique .open, unique .closed ]
                |> KeysSet.insert { open = '(', closed = ')' }
                |> KeysSet.insert { open = '{', closed = '}' }
            )
    --> True

runtime: `setLength * listLength`

-}
insertList :
    List element
    ->
        (KeysSet element
         -> KeysSet element
        )
insertList listOfElementsToInsert =
    \keysSet ->
        List.foldl insert
            keysSet
            listOfElementsToInsert


{-| How many elements there are

    KeysSet.promising [ unique identity ]
        |> KeysSet.insertList (List.range 0 41)
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
        |> KeysSet.insertList []
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


{-| Whether this element is considered unique / would be [`insert`](#insert)ed

    letters : KeysSet { lowercase : Char, uppercase : Char }
    letters =
        KeysSet.promising
            [ unique .lowercase, unique .uppercase ]
            |> KeysSet.insertList
                [ { lowercase = 'a', uppercase = 'A' }
                , { lowercase = 'b', uppercase = 'B' }
                ]

    KeysSet.isUniqueIn letters
        { lowercase = 'b', uppercase = 'C' }
    -- .lowercase 'b' already exists
    --> False

    KeysSet.isUniqueIn letters
        { lowercase = 'c', uppercase = 'A' }
    -- .uppercase 'A' already exists
    --> False

    KeysSet.isUniqueIn letters
        { lowercase = 'c', uppercase = 'C' }
    --> True

runtime: `length`

-}
isUniqueIn : KeysSet element -> (element -> Bool)
isUniqueIn keysSet =
    \elementToCheckAgainst ->
        keysSet
            |> all
                (\el ->
                    (keysSet |> uniqueness)
                        |> List.all
                            (\unique_ ->
                                (unique_ elementToCheckAgainst el).areUnique
                            )
                )


{-| Whether there are least some elements that pass a given test.

    KeysSet.promising
        [ unique .username, unique .email ]
        |> KeysSet.insertList
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
        |> KeysSet.insertList
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
(see [`Uniqueness`](#Uniqueness)),
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
        |> KeysSet.isEqualTo
            (KeysSet.promising
                [ unique .lowercase, unique .uppercase ]
                |> KeysSet.insertList
                    [ { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                    , { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                    , { lowercase = 'c', uppercase = 'C', rating = 0.6 }
                    ]
            )
    --> True

runtime: `length`

-}
insert :
    element
    ->
        (KeysSet element
         -> KeysSet element
        )
insert elementToInsertIfUnique =
    \keysSet ->
        if elementToInsertIfUnique |> isUniqueIn keysSet then
            keysSet |> internalElementListAlter ((::) elementToInsertIfUnique)

        else
            keysSet


internalElementListAlter :
    (List element -> List element)
    ->
        (KeysSet element
         -> KeysSet element
        )
internalElementListAlter elementsInternalChange =
    \(KeysSet keysSetInternal) ->
        { keysSetInternal
            | elements =
                keysSetInternal.elements |> elementsInternalChange
        }
            |> KeysSet


{-| Change the element with the matching aspect based on its current value

    KeysSet.promising
        [ unique .username, unique .email ]
        |> KeysSet.insertList
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.go" }
            ]
        |> KeysSet.elementAlter ( .username, "fred" )
            (\user -> { user | priority = user.priority + 3 })
        |> KeysSet.isEqualTo
            (KeysSet.promising [ unique .email ]
                |> KeysSet.insertList
                    [ { username = "fred", priority = 4, email = "higgi@outlook.com" }
                    , { username = "gria", priority = 3, email = "miggo@inlook.go" }
                    ]
            )
    --> True

If this aspect isn't unique, all elements with the matching aspect are altered

    KeysSet.promising [ unique .email ]
        |> KeysSet.insertList
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "fred", priority = 3, email = "miggo@inlook.go" }
            ]
        |> KeysSet.elementAlter ( .username, "fred" )
            (\user -> { user | priority = user.priority + 3 })
        |> KeysSet.isEqualTo
            (KeysSet.promising [ unique .email ]
                |> KeysSet.insertList
                    [ { username = "fred", priority = 4, email = "higgi@outlook.com" }
                    , { username = "fred", priority = 6, email = "miggo@inlook.go" }
                    ]
            )
    --> True

To change all elements → [`map`](#map), [`mapTry`](#mapTry) or [`alter`](#alter) if the result type is the same as before

runtime: `length`

-}
elementAlter :
    ( element -> aspect
    , aspect
    )
    -> (element -> element)
    ->
        (KeysSet element
         -> KeysSet element
        )
elementAlter ( elementAspectAccess, elementAspectToMatch ) elementChange =
    \keysSet ->
        keysSet
            |> internalElementListAlter
                (let
                    withoutElementToAlter =
                        keysSet |> elementRemove ( elementAspectAccess, elementAspectToMatch )
                 in
                 List.filterMap
                    (\el ->
                        if (el |> elementAspectAccess) /= elementAspectToMatch then
                            el |> Just

                        else
                            let
                                altered =
                                    el |> elementChange
                            in
                            if altered |> isUniqueIn withoutElementToAlter then
                                altered |> Just

                            else
                                Nothing
                    )
                )


{-| Reduce the elements from most recently to least recently inserted element

> The keys can be non-comparable. There is no obvious order.

> → You shouldn't rely on order when using functions like `fold` or `toList`

    brackets : KeysSet { open : Char, closed : Char }
    brackets =
        KeysSet.promising
            [ unique .open, unique .closed ]
            |> KeysSet.insertList
                [ { open = '(', closed = ')' }
                , { open = '{', closed = '}' }
                ]

    brackets
        |> KeysSet.foldFrom []
            (\{ open, closed } ->
                (::) (String.fromList [ open, closed ])
            )
    --> []
    -->     |> (::) (String.fromList [ '{', '}' ])
    -->     |> (::) (String.fromList [ '(', ')' ])

-}
foldFrom :
    accumulationValue
    -> (element -> (accumulationValue -> accumulationValue))
    ->
        (KeysSet element
         -> accumulationValue
        )
foldFrom initial reduce =
    toList >> List.foldl reduce initial


{-| Remove the element where a given aspect of the element matches a given value

    openClosedBrackets : KeysSet { open : String, closed : String }
    openClosedBrackets =
        KeysSet.promising
            [ unique .open, unique .closed ]
            |> KeysSet.insert
                { open = "(", closed = ")" }

    openClosedBrackets
        |> KeysSet.elementRemove ( .open, ")" )
        |> KeysSet.isEmpty
    --> False
    -- .open is never ")"

    openClosedBrackets
        |> KeysSet.elementRemove ( .closed, ")" )
        |> KeysSet.isEmpty
    --> True

If the checked aspect isn't promised to be unique, [`elementRemove`](#elementRemove) acts like a filter

    KeysSet.promising
        [ unique .open, unique .closed ]
        |> KeysSet.insertList
            [ { open = "[", closed = "]", description = "List" }
            , { open = "<", closed = ">", description = "Custom" }
            , { open = "\\", closed = "/", description = "Custom" }
            ]
        |> KeysSet.elementRemove ( .description, "Custom" )
        |> KeysSet.toList
    --> [ { open = "[", closed = "]", description = "List" } ]

[`KeysSet.mapTry`](#mapTry) to filter values

runtime: `length`

-}
elementRemove :
    ( element -> aspect, aspect )
    ->
        (KeysSet element
         -> KeysSet element
        )
elementRemove ( elementAspectAccess, elementAspectToMatchAgainst ) =
    internalElementListAlter
        (List.filter
            (\el ->
                (el |> elementAspectAccess)
                    /= elementAspectToMatchAgainst
            )
        )


{-| The `List` containing all elements from most recently (= head) to least recently inserted element

> The keys can be non-comparable. There is no obvious order.

> → You shouldn't rely on order when using functions like `fold` or `toList`

    mostRecentlyInserted =
        List.head << KeysSet.toList

    KeysSet.promising
        [ unique .open, unique .closed ]
        |> KeysSet.insertList
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]
        |> KeysSet.toList
    --> [ { open = '{', closed = '}' }
    --> , { open = '(', closed = ')' }
    --> ]

runtime: `1`

-}
toList : KeysSet element -> List element
toList =
    \(KeysSet internal) -> internal.elements


{-| What [`Uniqueness`](#Uniqueness) the [`KeysSet`](#KeysSet) is [`promising`](#promising)
to check for.

    KeysSet.promising [ unique .open, unique .closed ]
        |> KeysSet.uniqueness
    --→ [ unique .open, unique .closed ]

This is rarely useful! Some examples:

    alter change =
        \keysSet ->
            keysSet |> KeysSet.map change (keysSet |> uniqueness)

    where isOk =
        \keysSet ->
            keysSet
                |> KeysSet.mapTry
                    (\element ->
                        if element |> isOk then
                            element |> Just

                        else
                            Nothing
                    )
                    (keysSet |> uniqueness)

-}
uniqueness : KeysSet element -> List (Uniqueness element)
uniqueness =
    \(KeysSet internal) -> internal.uniqueness


{-| Alter every element based on its current value

    digitNames =
        KeysSet.promising
            [ unique .number, unique .name ]
            |> KeysSet.insertList
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

Your function doesn't change the type of the element? → [`KeysSet.alter`](#alter)

runtime: `length^2`

-}
map :
    (element -> mappedElement)
    -> List (Uniqueness mappedElement)
    ->
        (KeysSet element
         -> KeysSet mappedElement
        )
map elementChange uniquenessOfMappedElement =
    \keysSet ->
        keysSet
            |> foldFrom
                (promising uniquenessOfMappedElement)
                (elementChange >> insert)


{-| Only keep elements that are transformed to `Just` by a given function

    operators : KeysSet { symbol : String, name : String }
    operators =
        KeysSet.promising
            [ unique .symbol, unique .name ]
            |> KeysSet.insertList
                [ { symbol = ">", name = "gt" }
                , { symbol = "<", name = "lt" }
                , { symbol = "==", name = "eq" }
                ]

    -- singleCharOperators
    operators
        |> KeysSet.mapTry
            (\operator ->
                case operator.symbol |> String.length of
                    1 ->
                        operator.name |> Just
                    _ ->
                        Nothing
            )
            [ unique identity ]
        |> KeysSet.toList
    --> [ "gt", "lt" ]

runtime: `length^2`

-}
mapTry :
    (element -> Maybe mappedElementFill)
    -> List (Uniqueness mappedElementFill)
    ->
        (KeysSet element
         -> KeysSet mappedElementFill
        )
mapTry elementChangeTry uniquenessOfMappedElement =
    \keysSet ->
        keysSet
            |> foldFrom
                (promising uniquenessOfMappedElement)
                (\el ->
                    case el |> elementChangeTry of
                        Just fill ->
                            insert fill

                        Nothing ->
                            identity
                )


{-| Change every element based on its current value

    rankUpAllUsers =
        KeysSet.updateAll
            (\user -> { user | rank = user.rank + 1 })

If aspects that are promised to be unique become the same for 2 elements,
the more recently inserted element is chosen

Your function changes the type of the element? → [`KeysSet.map`](#map)

runtime: `length^2`

-}
alter :
    (element -> element)
    ->
        (KeysSet element
         -> KeysSet element
        )
alter change =
    \keysSet ->
        keysSet |> map change (keysSet |> uniqueness)
