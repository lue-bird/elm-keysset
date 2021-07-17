module MultiSet exposing
    ( MultiSet
    , promising
    , Uniqueness, unique
    , equal, isEmpty, at, size, isUnique, all, any
    , insert, insertAll, remove, update, updateAll
    , when
    , fold, toList, map, serialize
    )

{-|

@docs MultiSet


## create

@docs promising


### uniqueness

@docs Uniqueness, unique


## scan

@docs equal, isEmpty, at, size, isUnique, all, any


## modify

@docs insert, insertAll, remove, update, updateAll


### filter

@docs when


## transform

@docs fold, toList, map, serialize

-}

import Serialize exposing (Codec)
import Typed exposing (Checked, Internal, Typed, internalVal, isChecked, tag)
import Util exposing (aspect, equalIgnoringOrder, firstWhere)


{-| Unsorted data structure that lets you specify aspects that are checked to be unique across all elements.

    countries =
        MultiSet.promising
            [ unique .flag, unique .code ]
            |> MultiSet.insertAll
                [ { flag = "🇦🇺", code = "AU", name = "Australia" }
                , { flag = "🇦🇶", code = "AQ", name = "Antarctica" }
                , { flag = "🇱🇧", code = "LB", name = "Lebanon" }
                ]

If you have a key and the aspect to check if it matches, you can find the matching element:

    MultiSet.at .flag "🇦🇶" countries
    --> Just { flag = "🇦🇶", code = "AQ", name = "Antarctica" }

    MultiSet.at .code "LB" countries
    --> Just { flag = "🇱🇧", code = "LB", name = "Lebanon" }

-}
type alias MultiSet element =
    Typed Checked MultiSetTag Internal (ElementsWithUniquenessPromises element)


type alias ElementsWithUniquenessPromises element =
    { uniqueness : List (Uniqueness element), elements : List element }


{-| **Should not be exposed.**
-}
type MultiSetTag
    = MultiSet


{-| Check 2 elements if they are equal in a specified aspect. See [unique](MultiSet#unique)

    uniqueInCasedLetter =
        [ unique .inAlphabet
        , unique .lowercase
        , unique .uppercase
        ]

    MultiSet.promising uniqueInCasedLetter
        |> MultiSet.insert
            { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }
        |> MultiSet.insert
            { inAlphabet = 0, lowercase = 'b', uppercase = 'B' }
        --> isn't inserted. There's already an element where .inAlphabet is 0.

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

    unique (\person -> ( person.firstName, person.lastName ))
        { lastName = "jimmy", firstName = "petter", ... }
        { lastName = "jimmy", firstName = "greg", ... }
    --> { areUnique = True }

in `MultiSet`

    MultiSet.promising [ unique .email ]
        |> MultiSet.insert
            { username = "ben", email = "ben10@gmx.de" }
        |> MultiSet.insert
            { username = "mai", email = "ben10@gmx.de" }
        --> is not inserted.
        --> There's already an element where .email is "ben10@gmx.de"

-}
unique : (element -> aspect_) -> Uniqueness element
unique aspect =
    \a b -> { areUnique = aspect a /= aspect b }


{-| A `MultiSet` with no elements inside,
promising that given aspects are unique across all elements.
See [`Uniqueness`](MultiSet#Uniqueness)

    MultiSet.promising [ unique .email ]
        |> MultiSet.insert
            { username = "ben", email = "ben10@gmx.de" }
        |> MultiSet.insert
            { username = "mai", email = "ben10@gmx.de" }
        --> is not inserted.
        --> There's already an element where .email is "ben10@gmx.de"

Elements that are inserted must **not** contain **functions, json or regexes**.
Elm will crash trying to see if they are equal.

-}
promising : List (Uniqueness element) -> MultiSet element
promising uniqueness_ =
    { uniqueness = uniqueness_, elements = [] }
        |> tag
        |> isChecked MultiSet


{-| How can you know if each element in `aMultiSet` can also be found in `bMultiSet`?

    letterCodes =
        MultiSet.insertAll
            [ { letter = 'a', code = 97 }
            , { letter = 'b', code = 98 }
            ]
            (MultiSet.promising
                [ unique .letter, unique .code ]
            )

    fancyCompetingLetterCodes =
        MultiSet.promising
            [ unique .code, unique .letter ]
            |> MultiSet.insert { code = 98, letter = 'b' }
            |> MultiSet.insert { code = 97, letter = 'a' }

    letterCodes == fancyCompetingLetterCodes
    --> elm crashes

Because a `MultiSet`'s `Uniqueness` is defined as functions.

    MultiSet.toList letterCodes
    == MultiSet.toList fancyCompetingLetterCodes
    --> False

Even though both contain the same elements but in a different order.


### take away

> Don't use `==` to compare `MultiSet`s

> The keys can be non-comparable. There is no obvious order.
> → You shouldn't rely on order when using functions like `fold` or `toList`.

Instead, use

    MultiSet.equal
        letterCodes
        fancyCompetingLetterCodes
    --> True

-}
equal :
    MultiSet element
    -> MultiSet element
    -> Bool
equal =
    aspect toList equalIgnoringOrder


{-| Try to find an element where a given aspect of it matches a given value.

    casedLetters =
        MultiSet.promising
            [ unique .lowercase, unique .uppercase ]
            |> MultiSet.insertAll
                [ { lowercase = 'a', uppercase = 'A' }
                , { lowercase = 'b', uppercase = 'B' }
                ]

    lowercase char =
        casedLetters
            |> MultiSet.at .uppercase char
            |> Maybe.map .lowercase

    uppercase char =
        casedLetters
            |> MultiSet.at .lowercase char
            |> Maybe.map .uppercase

If the given aspect isn't promised to be unique,
`at` will find the most recently inserted element where the given aspect matches the given value.

    ratedOperators =
        MultiSet.promising
            [ unique .symbol, unique .name ]
            |> MultiSet.insertAll
                [ { rating = 0.5, symbol = "<", name = "lt" }
                , { rating = 0.5, symbol = ">", name = "gt" }
                ]

    MultiSet.at .rating 0.5 ratedOperators
    --> { rating = 0.5, symbol = ">", name = "gt" }
    -->     |> Just

-}
at :
    (element -> aspect)
    -> aspect
    -> MultiSet element
    -> Maybe element
at accessAspect keyToFind =
    toList >> firstWhere (accessAspect >> (==) keyToFind)


{-| Conveniently insert a list of elements. See [insert](MultiSet#insert).

    MultiSet.promising
        [ unique .open, unique .closed ]
        |> MultiSet.insertAll
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]
    --> MultiSet.promising [ unique .open, unique .closed ]
    -->     |> MultiSet.insert { open = '(', closed = ')' }
    -->     |> MultiSet.insert { open = '{', closed = '}' }

-}
insertAll :
    List element
    -> MultiSet element
    -> MultiSet element
insertAll listOfElementsToInsert =
    \multiSet ->
        List.foldl insert
            multiSet
            listOfElementsToInsert


{-| How many elements there are.

    MultiSet.promising
        [ unique .number, unique .following ]
        |> MultiSet.insertAll
            (List.map
                (\i -> { number = i, following = i + 1 })
                (List.range 0 41)
            )
        |> MultiSet.size
    --> 42

-}
size : MultiSet element_ -> Int
size =
    toList >> List.length


{-| Whether there are no elements inside.

    MultiSet.promising [ unique .name ]
        |> MultiSet.isEmpty
    --> True

    MultiSet.promising [ unique .name ]
        |> MultiSet.insertAll []
        |> MultiSet.isEmpty
    --> True

    MultiSet.promising [ unique .name ]
        |> MultiSet.insert { name = "pete" }
        |> MultiSet.isEmpty
    --> False

-}
isEmpty : MultiSet element_ -> Bool
isEmpty =
    toList >> List.isEmpty


{-| Whether this element is considered unique / would it be inserted.

    letters =
        MultiSet.promising
            [ unique .lowercase, unique .uppercase ]
            |> MultiSet.insertAll
                [ { lowercase = 'a', uppercase = 'A' }
                , { lowercase = 'b', uppercase = 'B' }
                ]

    letters
        |> MultiSet.isUnique
            { lowercase = 'b', uppercase = 'C' }
        -- the .lowercase already exists
    --> False

    letters
        |> MultiSet.isUnique
            { lowercase = 'c', uppercase = 'A' }
        -- the .uppercase already exists
    --> False

    letters
        |> MultiSet.isUnique
            { lowercase = 'c', uppercase = 'C' }
    --> True

-}
isUnique : element -> MultiSet element -> Bool
isUnique element =
    \multiSet ->
        uniqueness multiSet
            |> List.any
                (\unique_ ->
                    multiSet
                        |> any
                            (\bElement ->
                                not (unique_ element bElement).areUnique
                            )
                )
            |> not


{-| Whether there is least one element that passes a test.

    MultiSet.promising
        [ unique .username, unique .email ]
        |> MultiSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.com" }
            ]
        |> MultiSet.any (\user -> user.priority > 4)
    --> False

    member needle =
        MultiSet.any ((==) needle)

-}
any : (element -> Bool) -> MultiSet element -> Bool
any isOkay =
    toList >> List.any isOkay


{-| Whether all elements pass a test.

    MultiSet.promising
        [ unique .username, unique .email ]
        |> MultiSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.com" }
            ]
        |> MultiSet.all (\user -> user.priority < 4)
    --> True

-}
all : (element -> Bool) -> MultiSet element -> Bool
all isOkay =
    toList >> List.all isOkay


{-| Put an element into `MultiSet`.

If there is already an element with the same **key** is already **present**, (see `Uniqueness`), the `MultiSet` remains **unchanged**.

    MultiSet.promising
        [ unique .lowercase, unique .uppercase ]
        |> MultiSet.insert
            { lowercase = 'b', uppercase = 'B', rating = 0.5 }
            --> is inserted
        |> MultiSet.insert
            { lowercase = 'a', uppercase = 'A', rating = 0.5 }
            --> is inserted. .rating is not specified as unique
        |> MultiSet.insert
            { lowercase = 'b', uppercase = 'C', rating = 0 }
            --> is ignored. .lowercase 'b' already exists
        |> MultiSet.insert
            { lowercase = 'c', uppercase = 'A', rating = 0 }
            --> is ignored, .uppercase 'A' already exists
        |> MultiSet.insert
            { lowercase = 'c', uppercase = 'C', rating = 0.6 }
            --> is inserted

-}
insert :
    element
    -> MultiSet element
    -> MultiSet element
insert element =
    \multiSet ->
        if isUnique element multiSet then
            updateElements ((::) element) multiSet

        else
            multiSet


updateElements :
    (List element -> List element)
    -> MultiSet element
    -> MultiSet element
updateElements change =
    Typed.map
        (\multiSet ->
            { multiSet
                | elements =
                    change multiSet.elements
            }
        )
        >> isChecked MultiSet


{-| Change the element with the matching aspect based on its current value.

    MultiSet.promising
        [ unique .username, unique .email ]
        |> MultiSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "gria", priority = 3, email = "miggo@inlook.com" }
            ]
        |> MultiSet.update
            .username
            "fred"
            (\user -> { user | priority = p.priority + 3 })

If this aspect isn't unique, all elements with the matching aspect are updated.

    MultiSet.promising [ unique .email ]
        |> MultiSet.insertAll
            [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
            , { username = "fred", priority = 3, email = "miggo@inlook.com" }
            ]
        |> MultiSet.update
            .username
            "fred"
            (\user -> { user | priority = p.priority + 3 })

Every fred gets a higher priority!

-}
update : (a -> b) -> b -> (a -> a) -> MultiSet a -> MultiSet a
update aspect match change =
    \multiSet ->
        multiSet
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
        MultiSet.promising
            [ unique .open, unique .closed ]
            |> MultiSet.insertAll
                [ { open = '(', closed = ')' }
                , { open = '{', closed = '}' }
                ]

    openingAndClosing =
        brackets
            |> MultiSet.fold
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
    -> MultiSet element
    -> acc
fold reduce initial =
    toList >> List.foldl reduce initial


{-| Remove the element where `unique` of the element matches the `key`.
If **the key does not exist**, the `MultiSet` is **unchanged**

    openClosedBrackets =
        MultiSet.promising
            [ unique .open, unique .closed ]
            |> MultiSet.insert
                { open = "(", closed = ")" }

    openClosedBrackets
        |> MultiSet.remove .open ")"
        --> no change, .open is never ")"

    openClosedBrackets
        |> MultiSet.remove .closed ")"
        --> removes { open = "(", closed = ")" }

If there the checked aspect isn't promised to be unique, `remove` acts as a filter.

    MultiSet.promising
        [ unique .open, unique .closed ]
        |> MultiSet.insertAll
            [ { open = "[", closed = "]", meaning = List }
            , { open = "<", closed = ">", meaning = Custom }
            , { open = "\\", closed = "/", meaning = Custom }
            ]
        |> MultiSet.remove .meaning Custom

    --> MultiSet.promising
    -->     [ unique .open, unique .closed ]
    -->     |> MultiSet.insert
    -->         { open = "[", closed = "]", meaning = List }

-}
remove :
    (element -> aspect)
    -> aspect
    -> MultiSet element
    -> MultiSet element
remove aspect key =
    updateElements
        (List.filter
            (\element -> key /= aspect element)
        )


{-| Only keep elements that satisfy a test.


    operators =
        MultiSet.promising
            [ unique .symbol, unique .name ]
            |> MultiSet.insertAll
                [ { symbol = ">", name = "gt" }
                , { symbol = "<", name = "lt" }
                , { symbol = "==", name = "eq" }
                ]

    singleCharOperators =
        operators
            |> MultiSet.when
                (.symbol >> String.length >> (==) 1)

    --> MultiSet.promising
    -->     [ unique .symbol, unique .name ]
    -->     |> MultiSet.insertAll
    -->         [ { symbol = ">", name = "gt" }
    -->         , { symbol = "<", name = "lt" }
    -->         ]

-}
when : (element -> Bool) -> MultiSet element -> MultiSet element
when isGood =
    updateElements (List.filter isGood)


{-| The `List` containing all elements from most recently (= head) to least recently inserted element.

> The keys can be non-comparable. There is no obvious order.

> → You shouldn't rely on order when using functions like `fold` or `toList`.

    mostRecentlyInserted =
        List.head << MultiSet.toList

-}
toList : MultiSet element -> List element
toList =
    internalVal MultiSet >> .elements


uniqueness : MultiSet element -> List (Uniqueness element)
uniqueness =
    internalVal MultiSet >> .uniqueness


{-| Alter every element based on its current value.

    digitNames =
        MultiSet.promising
            [ unique .number, unique .name ]
            |> MultiSet.insertAll
                [ { number = 0, name = "zero" }
                , { number = 1, name = "one" }
                ]

    mathSymbolNames =
        digitNames
            |> MultiSet.map
                (\{ number, name } ->
                    { symbol = String.fromInt number, name = name }
                )
                [ unique .symbol, unique .name ]
            |> MultiSet.insert { symbol = "+", name = "plus" }

-}
map :
    (element -> mappedElement)
    -> List (Uniqueness mappedElement)
    -> MultiSet element
    -> MultiSet mappedElement
map alter uniquenessOfMappedElement =
    \multiSet ->
        multiSet
            |> fold (alter >> insert)
                (promising uniquenessOfMappedElement)


{-| Alter every element based on its current value.

Use [`map`](MultiSet#map) if your function changes the type of the element.

-}
updateAll :
    (element -> element)
    -> MultiSet element
    -> MultiSet element
updateAll alter =
    \multiSet ->
        multiSet
            |> map alter (uniqueness multiSet)


{-| A [Codec](https://package.elm-lang.org/packages/MartinSStewart/elm-serialize/latest/Serialize) to serialize a `MultiSet`.

    serializeUserMultiSet =
        MultiSet.serialze serializeUser
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
    -> Codec customError (MultiSet element)
serialize serializeElement uniqueness_ =
    Serialize.list serializeElement
        |> Serialize.map
            (List.foldl insert (promising uniqueness_))
            toList
