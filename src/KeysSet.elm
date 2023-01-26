module KeysSet exposing
    ( KeysSet, KeysSetTag
    , one
    , fromStack, fromList
    , size, element, end
    , insert, PreferenceOnCollisions(..), remove, elementAlter
    , map, mapTry
    , unifyWith, except, intersect
    , fold2From, FirstAndOrSecond(..)
    , toStack, toList
    , foldFrom, fold, foldFromOne
    )

{-| Lookup with multiple [`Keys`](Keys#Keys)

@docs KeysSet, KeysSetTag


## create

[`Emptiable.empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty),

@docs one
@docs fromStack, fromList


## scan

@docs size, element, end


## alter

@docs insert, PreferenceOnCollisions, remove, elementAlter
@docs map, mapTry


## combine

@docs unifyWith, except, intersect
@docs fold2From, FirstAndOrSecond


## transform

@docs toStack, toList
@docs foldFrom, fold, foldFromOne

-}

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable(..), emptyAdapt, fill, filled)
import Keys exposing (Key, Keys, keyIndex, keyOrderWith, toKeyWith)
import KeysSet.Internal exposing (treeElement)
import Linear exposing (Direction(..))
import List.Linear
import N exposing (Add1, Exactly, In, N, To, Up, n0)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Tree2
import Typed exposing (Checked, Public, Typed)


{-| 🗃️ Non-empty AVL-tree-based look-up. Elements and keys can be of any type

    import KeysSet exposing (KeysSet)
    import Stack
    import User exposing (User(..))

    users : Emptiable (KeysSet User User.ByName) never_
    users =
        KeysSet.fromStack User.byName
            (Stack.topBelow
                (User { name = "Alice", age = 28, height = 1.65 })
                [ User { name = "Bob", age = 19, height = 1.82 }
                , User { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

where

    module User exposing (ByName, User(..), byName)

    import KeysSet
    import Order

    type User
        = User { name : String, age : Int, height : Float }

    type Data
        = Data

    data : Map User Data { name : String, age : Int, height : Float }
    data =
        Typed.tag Data (\(User userData) -> userData)

    type alias ByName =
        Order.By
            (Map.Over UserData Record.Map.Name)
            ( String.Order.GreaterEarlier
            , ( Char.Order.Alphabetically, Char.Order.LowerUpper )
            )

    byName : Order.Key User String ByName
    byName =
        Keys.for (\name -> { name = name })
            |> Keys.and .name
                (Order.by data
                    (String.Order.greaterEarlier
                        (Char.Order.alphabetically Char.Order.lowerUpper)
                    )
                )

  - [`element`](#element), [`end`](#end), [`insert`](#insert) versions, [`elementAlter`](#elementAlter) versions, [`remove`](#remove) are runtime `log n`
  - [`toList`](#toList), [`toStack`](#toStack), [`foldFrom`](#foldFrom), [`fold`](#fold), [`foldFromOne`](#foldFromOne) are runtime `n`

-}
type alias KeysSet element tags lastIndex =
    Typed
        Checked
        ( KeysSetTag, tags )
        Public
        { size : Int
        , byKeys :
            ArraySized
                (Tree2.Branch element)
                (Exactly (Add1 lastIndex))
        }


{-| Tags a valid [`KeysSet`](#KeysSet) with multiple elements
-}
type KeysSetTag
    = KeysSet



-- create


{-| [`KeysSet`](#KeysSet) containing a single given element
-}
one :
    Keys element tags keys_ lastIndex
    ->
        (element
         -> Emptiable (KeysSet element tags lastIndex) never_
        )
one keys singleElement =
    keys
        |> Keys.toArray
        |> Typed.mapToWrap KeysSet
            (\keysArray ->
                { size = 1
                , byKeys =
                    keysArray
                        |> ArraySized.inToNumber
                        |> ArraySized.map
                            (\_ -> singleElement |> Tree2.one |> fill)
                }
            )
        |> filled


{-| Convert to a [`KeysSet`](#KeysSet),
⚠️ ignoring elements whose keys already exist **earlier** in the `List`

For construction, use [`fromStack`](#fromStack),
proving to the compiler what you already know about its (non-)emptiness

-}
fromList :
    Keys element tags keys_ lastIndex
    -> List element
    -> Emptiable (KeysSet element tags lastIndex) Possibly
fromList keys =
    \list ->
        list
            |> List.Linear.foldFrom Emptiable.empty
                Up
                (\toInsert soFar ->
                    soFar |> insert PreferExisting keys toInsert
                )


{-| Convert to a [`KeysSet`](#KeysSet),
⚠️ ignoring elements whose keys already exist **earlier** in the [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)

    KeysSet.fromStack User.byHandle
        (Stack.topBelow
            -- ↓ won't be part of the KeysSet
            { handle = "cr", shown = "chris \\o/" }
            [ { handle = "ann", shown = "Ann in stand by" }
            , { handle = "cr", shown = "creeper" }
            ]
        )

-}
fromStack :
    Keys element tags keys_ lastIndex
    -> Emptiable (Stacked element) possiblyOrNever
    -> Emptiable (KeysSet element tags lastIndex) possiblyOrNever
fromStack keys =
    \stack ->
        stack
            |> Emptiable.mapFlat
                (\stacked ->
                    stacked
                        |> filled
                        |> Stack.foldFromOne
                            (\a -> a |> one keys)
                            Up
                            (\toInsert soFar ->
                                soFar |> insert PreferExisting keys toInsert
                            )
                )


{-| Number of elements in the [`KeysSet`](#KeysSet).
Runtime `1`

    KeysSet.only "Hello"
        |> KeysSet.size
    --> 1

-}
size :
    Emptiable (KeysSet element_ tags_ lastIndex_) possiblyOrNever_
    -> Int
size =
    \keysSet ->
        case keysSet of
            Emptiable.Empty _ ->
                0

            Emptiable.Filled multiple ->
                multiple |> Typed.untag |> .size



-- scan


treeForIndex :
    N (In min_ (Up maxToLastIndex_ To lastIndex))
    ->
        (KeysSet element tags_ lastIndex
         -> Emptiable (Tree2.Branch element) never_
        )
treeForIndex index =
    \multiple ->
        multiple
            |> Typed.untag
            |> .byKeys
            |> ArraySized.inToOn
            |> ArraySized.element ( Up, index )
            |> Emptiable.filled


tree :
    ( Keys element tags keys lastIndex
    , keys -> Key element key_ (Up indexToLast_ To lastIndex)
    )
    ->
        (KeysSet element tags lastIndex
         -> Emptiable (Tree2.Branch element) never_
        )
tree key =
    \keysSet ->
        keysSet
            |> treeForIndex (key |> keyIndex)


{-| Access the element associated with a given key.
If no element with the given key is not present, `Emptiable.empty`

    import Emptiable exposing (Emptiable, filled, Emptiable.map)
    import Stack
    import KeysSet exposing (KeysSet)
    import Char.Order
    import String.Order

    type alias Animal =
        { name : String
        , kind : AnimalKind
        }

    type AnimalKind
        = Mouse
        | Cat

    animals : Emptiable (KeysSet Animal { byName : () }) never_
    animals =
        KeysSet.fromStack animalByName
            (Stack.topBelow
                { name = "Tom", kind = Cat }
                [ { name = "Jerry", kind = Mouse }
                ]
            )

    -- should be opaque and should use an opaque tag
    animalByName : Order.Key Animal String { byName : () }
    animalByName =
        Order.key .name
            { tag = { byName = () }
            , order =
                String.Order.greaterEarlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
            }

    animals |> KeysSet.element animalByName "Tom" |> Emptiable.map .kind
    --> filled Cat

    animals |> KeysSet.element animalByName "Jerry" |> Emptiable.map .kind
    --> filled Mouse

    animals |> KeysSet.element animalByName "Spike" |> Emptiable.map .kind
    --> Emptiable.empty

-}
element :
    ( Keys element tags keys lastIndex
    , keys -> Key element key (Up indexToLast_ To lastIndex)
    )
    -> key
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever_
         -> Emptiable element Possibly
        )
element ( keys, key ) keyToAccess =
    \keysSet ->
        keysSet
            |> Emptiable.emptyAdapt (\_ -> Possible)
            |> Emptiable.mapFlat
                (\info ->
                    info
                        |> tree ( keys, key )
                        |> treeElement
                            (\el ->
                                ( keyToAccess, el |> toKeyWith ( keys, key ) )
                                    |> keyOrderWith ( keys, key )
                            )
                )


{-| Get the element associated with a key at the end looking in a given `Direction`

  - minimum → `end Down`
  - maximum → `end Up`

[`KeysSet`](#KeysSet) is `empty` → Nothing

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable)
    import Stack
    import KeysSet exposing (KeysSet)
    import Char.Order
    import String.Order


    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

    users : Emptiable (KeysSet User { userByName : () }) never_
    users =
        KeysSet.fromStack userByName
            (Stack.topBelow
                { name = "Bob", age = 19, height = 1.82 }
                [ { name = "Alice", age = 28, height = 1.65 }
                , { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

    -- tag should be opaque
    userByName : Order.Key User String { userByName : () }
    userByName =
        Order.key .name
            { tag = { userByName = () }
            , order =
                String.Order.greaterEarlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
            }

    users |> KeysSet.end Down
    --> { name = "Alice", age = 28, height = 1.65 }

    users |> KeysSet.end Up
    --> { name = "Chuck", age = 33, height = 1.75 }

Notice how we safely avoided returning a `Maybe`
through the use of [`Emptiable ... Never`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)

If you don't know whether the [`KeysSet`](#KeysSet) will be empty

    users
        |> Emptiable.map (\us -> us |> filled |> KeysSet.end Up)
    --: Emptiable element Possibly

-}
end :
    ( Keys element tags keys lastIndex
    , keys -> Key element key_ (Up indexToLast_ To lastIndex)
    )
    -> Linear.Direction
    ->
        (Emptiable (KeysSet element tags lastIndex) Never
         -> element
        )
end ( keys, key ) direction =
    \keysSet ->
        keysSet
            |> Emptiable.fill
            |> tree ( keys, key )
            |> Tree2.end direction


filledInsert :
    Keys element tags keys_ lastIndex
    -> element
    ->
        (KeysSet element tags lastIndex
         -> Emptiable (KeysSet element tags lastIndex) never_
        )
filledInsert keys toInsert =
    let
        keyArray =
            keys
                |> Keys.toArray
                |> Typed.untag
                |> ArraySized.inToNumber
    in
    \keysSetFilled ->
        keysSetFilled
            |> Typed.map
                (\info ->
                    { size = info.size + 1
                    , byKeys =
                        info.byKeys
                            |> ArraySized.and keyArray
                            |> ArraySized.map
                                (\( branch, branchKey ) ->
                                    branch
                                        |> filled
                                        |> KeysSet.Internal.treeInsert branchKey toInsert
                                        |> fill
                                )
                    }
                )
            |> Typed.wrapToChecked KeysSet
            |> filled


{-| Strategy for dealing with the case the the element
you wanted to have in the [`KeysSet`](#KeysSet)
already has elements with a matching key.

  - `ReplaceExisting`: remove all elements with a matching key.
    The element you wanted to have in the [`KeysSet`](#KeysSet) can now be there!
  - `PreferExisting`: leave the [`KeysSet`](#KeysSet) as it was before,
    don't remove collisions.
    The element you wanted to have in the [`KeysSet`](#KeysSet) won't be there!

Used in [`insert`](#insert) and [`alter`](#alter)

-}
type PreferenceOnCollisions
    = ReplaceExisting
    | PreferExisting


{-| Insert a given element,
specifying what to do when there is a key collision


### keep collisions instead

    Emptiable.empty
        |> KeysSet.insert PreferExisting
            BracketPair.byOpenClosed
            { open = 'b', closed = 'C' }
        |> KeysSet.insert PreferExisting
            BracketPair.byOpenClosed
            { open = 'c', closed = 'A' }
        |> KeysSet.insert PreferExisting
            BracketPair.byOpenClosed
            { open = 'c', closed = 'C' }
        |> KeysSet.toList ( BracketPair.byOpenClosed, .open )
    --> [ { open = 'b', closed = 'C' }, { open = 'c', closed = 'A' } ]


### replace collisions

    Emptiable.empty
        |> KeysSet.insert ReplaceExisting
            BracketPair.byOpenClosed
            { open = 'b', closed = 'C' }
        |> KeysSet.insert ReplaceExisting
            BracketPair.byOpenClosed
            { open = 'c', closed = 'A' }
        |> KeysSet.insert ReplaceExisting
            BracketPair.byOpenClosed
            { open = 'c', closed = 'C' }
        |> KeysSet.toList ( BracketPair.byOpenClosed, .open )
    --> [ { open = 'c', closed = 'C' } ]

-}
insert :
    PreferenceOnCollisions
    -> Keys element tags keys_ lastIndex
    -> element
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element tags lastIndex) never_
        )
insert onCollisions keys toInsertOrReplacement =
    \keysSet ->
        case keysSet of
            Emptiable.Empty _ ->
                toInsertOrReplacement |> one keys

            Emptiable.Filled keysSetFill ->
                let
                    collisions =
                        keysSetFill
                            |> KeysSet.Internal.elementCollisions keys toInsertOrReplacement
                in
                case collisions |> Emptiable.map filled of
                    Emptiable.Empty _ ->
                        keysSetFill |> filledInsert keys toInsertOrReplacement

                    Emptiable.Filled collisionsFilled ->
                        case onCollisions of
                            PreferExisting ->
                                keysSetFill |> filled

                            ReplaceExisting ->
                                case
                                    keysSetFill
                                        |> filled
                                        |> exceptTree keys collisionsFilled
                                of
                                    Emptiable.Empty _ ->
                                        one keys toInsertOrReplacement

                                    Emptiable.Filled keySetFill ->
                                        keySetFill
                                            |> Typed.map
                                                (\info ->
                                                    { size = info.size + 1
                                                    , byKeys =
                                                        info.byKeys
                                                            |> ArraySized.and
                                                                (keys
                                                                    |> Keys.toArray
                                                                    |> Typed.untag
                                                                    |> ArraySized.inToNumber
                                                                )
                                                            |> ArraySized.map
                                                                (\( branchToAlter, branchOrder ) ->
                                                                    branchToAlter
                                                                        |> filled
                                                                        |> KeysSet.Internal.treeInsert branchOrder toInsertOrReplacement
                                                                        |> fill
                                                                )
                                                    }
                                                )
                                            |> Typed.wrapToChecked KeysSet
                                            |> filled


exceptTree :
    Keys element tags keys_ lastIndex
    -> Emptiable (Tree2.Branch element) exceptionsPossiblyOrNever_
    ->
        (Emptiable (KeysSet element tags lastIndex) Never
         -> Emptiable (KeysSet element tags lastIndex) Possibly
        )
exceptTree keys exceptions =
    \keysSetFilled ->
        keysSetFilled
            |> fill
            |> Typed.untag
            |> .byKeys
            |> ArraySized.and
                (keys
                    |> Keys.toArray
                    |> Typed.untag
                    |> ArraySized.inToNumber
                )
            |> ArraySized.map
                (\( branchToAlter, branchOrder ) ->
                    (branchToAlter |> filled)
                        |> KeysSet.Internal.treeExcept branchOrder exceptions
                )
            |> ArraySized.allFill
            |> Emptiable.map
                (\branches ->
                    keysSetFilled
                        |> fill
                        |> Typed.map
                            (\info ->
                                { size = info.size - (exceptions |> Tree2.size)
                                , byKeys = branches
                                }
                            )
                        |> Typed.wrapToChecked KeysSet
                )


{-| Remove its element whose key matches the given one.
If the key is not found, no changes are made

TODO example

-}
remove :
    ( Keys element tags keys lastIndex
    , keys -> Key element key (Up indexToLast_ To lastIndex)
    )
    -> key
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element tags lastIndex) Possibly
        )
remove ( keys, key ) keyToRemove =
    \keysSet ->
        case keysSet |> element ( keys, key ) keyToRemove of
            Emptiable.Empty _ ->
                keysSet |> Emptiable.emptyAdapt (\_ -> Possible)

            Emptiable.Filled toRemove ->
                keysSet
                    |> Emptiable.emptyAdapt (\_ -> Possible)
                    |> Emptiable.mapFlat
                        (\keysSetFill ->
                            keysSetFill
                                |> Typed.untag
                                |> .byKeys
                                |> ArraySized.and
                                    (keys
                                        |> Keys.toArray
                                        |> Typed.untag
                                        |> ArraySized.inToNumber
                                    )
                                |> ArraySized.map
                                    (\( branch, branchKey ) ->
                                        branch
                                            |> filled
                                            |> KeysSet.Internal.treeRemove
                                                (\el -> ( toRemove, el ) |> branchKey)
                                    )
                                |> ArraySized.allFill
                                |> Emptiable.map
                                    (\byKeysResult ->
                                        keysSetFill
                                            |> Typed.map
                                                (\info ->
                                                    { size = info.size - 1
                                                    , byKeys = byKeysResult
                                                    }
                                                )
                                            |> Typed.wrapToChecked KeysSet
                                    )
                        )



-- transform


{-| Change the element with a given key in a given way


### PreferExisting

    KeysSet.fromList Character.byIdOrChar
        [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
        |> KeysSet.elementAlter PreferExisting
            ( Character.byIdOrChar, .id )
            1
            (\c -> { c | char = 'C' })
            -- gets changed
        |> KeysSet.elementAlter PreferExisting
            ( Character.byIdOrChar, .id )
            1
            (\c -> { c | id = 0 })
            -- doesn't get changed
        |> KeysSet.toList ( Character.byIdOrChar, .id )
        --> [ { id = 0, char = 'A' }, { id = 1, char = 'C' } ]

If you want to sometimes [`remove`](#remove)
or [`insert`](#insert) a new value on empty for example,
first ask for the [`element`](#element) with the same key, then
match the [`Emptiable`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)
and operate as you like

-}
elementAlter :
    PreferenceOnCollisions
    ->
        ( Keys element tags keys lastIndex
        , keys -> Key element key (Up indexToLast_ To lastIndex)
        )
    -> key
    -> (element -> element)
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever
         -> Emptiable (KeysSet element tags lastIndex) possiblyOrNever
        )
elementAlter preferenceOnCollisions ( keys, key ) keyToAlter elementChange =
    \keysSet ->
        case keysSet |> element ( keys, key ) keyToAlter of
            Emptiable.Empty _ ->
                keysSet

            Emptiable.Filled elementToAlter ->
                let
                    elementAltered : element
                    elementAltered =
                        elementToAlter |> elementChange
                in
                case preferenceOnCollisions of
                    ReplaceExisting ->
                        keysSet
                            |> remove ( keys, key ) keyToAlter
                            |> insert ReplaceExisting keys elementAltered

                    PreferExisting ->
                        let
                            collisionsTree : Emptiable (Tree2.Branch element) Possibly
                            collisionsTree =
                                keysSet
                                    |> Emptiable.emptyAdapt (\_ -> Possible)
                                    |> Emptiable.mapFlat (KeysSet.Internal.elementCollisions keys elementAltered)
                        in
                        if (collisionsTree |> Tree2.size) >= 2 then
                            keysSet

                        else
                            keysSet
                                |> remove ( keys, key ) keyToAlter
                                |> insert PreferExisting keys elementAltered


{-| Convert to a `List` sorted by keys
in a given [`Direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Linear#Direction)

    import Linear exposing (Direction(..))
    import Stack
    import KeysSet
    import Char.Order
    import String.Order

    -- tag should be opaque in a separate module
    nameAlphabetical : Order.Key String String { alphabetical : () }
    nameAlphabetical =
        Order.key identity
            { tag = { alphabetical = () }
            , order =
                String.Order.greaterEarlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
            }

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeysSet.toList
    --> [ "Alice", "Bob" ]

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeysSet.toList
        |> List.reverse
    --> [ "Bob", "Alice" ]

to carry over information about (non-)emptiness → [`toStack`](#toStack)

Using `==` on [`KeysSet`](#KeysSet)s will be slower than [`toList`](#toList) if you have more than 2 keys.

-}
toList :
    ( Keys element tags keys lastIndex
    , keys -> Key element key_ (Up indexToLast_ To lastIndex)
    )
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever_
         -> List element
        )
toList ( keys, key ) =
    \keysSet ->
        case keysSet of
            Emptiable.Empty _ ->
                []

            Emptiable.Filled keysSetFill ->
                keysSetFill
                    |> tree ( keys, key )
                    |> Tree2.foldFrom [] Down (::)



-- transform


{-| Convert to a `List` sorted by keys
in a given [`Direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Linear#Direction)

    import Linear exposing (Direction(..))
    import Stack
    import KeysSet
    import Char.Order
    import String.Order

    -- tag should be opaque in a separate module
    nameAlphabetical : Order.Key String String { alphabetical : () }
    nameAlphabetical =
        Order.key identity
            { tag = { alphabetical = () }
            , order =
                String.Order.greaterEarlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
            }

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeysSet.toStack
    --> Stack.topBelow "Alice" [ "Bob" ]

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice", "Christoph" ])
        |> KeysSet.toStack
        |> Stack.reverse
    --> Stack.topBelow "Christoph" [ "Bob", "Alice" ]

The cool thing is that information about (non-)emptiness is carried over to the stack

Use this to fold over its elements

    import Linear exposing (Direction(..))
    import Stack
    import Int.Order
    import KeysSet

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeysSet.toStack (key intIncreasing identity)
    --> Stack.topBelow 543 [ 345, 234 ]
    --  the type knows it's never empty

    KeysSet.fromStack intIncreasing
        (Stack.topBelow False [ True, False ])
        |> KeysSet.toStack (key intIncreasing identity)
        |> Stack.foldFrom True Up (||)
    --> True

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 1 [ 2, 8, 16 ])
        |> KeysSet.toStack (key intIncreasing identity)
        |> Stack.fold Down (\n soFar -> soFar - n)
    --> 5


    intIncreasing : Keys Int ( (), Order.By Identity Int.Order.Increasing ) (Key Int Int (Up0 x)) (Up1 x)
    intIncreasing =
        Keys.for identity
            |> Keys.and Map.identity Int.Order.increasing

-}
toStack :
    ( Keys element tags keys lastIndex
    , keys -> Key element key_ (Up indexToLast_ To lastIndex)
    )
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever
         -> Emptiable (Stacked element) possiblyOrNever
        )
toStack ( keys, key ) =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFilled ->
                    keysSetFilled
                        |> tree ( keys, key )
                        |> Tree2.foldFromOne Stack.one Down Stack.onTopLay
                )


{-| Change each element based on its current value.

Runtime `n * log n` because mapped keys could be different (many other dicts/sets have runtime `n`)

If the keys of the mapped elements collide,
there's no promise of which element will be in the final mapped [`KeysSet`](#KeysSet)

-}
map :
    (element -> mappedElement)
    -> Keys mappedElement mappedTags mappedKeys_ mappedLastIndex
    ->
        (Emptiable (KeysSet element tags_ lastIndex_) possiblyOrNever
         -> Emptiable (KeysSet mappedElement mappedTags mappedLastIndex) possiblyOrNever
        )
map elementChange mappedKeys =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFilled ->
                    keysSetFilled
                        |> filled
                        |> foldFromOne
                            (\a -> a |> elementChange |> one mappedKeys)
                            (\element_ soFar ->
                                soFar |> insert PreferExisting mappedKeys (element_ |> elementChange)
                            )
                )


{-| Try to change each element based on its current value.

Runtime `n * log n` just like [`KeysSet.map`](#map) because mapped keys could be different

If the keys of the mapped elements collide,
there's no promise of which element will be in the final mapped [`KeysSet`](#KeysSet)

    {-| Keep only elements that pass a given test.
    Often called "filter"
    -}
    when orderKey isGood =
        KeysSet.mapTry
            (\element ->
                if element |> isGood then
                    Just element

                else
                    Nothing
            )
            orderKey

-}
mapTry :
    (element -> Emptiable mappedElement mappedElementPossiblyOrNever_)
    -> Keys mappedElement mappedTags mappedKeys_ mappedLastIndex
    ->
        (Emptiable (KeysSet element tags_ lastIndex_) possiblyOrNever_
         -> Emptiable (KeysSet mappedElement mappedTags mappedLastIndex) Possibly
        )
mapTry elementChangeTry mappedKeys =
    \keysSet ->
        keysSet
            |> foldFrom Emptiable.empty
                (\element_ ->
                    case element_ |> elementChangeTry of
                        Empty _ ->
                            identity

                        Filled elementMapped ->
                            \soFar ->
                                soFar
                                    |> insert PreferExisting mappedKeys elementMapped
                )



-- combine


{-| Fold, starting from one end as the initial accumulation value,
then reducing what's accumulated

    import Stack
    import KeysSet
    import Int.Order

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 1 [ 2, 8, 16 ])
        |> KeysSet.fold (\n soFar -> soFar - n)
    -- no! you shouldn't rely on order with fold

    -- ↓ tag should be opaque
    intIncreasing : Order.Key Int Int { increasing : () }
    intIncreasing =
        Order.key identity
            { tag = { increasing = () }
            , order = Int.Order.increasing
            }

-}
fold :
    (element -> (element -> element))
    ->
        (Emptiable (KeysSet element tags_ lastIndex_) Never
         -> element
        )
fold reduce =
    \keysSet ->
        keysSet |> foldFromOne identity reduce


{-| Fold, starting from one end element transformed to the initial accumulation value,
then reducing what's accumulated.

    import Stack
    import Int.Order
    import KeysSet

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeysSet.foldFromOne Stack.only Up Stack.onTopLay
    --> Stack.topBelow 543 [ 345, 234 ]
    -- no! you shouldn't rely on order with fold
    -- the type knows it's never empty

    -- tag should be opaque
    intIncreasing : Order.Key Int Int { increasing : () }
    intIncreasing =
        Order.key identity
            { tag = { increasing = () }
            , order = Int.Order.increasing
            }

A simpler version is [`fold`](#fold)

    KeysSet.fold =
        KeysSet.foldFromOne identity

-}
foldFromOne :
    (element -> accumulated)
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (KeysSet element tags_ lastIndex_) Never
         -> accumulated
        )
foldFromOne firstToInitial reduce =
    \keysSet ->
        keysSet
            |> fill
            |> treeForIndex n0
            |> Tree2.foldFromOne firstToInitial Up reduce


{-| Fold over its elements from an initial accumulator value

    import Stack
    import Int.Order
    import KeysSet

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeysSet.foldFrom [] (::)
    -- no! you shouldn't rely on order with fold
    --> [ 234, 345, 543 ]

    KeysSet.fromStack intIncreasing
        (Stack.topBelow False [ True, False ])
        |> KeysSet.foldFrom False (||)
    --> True

    -- tag should be opaque
    intIncreasing : Order.Key Int Int { increasing : () }
    intIncreasing =
        Order.key identity
            { tag = { increasing = () }
            , order = Int.Order.increasing
            }

-}
foldFrom :
    accumulated
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (KeysSet element tags_ lastIndex_) possiblyOrNever_
         -> accumulated
        )
foldFrom initial reduce =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFill ->
                    keysSetFill |> treeForIndex n0
                )
            |> Tree2.foldFrom initial Up reduce


{-| Combine with another [`KeysSet`](#KeysSet).
On key collision, keep the current [`KeysSet`](#KeysSet)'s element.

(To instead replace current elements with incoming elements, swap the arguments)

-}
unifyWith :
    Keys element tags keys_ lastIndex
    -> Emptiable (KeysSet element tags lastIndex) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever
         -> Emptiable (KeysSet element tags lastIndex) possiblyOrNever
        )
unifyWith orderKey toCombineWith =
    \keysSet ->
        toCombineWith
            |> foldFrom keysSet
                (\rightElement soFar ->
                    soFar |> insert PreferExisting orderKey rightElement
                )


{-| Keep each element whose key also appears in a given [`KeysSet`](#KeysSet)
-}
intersect :
    ( Keys element tags keys lastIndex
    , keys -> Key element key_ (Up indexToLast_ To lastIndex)
    )
    -> Emptiable (KeysSet element tags lastIndex) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element tags lastIndex) Possibly
        )
intersect ( keys, key ) toIntersectWith =
    \keysSet ->
        keysSet
            |> foldFrom
                (keysSet |> emptyAdapt (\_ -> Possible))
                (\element_ ->
                    case toIntersectWith |> element ( keys, key ) (element_ |> toKeyWith ( keys, key )) of
                        Emptiable.Filled _ ->
                            identity

                        Emptiable.Empty _ ->
                            \intersectionSoFar ->
                                intersectionSoFar
                                    |> remove ( keys, key )
                                        (element_ |> toKeyWith ( keys, key ))
                )


{-| Keep only those elements whose keys don't appear in the given [`KeysSet`](#KeysSet)
-}
except :
    ( Keys element tags keys lastIndex
    , keys -> Key element key_ (Up indexToLast_ To lastIndex)
    )
    -> Emptiable (KeysSet element tags toExcludeLastIndex_) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element tags lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element tags lastIndex) Possibly
        )
except ( keys, key ) toExclude =
    \keysSet ->
        toExclude
            |> foldFrom
                (keysSet |> emptyAdapt (\_ -> Possible))
                (\element_ diffSoFar ->
                    diffSoFar
                        |> remove ( keys, key )
                            (element_ |> toKeyWith ( keys, key ))
                )


{-| Unresolved element state while merging a "first" and "second" structure

  - only in the `First` structure
  - only in the `Second` structure
  - in both `FirstSecond`

`FirstSecond` has a tuple instead of a more descriptive record to make matching easier

-}
type FirstAndOrSecond first second
    = First first
    | Second second
    | FirstSecond ( first, second )


{-| Most powerful way of combining 2 [`KeysSet`](#KeysSet)s

Traverses all the keys from both [`KeysSet`](#KeysSet)s _from lowest to highest_,
accumulating whatever you want
for when a key appears in either the [`FirstAndOrSecond`](#FirstAndOrSecond)
of the [`KeysSet`](#KeysSet)s

You will find this as "merge" in most other dictionaries/sets

-}
fold2From :
    accumulated
    ->
        (FirstAndOrSecond firstElement secondElement
         -> (accumulated -> accumulated)
        )
    ->
        ({ first :
            { key :
                ( Keys firstElement firstTags firstKeys firstLastIndex
                , firstKeys -> Key firstElement key (Up firstIndexToLastIndex_ To firstLastIndex)
                )
            , set :
                Emptiable
                    (KeysSet firstElement firstTags firstLastIndex)
                    firstPossiblyOrNever_
            }
         , second :
            { key :
                ( Keys secondElement secondTags secondKeys secondLastIndex
                , secondKeys -> Key secondElement key (Up secondIndexToLastIndex_ To secondLastIndex)
                )
            , set :
                Emptiable
                    (KeysSet secondElement secondTags secondLastIndex)
                    secondPossiblyOrNever_
            }
         }
         -> accumulated
        )
fold2From initial reduce { first, second } =
    let
        secondAccumulate :
            firstElement
            ->
                ({ secondRemainder : List secondElement, accumulated : accumulated }
                 -> { secondRemainder : List secondElement, accumulated : accumulated }
                )
        secondAccumulate firstElement =
            \soFar ->
                case soFar.secondRemainder of
                    [] ->
                        { secondRemainder = []
                        , accumulated =
                            soFar.accumulated |> reduce (First firstElement)
                        }

                    secondElement :: secondRest ->
                        case
                            ( firstElement |> toKeyWith first.key
                            , secondElement |> toKeyWith second.key
                            )
                                |> keyOrderWith first.key
                        of
                            EQ ->
                                { secondRemainder = secondRest
                                , accumulated =
                                    soFar.accumulated
                                        |> reduce (FirstSecond ( firstElement, secondElement ))
                                }

                            LT ->
                                { secondRemainder = secondElement :: secondRest
                                , accumulated =
                                    soFar.accumulated |> reduce (First firstElement)
                                }

                            GT ->
                                secondAccumulate firstElement
                                    { secondRemainder = secondRest
                                    , accumulated =
                                        soFar.accumulated |> reduce (Second secondElement)
                                    }

        secondAccumulated =
            first.set
                |> foldFrom
                    { secondRemainder = second.set |> toList second.key
                    , accumulated = initial
                    }
                    secondAccumulate
    in
    secondAccumulated.secondRemainder
        |> List.Linear.foldFrom secondAccumulated.accumulated
            Up
            (\secondRemainderElement soFar ->
                soFar |> reduce (Second secondRemainderElement)
            )
