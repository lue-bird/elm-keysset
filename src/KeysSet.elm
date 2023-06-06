module KeysSet exposing
    ( KeysSet
    , one
    , fromStack, fromList
    , size, element, end
    , insertIfNoCollision, insertReplacingCollisions
    , remove
    , elementAlterIfNoCollision, elementAlterReplacingCollisions
    , map, fillsMap
    , unifyWith, except, intersect
    , fold2From, fold2FromOne
    , toKeys, toStack, toList
    , foldFrom, foldFromOne
    , foldUntilCompleteFrom, foldUntilCompleteFromOne
    )

{-| Lookup with multiple [`Keys`](Keys#Keys)

@docs KeysSet

  - [`element`](#element), [`end`](#end), [insert](#insertIfNoCollision) versions, [elementAlter](#elementAlterIfNoCollision) versions, [`remove`](#remove) are runtime `log n`
  - [`size`](#size) is runtime `1`
  - [`toList`](#toList), [`toStack`](#toStack), [`toKeys`](#toKeys), [`foldFrom`](#foldFrom), [`foldFromOne`](#foldFromOne), [`foldUntilCompleteFrom`](#foldUntilCompleteFrom), [`foldUntilCompleteFromOne`](#foldUntilCompleteFromOne) are runtime `n`


## create

[`Emptiable.empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty),

@docs one
@docs fromStack, fromList


## observe

@docs size, element, end


## alter

@docs insertIfNoCollision, insertReplacingCollisions
@docs remove
@docs elementAlterIfNoCollision, elementAlterReplacingCollisions
@docs map, fillsMap


## combine

@docs unifyWith, except, intersect
@docs fold2From, fold2FromOne


## transform

@docs toKeys, toStack, toList
@docs foldFrom, foldFromOne
@docs foldUntilCompleteFrom, foldUntilCompleteFromOne

There are more fold variants that could be exposed if you have a need for them,
namely

  - `fold...AnyOrder` which doesn't need a specific key and direction
  - `foldNavigate` which can decide for each value whether to even fold up or down

Open an issue and I'll expose them :)

-}

import And exposing (And)
import AndOr exposing (AndOr)
import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable(..), emptyAdapt, fill, filled)
import Keys exposing (Key, Keys, keyOrderWith, toKeyWith)
import Keys.Internal exposing (keyElement)
import KeysSet.Internal
import Linear exposing (Direction(..))
import List.Linear
import N exposing (Add1, Exactly, N1, On, n1)
import Order
import PartialOrComplete exposing (PartialOrComplete)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Tree2
import Tree2.Sorted
import Typed


{-| 🗃️ Non-empty AVL-tree-based look-up. Elements and keys can be of any type

    import KeysSet exposing (KeysSet)
    import Stack
    import User exposing (User(..))

    users : Emptiable (KeysSet User User.ByName N1) never_
    users =
        KeysSet.fromStack User.byName
            (Stack.topBelow
                (User { name = "Alice", age = 28, height = 1.65 })
                [ User { name = "Bob", age = 19, height = 1.82 }
                , User { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

where

    -- module User exposing (ByName, User(..), byName)


    import Char.Order
    import Keys exposing (Key, Keys)
    import KeysSet
    import Map exposing (Mapping)
    import N exposing (N1)
    import Order
    import String.Order

    type User
        = User { name : String, age : Int, height : Float }

    type Name
        = Name

    name : Mapping User Name String
    name =
        Map.tag Data (\(User userData) -> userData.name)

    type alias ByName =
        { name :
            Key
                User
                (Order.By
                    Name
                    (String.Order.Earlier
                        (Char.Order.AToZ Char.Order.LowerUpper)
                    )
                )
                String
                N1
        }

    byName : Keys User ByName N1
    byName =
        Keys.for (\name_ -> { name = name_ })
            |> Keys.by ( identity, Map.identity )
                (String.Order.earlier
                    (Char.Order.aToZ Char.Order.lowerUpper)
                )

-}
type alias KeysSet element keys keyCount =
    KeysSet.Internal.KeysSet element keys keyCount


{-| [`KeysSet`](#KeysSet) containing a single given element.

You don't need to provide [`Keys`](Keys#Keys)

-}
one : element -> Emptiable (KeysSet element keys_ keyCount_) never_
one singleElement =
    KeysSet.Internal.one singleElement |> filled


{-| Convert to a [`KeysSet`](#KeysSet),
⚠️ ignoring elements whose keys already exist **earlier** in the `List`

    KeysSet.fromList User.byHandle
        [ -- ↓ won't be part of the KeysSet
          { handle = "cr", shown = "chris \\o/" }
        , { handle = "ann", shown = "Ann in stand by" }
        , -- ↓ will be part of the KeysSet
          { handle = "cr", shown = "creeper" }
        ]

For construction, use [`fromStack`](#fromStack) instead,
proving to the compiler what you already know about its (non-)emptiness

-}
fromList :
    Keys element keys keyCount
    -> List element
    -> Emptiable (KeysSet element keys keyCount) Possibly
fromList keys =
    \list ->
        list
            |> List.Linear.foldFrom Emptiable.empty
                Up
                (\toInsert soFar ->
                    soFar |> insertIfNoCollision keys toInsert
                )


{-| Convert to a [`KeysSet`](#KeysSet),
⚠️ ignoring elements whose keys already exist **earlier** in the [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)

    KeysSet.fromStack User.byHandle
        (Stack.topBelow
            -- ↓ won't be part of the KeysSet
            { handle = "cr", shown = "chris \\o/" }
            [ { handle = "ann", shown = "Ann in stand by" }
            , -- ↓ will be part of the KeysSet
              { handle = "cr", shown = "creeper" }
            ]
        )

-}
fromStack :
    Keys element keys keyCount
    -> Emptiable (Stacked element) possiblyOrNever
    -> Emptiable (KeysSet element keys keyCount) possiblyOrNever
fromStack keys =
    \stack ->
        stack
            |> Emptiable.mapFlat
                (\stacked ->
                    stacked
                        |> filled
                        |> Stack.foldFromOne one
                            Up
                            (\toInsert soFar ->
                                soFar |> insertIfNoCollision keys toInsert
                            )
                )


{-| Number of elements in the [`KeysSet`](#KeysSet).
Runtime `1`

    KeysSet.one { name = "Helium", symbol = "He", atomicNumber = 2 }
        |> KeysSet.size
    --> 1

-}
size : Emptiable (KeysSet element_ keys_ keyCount_) possiblyOrNever_ -> Int
size =
    \keysSet ->
        case keysSet of
            Emptiable.Empty _ ->
                0

            Emptiable.Filled keysSetFill ->
                keysSetFill |> KeysSet.Internal.size


tree :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    ->
        (KeysSet element keys keyCount
         -> Emptiable (Tree2.Branch element) never_
        )
tree key =
    \keysSet ->
        keysSet |> KeysSet.Internal.treeForElement (key |> keyElement)


{-| Access the element associated with a given key.
If no element with the given key is not present, `Emptiable.empty`

    import Emptiable exposing (Emptiable, filled)
    import Typed
    import Stack
    import N exposing (N1)
    import Keys exposing (Keys, Key)
    import KeysSet exposing (KeysSet)
    import Map exposing (Mapping)
    import Order
    import Char.Order
    import String.Order

    type alias Animal =
        { name : String
        , kind : AnimalKind
        }

    type AnimalKind
        = Mouse
        | Cat

    type alias ByName =
        { name :
            Key
                Animal
                (Order.By
                    Name
                    (String.Order.Earlier
                        (Char.Order.AToZ Char.Order.LowerUpper)
                    )
                )
                String
                N1
        }

    animals : Emptiable (KeysSet Animal ByName N1) never_
    animals =
        KeysSet.fromStack animalByName
            (Stack.topBelow
                { name = "Tom", kind = Cat }
                [ { name = "Jerry", kind = Mouse }
                ]
            )

    type Name
        = Name -- not exposed

    name : Mapping Animal Name String
    name =
        Map.tag Name .name

    animalByName : Keys Animal ByName N1
    animalByName =
        Keys.for (\name_ -> { name = name_ })
            |> Keys.by ( .name, name )
                (String.Order.earlier
                    (Char.Order.aToZ Char.Order.lowerUpper)
                )

    animals |> KeysSet.element ( animalByName, .name ) "Tom" |> Emptiable.map .kind
    --> filled Cat

    animals |> KeysSet.element ( animalByName, .name ) "Jerry" |> Emptiable.map .kind
    --> filled Mouse

    animals |> KeysSet.element ( animalByName, .name ) "Spike" |> Emptiable.map .kind
    --> Emptiable.empty

-}
element :
    ( Keys element keys keyCount
    , keys -> Key element by_ key keyCount
    )
    -> key
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever_
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
                        |> Tree2.Sorted.treeElement
                            (\el ->
                                ( keyToAccess, el |> toKeyWith ( keys, key ) )
                                    |> keyOrderWith ( keys, key )
                            )
                )


{-| The element associated with the lowest key

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable)
    import N exposing (N1)
    import Stack
    import KeysSet exposing (KeysSet)
    import Map exposing (Mapping)
    import Order
    import Char.Order
    import String.Order
    import Keys exposing (Keys, Key)

    users : Emptiable (KeysSet User ByName N1) never_
    users =
        KeysSet.fromStack userByName
            (Stack.topBelow
                { name = "Bob", age = 19, height = 1.80 }
                [ { name = "Alia", age = 28, height = 1.69 }
                , { name = "Chucki", age = 33, height = 1.75 }
                ]
            )

    users |> KeysSet.end ( userByName, .name ) Down
    --> { name = "Alia", age = 28, height = 1.69 }

    users |> KeysSet.end ( userByName, .name ) Up
    --> { name = "Chucki", age = 33, height = 1.75 }

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

    type alias ByName =
        { name :
            Key
                User
                (Order.By
                    Name
                    (String.Order.Earlier
                        (Char.Order.AToZ Char.Order.LowerUpper)
                    )
                )
                String
                N1
        }

    type Name
        = Name -- not exposed

    name : Mapping User Name String
    name =
        Map.tag Name .name

    userByName : Keys User ByName N1
    userByName =
        Keys.for (\name_ -> { name = name_ })
            |> Keys.by ( .name, name )
                (String.Order.earlier
                    (Char.Order.aToZ Char.Order.lowerUpper)
                )

Notice how we safely avoided returning a `Maybe`
through the use of [`Emptiable ... Never`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)

If you don't know whether the [`KeysSet`](#KeysSet) will be empty

    users
        |> Emptiable.map (filled >> KeysSet.end ( userByName, .name ) Down)
    --: Emptiable element Possibly

-}
end :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    -> Linear.Direction
    ->
        (Emptiable (KeysSet element keys keyCount) Never
         -> element
        )
end ( keys, key ) direction =
    \keysSet ->
        keysSet
            |> fill
            |> tree ( keys, key )
            |> Tree2.end direction


fillInsertOnNoCollision :
    Keys element keys keyCount
    -> element
    ->
        (KeysSet element keys keyCount
         -> Emptiable (KeysSet element keys keyCount) never_
        )
fillInsertOnNoCollision keys toInsert =
    let
        keyArray : ArraySized (( element, element ) -> Order) (Exactly keyCount)
        keyArray =
            keys |> Keys.toArray |> ArraySized.inToNumber
    in
    \keysSetFill ->
        keysSetFill
            |> KeysSet.Internal.toMultiple keys
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
                                        |> Tree2.Sorted.treeInsertIfNoCollision branchKey toInsert
                                        |> fill
                                )
                    }
                )
            |> Typed.toChecked (KeysSet.Internal.tagFor keys)
            |> KeysSet.Internal.fromMultiple
            |> filled


elementCollisions :
    Keys element keys keyCount
    -> element
    ->
        (KeysSet element keys keyCount
         -> Emptiable (Tree2.Branch element) Possibly
        )
elementCollisions keys toCollideWith =
    \keysSet ->
        keysSet
            |> KeysSet.Internal.toMultiple keys
            |> Typed.untag
            |> .byKeys
            |> ArraySized.and
                (keys
                    |> Keys.toArray
                    |> ArraySized.inToNumber
                )
            |> ArraySized.foldFrom Emptiable.empty
                Down
                (\( branch, key ) soFar ->
                    soFar
                        |> (case
                                branch
                                    |> filled
                                    |> Tree2.Sorted.treeElement (\el -> ( toCollideWith, el ) |> key)
                            of
                                Emptiable.Empty _ ->
                                    identity

                                Emptiable.Filled collision ->
                                    Tree2.Sorted.treeInsertIfNoCollision key collision
                           )
                )


{-| Insert a given element.
If the element you wanted to insert already has elements with a matching key (collisions),
keep the existing collision elements instead.
To replace collisions instead → [`insertReplacingCollisions`](#insertReplacingCollisions)

    import BracketPair
    import Emptiable

    Emptiable.empty
        |> KeysSet.insertIfNoCollision BracketPair.keys
            { open = 'b', closed = 'C' }
        |> KeysSet.insertIfNoCollision BracketPair.keys
            { open = 'c', closed = 'A' }
        |> KeysSet.insertIfNoCollision BracketPair.keys
            { open = 'c', closed = 'C' }
        |> KeysSet.toList ( BracketPair.keys, .open )
    --> [ { open = 'b', closed = 'C' }, { open = 'c', closed = 'A' } ]

-}
insertIfNoCollision :
    Keys element keys keyCount
    -> element
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever_
         -> Emptiable (KeysSet element keys keyCount) never_
        )
insertIfNoCollision keys toInsertOrReplacement =
    \keysSet ->
        case keysSet of
            Emptiable.Empty _ ->
                toInsertOrReplacement |> one

            Emptiable.Filled keysSetFill ->
                let
                    collisions =
                        keysSetFill
                            |> elementCollisions keys toInsertOrReplacement
                in
                case collisions of
                    Emptiable.Empty _ ->
                        keysSetFill |> fillInsertOnNoCollision keys toInsertOrReplacement

                    Emptiable.Filled _ ->
                        keysSetFill |> filled


{-| Insert a given element.
If the element you wanted to insert already has elements with a matching key (collisions),
replace all collisions.
To keep collisions instead → [`insertIfNoCollision`](#insertIfNoCollision)

    import BracketPair
    import Emptiable

    Emptiable.empty
        |> KeysSet.insertReplacingCollisions BracketPair.keys
            { open = 'b', closed = 'C' }
        |> KeysSet.insertReplacingCollisions BracketPair.keys
            { open = 'c', closed = 'A' }
        |> KeysSet.insertReplacingCollisions BracketPair.keys
            { open = 'c', closed = 'C' }
        |> KeysSet.toList ( BracketPair.keys, .open )
    --> [ { open = 'c', closed = 'C' } ]

-}
insertReplacingCollisions :
    Keys element keys keyCount
    -> element
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever_
         -> Emptiable (KeysSet element keys keyCount) never_
        )
insertReplacingCollisions keys toInsertOrReplacement =
    \keysSet ->
        case keysSet of
            Emptiable.Empty _ ->
                toInsertOrReplacement |> one

            Emptiable.Filled keysSetFill ->
                let
                    collisions =
                        keysSetFill
                            |> elementCollisions keys toInsertOrReplacement
                in
                case collisions of
                    Emptiable.Empty _ ->
                        keysSetFill |> fillInsertOnNoCollision keys toInsertOrReplacement

                    Emptiable.Filled collisionsTreeFilled ->
                        case
                            keysSetFill
                                |> filled
                                |> exceptTree keys (collisionsTreeFilled |> filled)
                        of
                            Emptiable.Empty _ ->
                                toInsertOrReplacement |> one

                            Emptiable.Filled keySetFill ->
                                keySetFill
                                    |> KeysSet.Internal.toMultiple keys
                                    |> Typed.map
                                        (\info ->
                                            { size = info.size + 1
                                            , byKeys =
                                                info.byKeys
                                                    |> ArraySized.and
                                                        (keys
                                                            |> Keys.toArray
                                                            |> ArraySized.inToNumber
                                                        )
                                                    |> ArraySized.map
                                                        (\( branchToAlter, branchOrder ) ->
                                                            branchToAlter
                                                                |> filled
                                                                |> Tree2.Sorted.treeInsertIfNoCollision branchOrder toInsertOrReplacement
                                                                |> fill
                                                        )
                                            }
                                        )
                                    |> Typed.toChecked (KeysSet.Internal.tagFor keys)
                                    |> KeysSet.Internal.fromMultiple
                                    |> filled


exceptTree :
    Keys element keys keyCount
    -> Emptiable (Tree2.Branch element) exceptionsPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys keyCount) Never
         -> Emptiable (KeysSet element keys keyCount) Possibly
        )
exceptTree keys exceptions =
    \keysSetFilled ->
        let
            multipleLike =
                keysSetFilled
                    |> fill
                    |> KeysSet.Internal.toMultiple keys
        in
        multipleLike
            |> Typed.untag
            |> .byKeys
            |> ArraySized.and
                (keys
                    |> Keys.toArray
                    |> ArraySized.inToNumber
                )
            |> ArraySized.map
                (\( branchToAlter, branchOrder ) ->
                    (branchToAlter |> filled)
                        |> Tree2.Sorted.treeExcept branchOrder exceptions
                )
            |> ArraySized.allFill
            |> Emptiable.map
                (\branches ->
                    multipleLike
                        |> Typed.map
                            (\info ->
                                { size = info.size - (exceptions |> Tree2.size)
                                , byKeys = branches
                                }
                            )
                        |> Typed.toChecked (KeysSet.Internal.tagFor keys)
                        |> KeysSet.Internal.fromMultiple
                )


{-| Remove its element whose key matches the given one.
If the key is not found, no changes are made

    import Character

    KeysSet.fromList Character.keys
        [ { id = 0, char = 'A' }
        , { id = 1, char = 'B' }
        ]
        |> KeysSet.insertIfNoCollision Character.keys
            { id = 2, char = 'C' }
        |> KeysSet.remove ( Character.keys, .id ) 2
        |> KeysSet.toList ( Character.keys, .id )
    --> [ { id = 0, char = 'A' }
    --> , { id = 1, char = 'B' }
    --> ]

-}
remove :
    ( Keys element keys keyCount
    , keys -> Key element by_ key keyCount
    )
    -> key
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever_
         -> Emptiable (KeysSet element keys keyCount) Possibly
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
                            let
                                multipleLike : KeysSet.Internal.Multiple element keys keyCount
                                multipleLike =
                                    keysSetFill |> KeysSet.Internal.toMultiple keys
                            in
                            multipleLike
                                |> Typed.untag
                                |> .byKeys
                                |> ArraySized.and
                                    (keys
                                        |> Keys.toArray
                                        |> ArraySized.inToNumber
                                    )
                                |> ArraySized.map
                                    (\( branch, branchKey ) ->
                                        branch
                                            |> filled
                                            |> Tree2.Sorted.treeRemove
                                                (\el -> ( toRemove, el ) |> branchKey)
                                    )
                                |> ArraySized.allFill
                                |> Emptiable.map
                                    (\byKeysResult ->
                                        multipleLike
                                            |> Typed.map
                                                (\info ->
                                                    { size = info.size - 1
                                                    , byKeys = byKeysResult
                                                    }
                                                )
                                            |> Typed.toChecked (KeysSet.Internal.tagFor keys)
                                            |> KeysSet.Internal.fromMultiple
                                    )
                        )


{-| Change the element with a given key in a given way
Only actually alter the element if the result doesn't have existing elements with a matching key (collisions).
To replace collisions with the result instead → [`elementAlterReplacingCollisions`](#elementAlterReplacingCollisions)

    import Character

    KeysSet.fromList Character.keys
        [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
        |> KeysSet.elementAlterIfNoCollision ( Character.keys, .id )
            1
            (\c -> { c | char = 'C' })
            -- gets changed
        |> KeysSet.elementAlterIfNoCollision ( Character.keys, .id )
            1
            (\c -> { c | id = 0 })
            -- doesn't get changed
        |> KeysSet.toList ( Character.keys, .id )
        --> [ { id = 0, char = 'A' }, { id = 1, char = 'C' } ]

If you want to sometimes [`remove`](#remove)
or [insert](#insertIfNoCollision) a new value on empty for example,
first ask for the [`element`](#element) with the same key, then
match the [`Emptiable`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)
and operate as you like

-}
elementAlterIfNoCollision :
    ( Keys element keys keyCount
    , keys -> Key element by_ key keyCount
    )
    -> key
    -> (element -> element)
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever
         -> Emptiable (KeysSet element keys keyCount) possiblyOrNever
        )
elementAlterIfNoCollision ( keys, key ) keyToAlter elementChange =
    \keysSet ->
        case keysSet |> element ( keys, key ) keyToAlter of
            Emptiable.Empty _ ->
                keysSet

            Emptiable.Filled elementToAlter ->
                let
                    elementAltered : element
                    elementAltered =
                        elementToAlter |> elementChange

                    collisionsTree : Emptiable (Tree2.Branch element) Possibly
                    collisionsTree =
                        keysSet
                            |> Emptiable.emptyAdapt (\_ -> Possible)
                            |> Emptiable.mapFlat (elementCollisions keys elementAltered)
                in
                if (collisionsTree |> Tree2.size) >= 2 then
                    keysSet

                else
                    case keysSet |> remove ( keys, key ) keyToAlter of
                        Emptiable.Empty _ ->
                            elementAltered |> one

                        Emptiable.Filled removed ->
                            removed |> fillInsertOnNoCollision keys elementAltered


{-| Change the element with a given key in a given way
If the result has existing elements with a matching key (collisions), replace them.
To not alter the element if there are collisions with the result instead → [`elementAlterIfNoCollision`](#elementAlterIfNoCollision)

    import Character

    KeysSet.fromList Character.keys
        [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
        |> KeysSet.elementAlterIfNoCollision ( Character.keys, .id )
            1
            (\c -> { c | char = 'C' })
            -- gets changed
        |> KeysSet.elementAlterIfNoCollision ( Character.keys, .id )
            1
            (\c -> { c | id = 0 })
            -- doesn't get changed
        |> KeysSet.toList ( Character.keys, .id )
        --> [ { id = 0, char = 'A' }, { id = 1, char = 'C' } ]

If you want to sometimes [`remove`](#remove)
or [insert](#insertIfNoCollision) a new value on empty for example,
first ask for the [`element`](#element) with the same key, then
match the [`Emptiable`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)
and operate as you like

-}
elementAlterReplacingCollisions :
    ( Keys element keys keyCount
    , keys -> Key element by_ key keyCount
    )
    -> key
    -> (element -> element)
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever
         -> Emptiable (KeysSet element keys keyCount) possiblyOrNever
        )
elementAlterReplacingCollisions ( keys, key ) keyToAlter elementChange =
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
                keysSet
                    |> remove ( keys, key ) keyToAlter
                    |> insertReplacingCollisions keys elementAltered


{-| A [`KeysSet`](#KeysSet) sorted by the identity of one of the keys of the original set.
Runtime is O(n).
-}
toKeys :
    ( Keys element keys keyCount
    , keys -> Key element (Order.By toKeyTag_ keyOrderTag) key keyCount
    )
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever
         ->
            Emptiable
                (KeysSet key (Keys.Identity key keyOrderTag) N1)
                possiblyOrNever
        )
toKeys ( keys, key ) =
    \keysSet ->
        keysSet
            |> Emptiable.map
                (\keysSetFill ->
                    let
                        info : { size : Int, byKeys : ArraySized (Tree2.Branch element) (Exactly keyCount) }
                        info =
                            keysSetFill |> KeysSet.Internal.toMultiple keys |> Typed.untag
                    in
                    { size = info.size
                    , byKeys =
                        keysSetFill
                            |> tree ( keys, key )
                            |> Tree2.map (Keys.toKeyWith ( keys, key ))
                            |> fill
                            |> ArraySized.one
                            |> ArraySized.inToNumber
                    }
                        |> Typed.tag (KeysSet.Internal.tagForIdentity ( keys, key ))
                        |> KeysSet.Internal.fromMultiple
                )


{-| Convert to a `List`

    import N exposing (Up, To, N0)
    import Stack
    import Keys exposing (Keys, Key)
    import KeysSet
    import Map
    import Order
    import Char.Order
    import String.Order

    nameAlphabetical : Keys.Identity String (String.Order.Earlier (Char.Order.AToZ Char.Order.LowerUpper))
    nameAlphabetical =
        Keys.identity
            (String.Order.earlier
                (Char.Order.aToZ Char.Order.lowerUpper)
            )

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeysSet.toList ( nameAlphabetical, identity )
    --> [ "Alice", "Bob" ]

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeysSet.toList ( nameAlphabetical, identity )
        |> List.reverse
    --> [ "Bob", "Alice" ]

to carry over information about (non-)emptiness → [`toStack`](#toStack)

Using `==` on [`KeysSet`](#KeysSet)s will be slower than [`toList`](#toList) if you have more than 2 keys.

-}
toList :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever_
         -> List element
        )
toList ( keys, key ) =
    \keysSet ->
        keysSet |> foldFrom ( keys, key ) [] Down (::)



-- transform


{-| Convert to a `List` sorted by a given key

    import N exposing (Up, To, N0)
    import Stack
    import Map
    import Order
    import Char.Order
    import String.Order
    import Keys exposing (Keys, Key)
    import KeysSet

    nameAlphabetical : Keys.Identity String (String.Order.Earlier (Char.Order.AToZ Char.Order.LowerUpper))
    nameAlphabetical =
        Keys.identity
            (String.Order.earlier
                (Char.Order.aToZ Char.Order.lowerUpper)
            )

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeysSet.toStack ( nameAlphabetical, identity )
    --> Stack.topBelow "Alice" [ "Bob" ]

    KeysSet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice", "Christoph" ])
        |> KeysSet.toStack ( nameAlphabetical, identity )
        |> Stack.reverse
    --> Stack.topBelow "Christoph" [ "Bob", "Alice" ]

The cool thing is that information about (non-)emptiness is carried over to the stack

Use this to fold over its elements

    import N exposing (Up, To, N0)
    import Map
    import Stack
    import Order
    import Int.Order
    import Keys exposing (Keys, Key)
    import KeysSet
    import Linear exposing (Direction(..))

    intUp : Keys.Identity Int Int.Order.Up
    intUp =
        Keys.identity Int.Order.up

    KeysSet.fromStack intUp
        (Stack.topBelow 345 [ 234, 543 ])
        |> KeysSet.toStack ( intUp, identity )
    --> Stack.topBelow 234 [ 345, 543 ]
    --  the type knows it's never empty

    KeysSet.fromStack intUp
        (Stack.topBelow 1 [ 2, 8, 16 ])
        |> KeysSet.toStack ( intUp, identity )
        |> Stack.fold Down (\n soFar -> soFar - n)
    --> 5

-}
toStack :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever
         -> Emptiable (Stacked element) possiblyOrNever
        )
toStack key =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFill ->
                    keysSetFill
                        |> filled
                        |> foldFromOne key Stack.one Down Stack.onTopLay
                )


{-| Change each element based on its current value.

Runtime `n * log n` because mapped keys could be different (many other dicts/sets have runtime `n`)

If the keys of the mapped elements collide,
there's no promise of which element will be in the final mapped [`KeysSet`](#KeysSet)

-}
map :
    (element -> mappedElement)
    -> Keys mappedElement mappedKeys mappedKeyCount
    ->
        (Emptiable (KeysSet element keys_ (Add1 keyCountFrom1_)) possiblyOrNever
         -> Emptiable (KeysSet mappedElement mappedKeys mappedKeyCount) possiblyOrNever
        )
map elementChange mappedKeys =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFill ->
                    keysSetFill
                        |> filled
                        |> foldFromOneAnyOrder
                            (\a -> a |> elementChange |> one)
                            (\element_ soFar ->
                                soFar |> insertIfNoCollision mappedKeys (element_ |> elementChange)
                            )
                )


foldFromOneBy :
    (ArraySized (Tree2.Branch element) (Exactly (On keyCount))
     -> Tree2.Branch element
    )
    -> (element -> folded)
    -> Linear.Direction
    -> (element -> (folded -> folded))
    ->
        (Emptiable (KeysSet element keys_ keyCount) Never
         -> folded
        )
foldFromOneBy keyElement startToInitialFolded direction reduce =
    \keysSet ->
        keysSet
            |> fill
            |> KeysSet.Internal.treeForElement keyElement
            |> Tree2.foldFromOne startToInitialFolded direction reduce



-- combine


foldFromOneAnyOrder :
    (element -> folded)
    -> (element -> (folded -> folded))
    ->
        (Emptiable (KeysSet element keys_ (Add1 keyCountFrom1_)) Never
         -> folded
        )
foldFromOneAnyOrder startToInitialFolded reduce =
    \keysSet ->
        keysSet
            |> foldFromOneBy
                (ArraySized.element ( Up, n1 ))
                startToInitialFolded
                Up
                reduce


{-| Since there is no key provided, you can't rely on a specific order.
Use [`foldFrom`](#foldFrom) to use a specific order.

**Should not be exposed!**

-}
foldFromAnyOrder :
    folded
    -> (element -> (folded -> folded))
    ->
        (Emptiable (KeysSet element keys_ keyCount_) possiblyOrNever_
         -> folded
        )
foldFromAnyOrder initial reduce =
    \keysSet ->
        keysSet
            |> Emptiable.emptyAdapt (\_ -> Possible)
            |> Emptiable.mapFlat KeysSet.Internal.treeForAnyElementTry
            |> Tree2.foldFrom initial Up reduce


{-| Try to change each element based on its current value.

Runtime `n * log n` just like [`KeysSet.map`](#map) because mapped keys could be different

If the keys of the mapped elements collide,
there's no promise of which element will be in the final mapped [`KeysSet`](#KeysSet)

    {-| Keep only elements that pass a given test.
    Often called "filter"
    -}
    when orderKey isGood =
        KeysSet.fillsMap
            (\element ->
                if element |> isGood then
                    Just element

                else
                    Nothing
            )
            orderKey

-}
fillsMap :
    (element -> Emptiable mappedElement mappedElementPossiblyOrNever_)
    -> Keys mappedElement mappedKeys mappedKeyCount
    ->
        (Emptiable (KeysSet element keys_ lastIndex_) possiblyOrNever_
         -> Emptiable (KeysSet mappedElement mappedKeys mappedKeyCount) Possibly
        )
fillsMap elementChangeTry mappedKeys =
    \keysSet ->
        keysSet
            |> foldFromAnyOrder Emptiable.empty
                (\element_ ->
                    case element_ |> elementChangeTry of
                        Empty _ ->
                            identity

                        Filled elementMapped ->
                            \soFar ->
                                soFar
                                    |> insertIfNoCollision mappedKeys elementMapped
                )


{-| Fold, starting from one end element transformed to the initial accumulation value,
then reducing what's accumulated
in a given [`Direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Linear#Direction)

    import Linear exposing (Direction(..))
    import Stack
    import Int.Order
    import Keys
    import KeysSet

    KeysSet.fromStack intUp
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeysSet.foldFromOne ( intUp, identity )
            Stack.one
            Up
            Stack.onTopLay
    --> Stack.topBelow 543 [ 345, 234 ]

    intUp : Keys.Identity Int Int.Order.Up
    intUp =
        Keys.identity Int.Order.up

-}
foldFromOne :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    -> (element -> folded)
    -> Linear.Direction
    -> (element -> (folded -> folded))
    ->
        (Emptiable (KeysSet element keys_ keyCount) Never
         -> folded
        )
foldFromOne key startToInitialFolded direction reduce =
    \keysSet ->
        keysSet
            |> foldFromOneBy (key |> keyElement) startToInitialFolded direction reduce


{-| Fold over its elements from an initial accumulator value
in a given [`Direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Linear#Direction)

    import Linear exposing (Direction(..))
    import Stack
    import Int.Order
    import KeysSet
    import Keys

    KeysSet.fromStack intUp
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeysSet.foldFrom ( intUp, identity ) [] Down (::)
    --> [ 234, 345, 543]

    KeysSet.fromStack intUp
        (Stack.topBelow 5 [ 7, -6 ])
        |> KeysSet.foldFrom ( intUp, identity ) 0 Up (+)
    --> 6

    intUp : Keys.Identity Int Int.Order.Up
    intUp =
        Keys.identity Int.Order.up

-}
foldFrom :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    -> folded
    -> Linear.Direction
    -> (element -> (folded -> folded))
    ->
        (Emptiable (KeysSet element keys_ keyCount) possiblyOrNever_
         -> folded
        )
foldFrom key initial direction reduce =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFill ->
                    keysSetFill |> KeysSet.Internal.treeForElement (key |> keyElement)
                )
            |> Tree2.foldFrom initial direction reduce


{-| [`foldFrom`](#foldFrom) with the ability to stop early once
a given reduce function returns a [`Complete`](https://dark.elm.dmy.fr/packages/lue-bird/elm-partial-or-complete/latest/PartialOrComplete#PartialOrComplete) value.

    import Linear exposing (Direction(..))
    import Stack
    import Int.Order
    import KeysSet
    import Keys
    -- from lue-bird/partial-or-complete
    import PartialOrComplete exposing (PartialOrComplete(..))

    KeysSet.fromList intUp [ 11, 21, 31, 41, 51 ]
        -- do we have a sum >= 100?
        |> KeysSet.foldUntilCompleteFrom ( intUp, identity )
            0
            Up
            (\n sumSoFar ->
                if sumSoFar >= 100 then
                    -- no need to sum the rest!
                    () |> Complete
                else
                    sumSoFar + n |> Partial
            )
        |> PartialOrComplete.isComplete
    --> True


    intUp : Keys.Identity Int Int.Order.Up
    intUp =
        Keys.identity Int.Order.up

-}
foldUntilCompleteFrom :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    -> folded
    -> Linear.Direction
    -> (element -> (folded -> PartialOrComplete folded complete))
    ->
        (Emptiable (KeysSet element keys_ keyCount) possiblyOrNever_
         -> PartialOrComplete folded complete
        )
foldUntilCompleteFrom key initialFolded direction reduceStep =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFill ->
                    keysSetFill |> KeysSet.Internal.treeForElement (key |> keyElement)
                )
            |> Emptiable.emptyAdapt (\_ -> Possible)
            |> Tree2.foldUntilCompleteFrom initialFolded direction reduceStep


{-| [`foldFromOne`](#foldFromOne) with the ability to stop early once
a given reduce function returns a [`Complete`](https://dark.elm.dmy.fr/packages/lue-bird/elm-partial-or-complete/latest/PartialOrComplete#PartialOrComplete) value.

    import Linear exposing (Direction(..))
    import Stack
    import Int.Order
    import KeysSet
    import Keys
    -- from lue-bird/partial-or-complete
    import PartialOrComplete exposing (PartialOrComplete(..))

    KeysSet.fromStack intUp
        (Stack.topBelow 11 [ 21, 31, 41, 51 ])
        -- take the last 3
        |> KeysSet.foldUntilCompleteFromOne ( intUp, identity )
            (\lastElement ->
                { stack = lastElement |> Stack.one
                , length = 1
                }
                    |> Partial
            )
            Down
            (\element soFar ->
                if soFar.length >= 3 then
                    soFar.stack |> Complete
                else
                    { stack = soFar.stack |> Stack.onTopLay element
                    , length = soFar.length + 1
                    }
                        |> Partial
            )
        -- if we have less than 3 elements, that's fine, too
        |> PartialOrComplete.completeElseOnPartial .stack
    --> Stack.topBelow 31 [ 41, 51 ]


    intUp : Keys.Identity Int Int.Order.Up
    intUp =
        Keys.identity Int.Order.up

-}
foldUntilCompleteFromOne :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    -> (element -> PartialOrComplete folded complete)
    -> Linear.Direction
    -> (element -> (folded -> PartialOrComplete folded complete))
    ->
        (Emptiable (KeysSet element keys_ keyCount) Never
         -> PartialOrComplete folded complete
        )
foldUntilCompleteFromOne key startElementStep direction reduceStep =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFill ->
                    keysSetFill |> KeysSet.Internal.treeForElement (key |> keyElement)
                )
            |> Tree2.foldUntilCompleteFromOne startElementStep direction reduceStep


{-| Combine with another [`KeysSet`](#KeysSet).
On key collision, keep the current [`KeysSet`](#KeysSet)'s element.

(To instead replace current elements with incoming elements, swap the arguments)

-}
unifyWith :
    Keys element keys keyCount
    -> Emptiable (KeysSet element keys keyCount) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever
         -> Emptiable (KeysSet element keys keyCount) possiblyOrNever
        )
unifyWith orderKey toCombineWith =
    \keysSet ->
        toCombineWith
            |> foldFromAnyOrder keysSet
                (\upElement soFar ->
                    soFar |> insertIfNoCollision orderKey upElement
                )


{-| Keep each element whose key also appears in a given [`KeysSet`](#KeysSet)
-}
intersect :
    ( Keys element keys keyCount
    , keys -> Key element key_ by_ keyCount
    )
    -> Emptiable (KeysSet element keys keyCount) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever_
         -> Emptiable (KeysSet element keys keyCount) Possibly
        )
intersect ( keys, key ) toIntersectWith =
    \keysSet ->
        keysSet
            |> foldFromAnyOrder
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

    import Character

    KeysSet.fromList Character.keys
        [ { id = 0, char = 'A' }
        , { id = 1, char = 'B' }
        , { id = 2, char = 'c' }
        , { id = 3, char = 'd' }
        ]
        |> KeysSet.except ( Character.keys, .id )
            (KeysSet.fromList Character.keys
                [ { id = 2, char = 'c' }
                , { id = 3, char = 'd' }
                , { id = 4, char = 'e' }
                , { id = 5, char = 'f' }
                ]
                |> KeysSet.toKeys ( Character.keys, .id )
            )
        |> KeysSet.toList ( Character.keys, .id )
    --> [ { id = 0, char = 'A' }
    --> , { id = 1, char = 'B' }
    --> ]

-}
except :
    ( Keys element keys keyCount
    , keys -> Key element by_ key keyCount
    )
    -> Emptiable (KeysSet key incomingKeys_ incomingKeyCount_) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys keyCount) possiblyOrNever_
         -> Emptiable (KeysSet element keys keyCount) Possibly
        )
except ( keys, key ) toExcludeKeys =
    \keysSet ->
        toExcludeKeys
            |> foldFromAnyOrder
                (keysSet |> emptyAdapt (\_ -> Possible))
                (\keyToExclude diffSoFar ->
                    diffSoFar |> remove ( keys, key ) keyToExclude
                )


{-| Most powerful way of combining 2 [`KeysSet`](#KeysSet)s

Traverses all the keys from both [`KeysSet`](#KeysSet)s _from lowest to highest_,
accumulating whatever you want
for when a key appears in the
first [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)
second [`KeysSet`](#KeysSet).

You will find this as "merge" in most other dictionaries/sets,
except that you have the [diff as a value](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)
you can reduce with instead of separate functions for "only first", "only second" and "both".

To handle the [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)
cases, use a `case..of` or the [helpers in `elm-and-or`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)

-}
fold2From :
    folded
    ->
        (AndOr firstElement secondElement
         -> (folded -> folded)
        )
    ->
        (And
            { key :
                ( Keys firstElement firstKeys firstKeyCount
                , firstKeys -> Key firstElement firstBy_ key firstKeyCount
                )
            , set :
                Emptiable
                    (KeysSet firstElement firstKeys firstKeyCount)
                    firstPossiblyOrNever_
            }
            { key :
                ( Keys secondElement secondKeys secondKeyCount
                , secondKeys -> Key secondElement secondBy_ key secondKeyCount
                )
            , set :
                Emptiable
                    (KeysSet secondElement secondKeys secondKeyCount)
                    secondPossiblyOrNever_
            }
         -> folded
        )
fold2From initial reduce =
    \( first, second ) ->
        ( { tree =
                first.set
                    |> Emptiable.mapFlat
                        (KeysSet.Internal.treeForElement (Keys.Internal.keyElement first.key))
          , key = Keys.toKeyWith first.key
          }
        , { tree =
                second.set
                    |> Emptiable.mapFlat
                        (KeysSet.Internal.treeForElement (Keys.Internal.keyElement second.key))
          , key = Keys.toKeyWith second.key
          }
        )
            |> Tree2.Sorted.fold2From
                (Keys.keyOrderWith first.key)
                initial
                reduce


{-| Most powerful way of combining 2 [`KeysSet`](#KeysSet)s

Traverses all the keys from both [`KeysSet`](#KeysSet)s _from lowest to highest_,
accumulating whatever you want
for when a key appears in the
first [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)
second [`KeysSet`](#KeysSet).

You will find this as "merge" in most other dictionaries/sets,
except that you have the [diff as a value](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)
you can reduce with instead of separate functions for "only first", "only second" and "both".

To handle the [`AndOr`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)
cases, use a `case..of` or the [helpers in `elm-and-or`](https://dark.elm.dmy.fr/packages/lue-bird/elm-and-or/latest/AndOr)

-}
fold2FromOne :
    (AndOr firstElement secondElement -> folded)
    ->
        (AndOr firstElement secondElement
         -> (folded -> folded)
        )
    ->
        (And
            { key :
                ( Keys firstElement firstKeys firstKeyCountFrom1
                , firstKeys -> Key firstElement firstBy_ key firstKeyCountFrom1
                )
            , set :
                Emptiable
                    (KeysSet firstElement firstKeys firstKeyCountFrom1)
                    Never
            }
            { key :
                ( Keys secondElement secondKeys secondKeyCount
                , secondKeys -> Key secondElement secondBy_ key secondKeyCount
                )
            , set :
                Emptiable
                    (KeysSet secondElement secondKeys secondKeyCount)
                    secondPossiblyOrNever_
            }
         -> folded
        )
fold2FromOne startToInitialFolded reduce ( first, second ) =
    Tree2.Sorted.fold2FromOne
        (Keys.keyOrderWith first.key)
        startToInitialFolded
        reduce
        ( { tree =
                first.set
                    |> Emptiable.mapFlat
                        (KeysSet.Internal.treeForElement (Keys.Internal.keyElement first.key))
          , key = Keys.toKeyWith first.key
          }
        , { tree =
                second.set
                    |> Emptiable.mapFlat
                        (KeysSet.Internal.treeForElement (Keys.Internal.keyElement second.key))
          , key = Keys.toKeyWith second.key
          }
        )
