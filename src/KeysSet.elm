module KeysSet exposing
    ( KeysSet
    , one
    , fromStack, fromList
    , size, element, end
    , insertIfNoCollision, insertReplacingCollisions
    , remove
    , elementAlterIfNoCollision, elementAlterReplacingCollisions
    , map, mapTry
    , unifyWith, except, intersect
    , fold2From, FirstAndOrSecond(..)
    , toKeys, toStack, toList
    , foldFrom, fold, foldFromOne
    )

{-| Lookup with multiple [`Keys`](Keys#Keys)

@docs KeysSet


## create

[`Emptiable.empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty),

@docs one
@docs fromStack, fromList


## scan

@docs size, element, end


## alter

@docs insertIfNoCollision, insertReplacingCollisions
@docs remove
@docs elementAlterIfNoCollision, elementAlterReplacingCollisions
@docs map, mapTry


## combine

@docs unifyWith, except, intersect
@docs fold2From, FirstAndOrSecond


## transform

@docs toKeys, toStack, toList
@docs foldFrom, fold, foldFromOne

-}

import ArraySized
import Emptiable exposing (Emptiable(..), emptyAdapt, fill, filled)
import Keys exposing (Key, Keys, keyIndex, keyOrderWith, toKeyWith)
import KeysSet.Internal
import Linear exposing (Direction(..))
import List.Linear
import Map
import N exposing (N0, To, Up, n0)
import Order
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Tree2
import Tree2.Sorted
import Typed


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

    import Char.Order
    import Keys exposing (Key, Keys)
    import KeysSet
    import Map exposing (Mapping)
    import Order
    import String.Order

    type User
        = User { name : String, age : Int, height : Float }

    type Name
        = Name

    name : Mapping User Name String
    name =
        Typed.tag Data (\(User userData) -> userData.name)

    type alias ByName =
        { name :
            Key
                User
                (Order.By
                    Name
                    (String.Order.Earlier
                        (Char.Order.Alphabetically Char.Order.LowerUpper)
                    )
                )
                String
                (Up N0 To N0)
        }

    byName : Keys User ByName N0
    byName =
        Keys.for (\name -> { name = name })
            |> Keys.by ( identity, Map.identity )
                (String.Order.earlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
                )

  - [`element`](#element), [`end`](#end), [insert](#insertIfNoCollision) versions, [elementAlter](#elementAlterIfNoCollision) versions, [`remove`](#remove) are runtime `log n`
  - [`toList`](#toList), [`toStack`](#toStack), [`toKeys`](#toKeys), [`foldFrom`](#foldFrom), [`fold`](#fold), [`foldFromOne`](#foldFromOne) are runtime `n`

-}
type alias KeysSet element keys lastIndex =
    KeysSet.Internal.KeysSet element keys lastIndex


{-| [`KeysSet`](#KeysSet) containing a single given element.

You don't need to provide [`Keys`](Keys#Keys)

-}
one : element -> Emptiable (KeysSet element keys_ lastIndex_) never_
one singleElement =
    KeysSet.Internal.one singleElement |> filled


{-| Convert to a [`KeysSet`](#KeysSet),
⚠️ ignoring elements whose keys already exist **earlier** in the `List`

For construction, use [`fromStack`](#fromStack),
proving to the compiler what you already know about its (non-)emptiness

-}
fromList :
    Keys element keys lastIndex
    -> List element
    -> Emptiable (KeysSet element keys lastIndex) Possibly
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
            , { handle = "cr", shown = "creeper" }
            ]
        )

-}
fromStack :
    Keys element keys lastIndex
    -> Emptiable (Stacked element) possiblyOrNever
    -> Emptiable (KeysSet element keys lastIndex) possiblyOrNever
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
size : Emptiable (KeysSet element_ keys_ lastIndex_) possiblyOrNever_ -> Int
size =
    \keysSet ->
        case keysSet of
            Emptiable.Empty _ ->
                0

            Emptiable.Filled keysSetFill ->
                keysSetFill |> KeysSet.Internal.size


tree :
    ( Keys element keys lastIndex
    , keys -> Key element key_ by_ (Up indexToLast_ To lastIndex)
    )
    ->
        (KeysSet element keys lastIndex
         -> Emptiable (Tree2.Branch element) never_
        )
tree key =
    \keysSet ->
        keysSet |> KeysSet.Internal.treeForIndex (key |> keyIndex)


{-| Access the element associated with a given key.
If no element with the given key is not present, `Emptiable.empty`

    import Emptiable exposing (Emptiable, filled)
    import Typed
    import Stack
    import N exposing (Up, To, N0)
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
                        (Char.Order.Alphabetically Char.Order.LowerUpper)
                    )
                )
                String
                (Up N0 To N0)
        }

    animals : Emptiable (KeysSet Animal ByName N0) never_
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
        Typed.tag Name .name

    animalByName : Keys Animal ByName N0
    animalByName =
        Keys.for (\name_ -> { name = name_ })
            |> Keys.by ( .name, name )
                (String.Order.earlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
                )

    animals |> KeysSet.element ( animalByName, .name ) "Tom" |> Emptiable.map .kind
    --> filled Cat

    animals |> KeysSet.element ( animalByName, .name ) "Jerry" |> Emptiable.map .kind
    --> filled Mouse

    animals |> KeysSet.element ( animalByName, .name ) "Spike" |> Emptiable.map .kind
    --> Emptiable.empty

-}
element :
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> key
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever_
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


{-| Get the element associated with a key at the end looking in a given `Direction`

  - minimum → `end Down`
  - maximum → `end Up`

[`KeysSet`](#KeysSet) is `empty` → Nothing

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable)
    import Typed
    import N exposing (Up, To, N0)
    import Stack
    import KeysSet exposing (KeysSet)
    import Map exposing (Mapping)
    import Order
    import Char.Order
    import String.Order
    import Keys exposing (Keys, Key)


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
                        (Char.Order.Alphabetically Char.Order.LowerUpper)
                    )
                )
                String
                (Up N0 To N0)
        }

    users : Emptiable (KeysSet User ByName N0) never_
    users =
        KeysSet.fromStack userByName
            (Stack.topBelow
                { name = "Bob", age = 19, height = 1.82 }
                [ { name = "Alice", age = 28, height = 1.65 }
                , { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

    type Name
        = Name -- not exposed

    name : Mapping User Name String
    name =
        Typed.tag Name .name

    userByName : Keys User ByName N0
    userByName =
        Keys.for (\name_ -> { name = name_ })
            |> Keys.by ( .name, name )
                (String.Order.earlier
                    (Char.Order.alphabetically Char.Order.lowerUpper)
                )

    users |> KeysSet.end ( userByName, .name ) Down
    --> { name = "Alice", age = 28, height = 1.65 }

    users |> KeysSet.end ( userByName, .name ) Up
    --> { name = "Chuck", age = 33, height = 1.75 }

Notice how we safely avoided returning a `Maybe`
through the use of [`Emptiable ... Never`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)

If you don't know whether the [`KeysSet`](#KeysSet) will be empty

    users
        |> Emptiable.map
            (\us -> us |> filled |> KeysSet.end ( userByName, .name ) Up)
    --: Emptiable element Possibly

-}
end :
    ( Keys element keys lastIndex
    , keys -> Key element key_ by_ (Up indexToLast_ To lastIndex)
    )
    -> Linear.Direction
    ->
        (Emptiable (KeysSet element keys lastIndex) Never
         -> element
        )
end ( keys, key ) direction =
    \keysSet ->
        keysSet
            |> Emptiable.fill
            |> tree ( keys, key )
            |> Tree2.end direction


fillInsertOnNoCollision :
    Keys element keys lastIndex
    -> element
    ->
        (KeysSet element keys lastIndex
         -> Emptiable (KeysSet element keys lastIndex) never_
        )
fillInsertOnNoCollision keys toInsert =
    let
        keyArray =
            keys
                |> Keys.toArray
                |> ArraySized.inToNumber
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
    Keys element keys lastKeyIndex
    -> element
    ->
        (KeysSet element keys lastKeyIndex
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
    Keys element keys lastIndex
    -> element
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element keys lastIndex) never_
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
                case collisions |> Emptiable.map filled of
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
    Keys element keys lastIndex
    -> element
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element keys lastIndex) never_
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
                case collisions |> Emptiable.map filled of
                    Emptiable.Empty _ ->
                        keysSetFill |> fillInsertOnNoCollision keys toInsertOrReplacement

                    Emptiable.Filled collisionsTreeFilled ->
                        case
                            keysSetFill
                                |> filled
                                |> exceptTree keys collisionsTreeFilled
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
    Keys element keys lastIndex
    -> Emptiable (Tree2.Branch element) exceptionsPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys lastIndex) Never
         -> Emptiable (KeysSet element keys lastIndex) Possibly
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
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> key
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element keys lastIndex) Possibly
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
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> key
    -> (element -> element)
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever
         -> Emptiable (KeysSet element keys lastIndex) possiblyOrNever
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
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> key
    -> (element -> element)
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever
         -> Emptiable (KeysSet element keys lastIndex) possiblyOrNever
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
    ( Keys element keys lastIndex
    , keys -> Key element (Order.By toKeyTag_ keyOrderTag) key (Up indexToLast_ To lastIndex)
    )
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever
         ->
            Emptiable
                (KeysSet key (Keys.Key key (Order.By Map.Identity keyOrderTag) key (Up N0 To N0)) N0)
                possiblyOrNever
        )
toKeys ( keys, key ) =
    \keysSet ->
        keysSet
            |> Emptiable.map
                (\keysSetFill ->
                    let
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

    import Linear exposing (Direction(..))
    import N exposing (Up, To, N0)
    import Stack
    import Keys exposing (Keys, Key)
    import KeysSet
    import Map
    import Order
    import Char.Order
    import String.Order

    nameAlphabetical : Keys.Identity String (String.Order.Earlier (Char.Order.Alphabetically Char.Order.LowerUpper))
    nameAlphabetical =
        Keys.identity
            (String.Order.earlier
                (Char.Order.alphabetically Char.Order.lowerUpper)
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
    ( Keys element keys lastIndex
    , keys -> Key element key_ by_ (Up indexToLast_ To lastIndex)
    )
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever_
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
    import N exposing (Up, To, N0)
    import Stack
    import Map
    import Order
    import Char.Order
    import String.Order
    import Keys exposing (Keys, Key)
    import KeysSet

    nameAlphabetical : Keys.Identity String (String.Order.Earlier (Char.Order.Alphabetically Char.Order.LowerUpper))
    nameAlphabetical =
        Keys.identity
            (String.Order.earlier
                (Char.Order.alphabetically Char.Order.lowerUpper)
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

    import Linear exposing (Direction(..))
    import N exposing (Up, To, N0)
    import Map
    import Stack
    import Order
    import Int.Order
    import Keys exposing (Keys, Key)
    import KeysSet

    intIncreasing : Keys.Identity Int Int.Order.Increasing
    intIncreasing =
        Keys.identity Int.Order.increasing

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 345 [ 234, 543 ])
        |> KeysSet.toStack ( intIncreasing, identity )
    --> Stack.topBelow 234 [ 345, 543 ]
    --  the type knows it's never empty

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 1 [ 2, 8, 16 ])
        |> KeysSet.toStack ( intIncreasing, identity )
        |> Stack.fold Down (\n soFar -> soFar - n)
    --> 5

-}
toStack :
    ( Keys element keys lastIndex
    , keys -> Key element key_ by_ (Up indexToLast_ To lastIndex)
    )
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever
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
    -> Keys mappedElement mappedKeys mappedLastIndex
    ->
        (Emptiable (KeysSet element keys_ lastIndex_) possiblyOrNever
         -> Emptiable (KeysSet mappedElement mappedKeys mappedLastIndex) possiblyOrNever
        )
map elementChange mappedKeys =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFilled ->
                    keysSetFilled
                        |> filled
                        |> foldFromOne
                            (\a -> a |> elementChange |> one)
                            (\element_ soFar ->
                                soFar |> insertIfNoCollision mappedKeys (element_ |> elementChange)
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
    -> Keys mappedElement mappedKeys mappedLastIndex
    ->
        (Emptiable (KeysSet element keys_ lastIndex_) possiblyOrNever_
         -> Emptiable (KeysSet mappedElement mappedKeys mappedLastIndex) Possibly
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
                                    |> insertIfNoCollision mappedKeys elementMapped
                )



-- combine


{-| Fold, starting from one end as the initial accumulation value,
then reducing what's accumulated

    import Stack
    import KeysSet
    import Int.Order

    intIncreasing : Keys.Identity Int Int.Order.Increasing
    intIncreasing =
        Keys.identity Int.Order.increasing

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 1 [ 2, 8, 16 ])
        |> KeysSet.fold (\n soFar -> soFar - n)
    -- no! you shouldn't rely on order with fold

-}
fold :
    (element -> (element -> element))
    ->
        (Emptiable (KeysSet element keys_ lastIndex_) Never
         -> element
        )
fold reduce =
    \keysSet ->
        keysSet |> foldFromOne identity reduce


{-| Fold, starting from one end element transformed to the initial accumulation value,
then reducing what's accumulated.

    import Stack
    import N exposing (Up, To, N0)
    import Map
    import Order
    import Int.Order
    import Keys exposing (Keys, Key)
    import KeysSet

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeysSet.foldFromOne Stack.one Stack.onTopLay
    --> Stack.topBelow 543 [ 345, 234 ]
    -- no! you shouldn't rely on order with fold
    -- the type knows it's never empty

    intIncreasing : Keys.Identity Int Int.Order.Increasing
    intIncreasing =
        Keys.identity Int.Order.increasing

A simpler version is [`fold`](#fold)

    KeysSet.fold =
        KeysSet.foldFromOne identity

-}
foldFromOne :
    (element -> accumulated)
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (KeysSet element keys_ lastIndex_) Never
         -> accumulated
        )
foldFromOne firstToInitial reduce =
    \keysSet ->
        keysSet
            |> fill
            |> KeysSet.Internal.treeForIndex n0
            |> Tree2.foldFromOne firstToInitial Up reduce


{-| Fold over its elements from an initial accumulator value

    import N exposing (Up, To, N0)
    import Stack
    import Map
    import Order
    import Int.Order
    import KeysSet
    import Keys exposing (Keys, Key)

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeysSet.foldFrom [] (::)
    -- no! you shouldn't rely on order with fold

    KeysSet.fromStack intIncreasing
        (Stack.topBelow 5 [ 7, -6 ])
        |> KeysSet.foldFrom 0 (+)
    --> 6

    intIncreasing : Keys.Identity Int Int.Order.Increasing
    intIncreasing =
        Keys.identity Int.Order.increasing

-}
foldFrom :
    accumulated
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (KeysSet element keys_ lastIndex_) possiblyOrNever_
         -> accumulated
        )
foldFrom initial reduce =
    \keysSet ->
        keysSet
            |> Emptiable.mapFlat
                (\keysSetFill ->
                    keysSetFill |> KeysSet.Internal.treeForIndex n0
                )
            |> Tree2.foldFrom initial Up reduce


{-| Combine with another [`KeysSet`](#KeysSet).
On key collision, keep the current [`KeysSet`](#KeysSet)'s element.

(To instead replace current elements with incoming elements, swap the arguments)

-}
unifyWith :
    Keys element keys lastIndex
    -> Emptiable (KeysSet element keys lastIndex) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever
         -> Emptiable (KeysSet element keys lastIndex) possiblyOrNever
        )
unifyWith orderKey toCombineWith =
    \keysSet ->
        toCombineWith
            |> foldFrom keysSet
                (\upElement soFar ->
                    soFar |> insertIfNoCollision orderKey upElement
                )


{-| Keep each element whose key also appears in a given [`KeysSet`](#KeysSet)
-}
intersect :
    ( Keys element keys lastIndex
    , keys -> Key element key_ by_ (Up indexToLast_ To lastIndex)
    )
    -> Emptiable (KeysSet element keys lastIndex) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element keys lastIndex) Possibly
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
    ( Keys element keys lastIndex
    , keys -> Key element by_ key (Up indexToLast_ To lastIndex)
    )
    -> Emptiable (KeysSet key incomingKeys_ incomingLastIndex_) incomingPossiblyOrNever_
    ->
        (Emptiable (KeysSet element keys lastIndex) possiblyOrNever_
         -> Emptiable (KeysSet element keys lastIndex) Possibly
        )
except ( keys, key ) toExcludeKeys =
    \keysSet ->
        toExcludeKeys
            |> foldFrom
                (keysSet |> emptyAdapt (\_ -> Possible))
                (\keyToExclude diffSoFar ->
                    diffSoFar |> remove ( keys, key ) keyToExclude
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
                ( Keys firstElement firstKeys firstLastIndex
                , firstKeys -> Key firstElement firstBy_ key (Up firstIndexToLastIndex_ To firstLastIndex)
                )
            , set :
                Emptiable
                    (KeysSet firstElement firstKeys firstLastIndex)
                    firstPossiblyOrNever_
            }
         , second :
            { key :
                ( Keys secondElement secondKeys secondLastIndex
                , secondKeys -> Key secondElement secondBy_ key (Up secondIndexToLastIndex_ To secondLastIndex)
                )
            , set :
                Emptiable
                    (KeysSet secondElement secondKeys secondLastIndex)
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
