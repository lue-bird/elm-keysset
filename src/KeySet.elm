module KeySet exposing
    ( KeySet
    , Sorting, sortingKey
    , only, fromStack, fromList
    , size, element, end
    , insert, elementRemove, elementAlter
    , map, mapTry
    , unifyWith, except, intersect
    , fold2From, FirstOrSecondOrBoth(..)
    , toList
    , foldFrom, foldOnto, fold
    )

{-| Lookup with one key

@docs KeySet
@docs Sorting, sortingKey


## create

[`Emptiable.empty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#empty),

@docs only, fromStack, fromList


## scan

@docs size, element, end


## alter

@docs insert, elementRemove, elementAlter
@docs map, mapTry


## combine

@docs unifyWith, except, intersect
@docs fold2From, FirstOrSecondOrBoth


## transform

@docs toList
@docs foldFrom, foldOnto, fold

-}

import Emptiable exposing (Emptiable(..), emptyAdapt, fillMap, fillMapFlat, filled)
import KeySet.Internal
import Linear exposing (Direction(..))
import List.Linear
import Order exposing (Ordering)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Tree2
import Typed exposing (Checked, Public, Typed)


{-| 🗃️ Non-empty AVL-tree-based look-up. Elements and keys can be of any type

[`insert`](#insert), [`elementRemove`](#elementRemove), [`element`](#element) are runtime `log n`

    import KeySet exposing (KeySet)
    import Stack
    import User

    users : Emptiable (KeySet User User.ByName) never_
    users =
        KeySet.fromStack User.byName
            (Stack.topBelow
                { name = "Alice", age = 28, height = 1.65 }
                [ { name = "Bob", age = 19, height = 1.82 }
                , { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

where

    module User exposing (ByName, User(..), byName)

    import KeySet
    import Order

    type User
        = User { name : String, email : Email }

    type ByName
        = ByName

    byName : KeySet.Sorting User String ByName
    byName =
        KeySet.sortingKey .name
            { tag = ByName
            , order = String.Order.greaterEarlier (Char.Order.alphabetically Order.lowerUpper)
            }

-}
type alias KeySet element tag =
    KeySet.Internal.KeySet element tag


{-| Configure what's considered a key inside a [`KeySet`](#KeySet)

Create using [`sortingKey`](#sortingKey), as shown in

  - [`KeySet` example](#KeySet)
  - [readme example](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/#KeySet)

-}
type alias Sorting element key tag =
    Typed
        Checked
        tag
        Public
        { key : element -> key
        , keyOrder :
            Ordering key
        }


{-| By which aspect = key and in which key `Order` the elements are [sorted](#Sorting)

For example, to create a [`Sorting`](#Sorting) to use with a simple set, use

    sortingKey identity ..tag..order..

more in

  - [`KeySet` example](#KeySet)
  - [readme example](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/#KeySet)

-}
sortingKey :
    (element -> key)
    ->
        { tag : tag
        , order : Ordering key
        }
    -> Sorting element key tag
sortingKey elementKey keySorting =
    { key = elementKey
    , keyOrder = keySorting.order
    }
        |> Typed.tag keySorting.tag
        |> Typed.isChecked keySorting.tag



-- create


{-| [`KeysSet`](#KeySet) containing a single given element
-}
only : element -> Emptiable (KeySet element tag_) never_
only singleElement =
    KeySet.Internal.only singleElement


{-| Convert to a [`KeysSet`](#KeySet),
ignoring elements whose keys already exist **earlier** in the `List`

For construction, use [`fromStack`](#fromStack),
proving to the compiler what you already know

-}
fromList :
    Sorting element key_ tag
    -> List element
    -> Emptiable (KeySet element tag) Possibly
fromList sorting =
    \list ->
        list
            |> List.foldr
                (\el soFar -> soFar |> insert sorting el)
                Emptiable.empty


{-| Convert to a [`KeysSet`](#KeySet),
ignoring elements whose keys already exist **earlier** in the [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)

    Keyset.fromStack User.ByHandle
        (Stack.topBelow
            { handle = "cr", shown = "chris \\o/" }
            [ { handle = "ann", shown = "Ann in stand by" }
            , -- ↓ won't be part of the KeySet
              { handle = "cr", shown = "creeper" }
            ]
        )

-}
fromStack :
    Sorting element key_ tag
    -> Emptiable (Stacked element) possiblyOrNever
    -> Emptiable (KeySet element tag) possiblyOrNever
fromStack sorting =
    \stack ->
        stack
            |> fillMapFlat
                (\stacked ->
                    stacked
                        |> filled
                        |> Stack.foldOnto only
                            Down
                            (\el soFar -> soFar |> insert sorting el)
                )



-- scan


{-| Number of elements in the [`KeysSet`](#KeySet).
Runtime `1`.

    KeySet.only "Hello"
        |> KeySet.size
    --> 1

-}
size : Emptiable (KeySet element_ tag_) possiblyOrNever_ -> Int
size =
    \keySet ->
        keySet |> KeySet.Internal.size


tree :
    Emptiable (KeySet element tag_) possiblyOrNever
    -> Emptiable (Tree2.Branch element) possiblyOrNever
tree =
    \keySet -> keySet |> KeySet.Internal.tree


{-| Get the value associated with a key. If the key is not found, return Nothing.
This is useful when you are not sure if a key will be in the [`KeysSet`](#KeySet).

    import KeySet exposing (KeySet)
    import Emptiable exposing (Emptiable, filled)

    type Animal
        = Mouse
        | Cat

    animals : Emptiable (KeySet String { byName : () }) never_
    animals =
        KeySet.fromStack animalByName
            (Stack.topBelow
                { name = "Tom", kind = Cat }
                [ { name = "Jerry", kind = Mouse }
                ]
            )

    -- should be opaque and should use an opaque tag
    animalByName : KeySet.Sorting Animal String { byName : () }
    animalByName =
        KeySet.sortingKey .name
            { tag = { byName = () }
            , order =
                String.Order.greaterEarlier (Char.Order.alphabetically Order.lowerUpper)
            }

    animals |> KeySet.element animalByName "Tom"
    --> filled Cat

    animals |> KeySet.element animalByName "Jerry"
    --> filled Mouse

    animals |> KeySet.element animalByName "Spike"
    --> Emptiable.empty

-}
element :
    Sorting element key tag
    -> key
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable element Possibly
        )
element sorting keyToAccess =
    \keySet ->
        keySet
            |> tree
            |> emptyAdapt (\_ -> Possible)
            |> treeElement (sorting |> Typed.untag) keyToAccess


treeElement :
    { key : element -> key
    , keyOrder : Ordering key
    }
    -> key
    ->
        (Emptiable (Tree2.Branch element) Possibly
         -> Emptiable element Possibly
        )
treeElement sorting key =
    \tree_ ->
        tree_
            |> fillMap filled
            |> fillMapFlat
                (\treeFilled ->
                    case sorting.keyOrder key (treeFilled |> Tree2.trunk |> sorting.key) of
                        LT ->
                            treeFilled |> Tree2.children |> .left |> treeElement sorting key

                        GT ->
                            treeFilled |> Tree2.children |> .right |> treeElement sorting key

                        EQ ->
                            treeFilled |> Tree2.trunk |> filled
                )


{-| Get the element associated with a key at the end looking in a given `Direction`

  - minimum → `end Down`
  - maximum → `end Up`

The [`KeySet`](#KeySet) is `empty` → Nothing

    import KeySet exposing (KeySet)

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

    users : Emptiable (KeySetFilled User User.ByName) never_
    users =
        KeySet.fromStack User.byName
            (Stack.topBelow
                { name = "Bob", age = 19, height = 1.82 }
                [ { name = "Alice", age = 28, height = 1.65 }
                , { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

    users |> KeySet.end Down
    --> { name = "Alice", age = 28, height = 1.65 }

    users |> KeySet.end Up
    --> { name = "Chuck", age = 33, height = 1.75 }

Notice how we safely avoided returning a `Maybe`
through the use of [`Emptiable ... Never`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)

If you don't know whether the [`KeySet`](#KeySet) will be empty

    users
        |> fillMap (\us -> us |> filled |> KeySet.end Up)
    --: Emptiable element Possibly

-}
end :
    Linear.Direction
    ->
        (Emptiable (KeySet element tag_) Never
         -> element
        )
end direction =
    \keySet ->
        keySet |> tree |> Tree2.end direction



-- alter


{-| Insert a given element.
Replaces value when there is a collision.
-}
insert :
    Sorting element key_ tag
    -> element
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) never_
        )
insert sorting elementToInsert =
    \keySet ->
        keySet |> KeySet.Internal.insert sorting elementToInsert


{-| Remove its element whose key matches the given one.
If the key is not found, no changes are made.
-}
elementRemove :
    Sorting element key tag
    -> key
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) Possibly
        )
elementRemove sorting keyToRemove =
    \keySet ->
        keySet
            |> KeySet.Internal.elementRemove sorting keyToRemove


{-| Change the element for a specific key with a given function.
-}
elementAlter :
    Sorting element key tag
    -> key
    ->
        (Emptiable element Possibly
         -> Emptiable element elementAlteredEmptyPossiblyOrNever
        )
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) elementAlteredEmptyPossiblyOrNever
        )
elementAlter sorting keyToAlter emptiableElementTryMap =
    \keySet ->
        case keySet |> element sorting keyToAlter of
            Empty _ ->
                case Emptiable.empty |> emptiableElementTryMap of
                    Empty emptiable ->
                        keySet |> emptyAdapt (\_ -> emptiable)

                    Filled elementNew ->
                        keySet |> insert sorting elementNew

            Filled elementToAlter ->
                case elementToAlter |> filled |> emptiableElementTryMap of
                    Empty emptiable ->
                        keySet
                            |> elementRemove sorting keyToAlter
                            |> emptyAdapt (\_ -> emptiable)

                    Filled elementAltered ->
                        keySet |> insert sorting elementAltered



-- transform


{-| Convert to a `List` sorted by keys
in a given direction

    import Stack
    import KeySet
    import Case
    import Char.Order
    import String.Order

    -- tag should be opaque in a separate module
    nameAlphabetical : KeySet.Sorting String String { alphabetical : () }
    nameAlphabetical =
        KeySet.sortingKey identity
            { tag = { alphabetical = () }
            , order = String.Order.greaterEarlier (Char.Order.alphabetical Case.lowerUpper)
            }

    KeySet.fromList nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeySet.toList Up
    --> [ "Alice", "Bob" ]

    KeySet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeySet.toList Down
    --> [ "Bob", "Alice" ]

-}
toList :
    Linear.Direction
    ->
        (Emptiable (KeySet element tag_) possiblyOrNever_
         -> List element
        )
toList direction =
    \keySet ->
        keySet
            |> foldFrom []
                (direction |> Linear.opposite)
                (\element_ soFar -> soFar |> (::) element_)


{-| Change each element based on its current value.
Runtime `n * log n` because keys could be different (most other keySets have runtime `n`)
-}
map :
    (element -> mappedElement)
    -> Sorting mappedElement mappedKey_ mappedTag
    ->
        (Emptiable (KeySet element tag_) possiblyOrNever
         -> Emptiable (KeySet mappedElement mappedTag) possiblyOrNever
        )
map elementChange mappedKey =
    \keySet ->
        keySet
            |> fillMapFlat
                (\keySetFilled ->
                    keySetFilled
                        |> filled
                        |> foldOnto (\end_ -> end_ |> elementChange |> only)
                            Up
                            (\element_ soFar ->
                                soFar |> insert mappedKey (element_ |> elementChange)
                            )
                )


{-| Try to change each element based on its current value.
Runtime `n * log n` just like [`KeySet.map`](#map) because keys could be different

    {-| Keep only elements that pass a given test.
    Often called "filter"
    -}
    when sorting isGood =
        KeySet.mapTry
            (\element ->
                if element |> isGood then
                    Just element

                else
                    Nothing
            )
            sorting

-}
mapTry :
    (element -> Emptiable mappedElement possiblyOrNever)
    -> Sorting mappedElement mappedKey_ mappedTag
    ->
        (Emptiable (KeySet element tag_) possiblyOrNever
         -> Emptiable (KeySet mappedElement mappedTag) Possibly
        )
mapTry elementChangeTry mappedKey =
    \keySet ->
        keySet
            |> foldFrom Emptiable.empty
                Up
                (\element_ ->
                    case element_ |> elementChangeTry of
                        Empty _ ->
                            identity

                        Filled elementMapped ->
                            \soFar -> soFar |> insert mappedKey elementMapped
                )


{-| Fold, starting from one end as the initial accumulation value,
then reducing what's accumulated in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import KeySet
    import Int.Order

    KeySet.fromStack
        (KeySet.sorting identity -- tag should be opaque instead
            { tag = { increasing = () }, order = Int.Order.increasing }
        )
        (Stack.topBelow 4 [ 2, 8, 4 ])
        |> KeySet.fold Up (/)
    --> 0.0625

-}
fold :
    Linear.Direction
    -> (element -> (element -> element))
    ->
        (Emptiable (KeySet element tag_) Never
         -> element
        )
fold direction reduce =
    \keySet ->
        keySet |> foldOnto identity direction reduce


{-| Fold, starting from one end element transformed to the initial accumulation value,
then reducing what's accumulated in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import Stack exposing (topBelow)

    topBelow 234 [ 345, 543 ]
        |> Stack.foldOnto Stack.only Down Stack.onTopLay

A simpler version is

    Stack.fold =
        Stack.foldOnto identity

-}
foldOnto :
    (element -> accumulated)
    -> Linear.Direction
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (KeySet element tag_) Never
         -> accumulated
        )
foldOnto firstToInitial direction reduce =
    \keySet ->
        keySet
            |> tree
            |> Tree2.foldOnto firstToInitial direction reduce


{-| Fold over its elements from an initial accumulator value either

  - `Up` from lowest key to highest
  - `Down` from highest key to lowest

-}
foldFrom :
    accumulated
    -> Linear.Direction
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (KeySet element tag_) possiblyOrNever_
         -> accumulated
        )
foldFrom initial direction reduce =
    \keySet ->
        keySet
            |> tree
            |> Tree2.foldFrom initial direction reduce



-- combine


{-| Combine two.
On key collision, keep the current [`KeySet`](#KeySet)'s element
-}
unifyWith :
    Sorting element key_ tag
    -> Emptiable (KeySet element tag) incomingPossiblyOrNever_
    ->
        (Emptiable (KeySet element tag) possiblyOrNever
         -> Emptiable (KeySet element tag) possiblyOrNever
        )
unifyWith sorting toCombineWith =
    \keySet ->
        toCombineWith
            |> foldFrom keySet
                Up
                (\rightElement soFar ->
                    soFar |> insert sorting rightElement
                )


{-| Keep each element whose key also appears in a given [`KeySet`](#KeySet)
-}
intersect :
    Sorting element key_ tag
    -> Emptiable (KeySet element tag) incomingPossiblyOrNever_
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) Possibly
        )
intersect sorting toIntersectWith =
    \keySet ->
        let
            keySetEmptiablePossibly =
                keySet |> emptyAdapt (\_ -> Possible)
        in
        toIntersectWith
            |> emptyAdapt (\_ -> Possible)
            |> mapTry
                (\element_ ->
                    keySetEmptiablePossibly
                        |> element sorting
                            (element_ |> (sorting |> Typed.untag |> .key))
                )
                sorting


{-| Keep only those elements whose keys don't appear in the given [`KeySet`](#KeySet)
-}
except :
    Sorting element key_ tag
    -> Emptiable (KeySet element tag) incomingPossiblyOrNever_
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) Possibly
        )
except sorting toExclude =
    \keySet ->
        toExclude
            |> foldFrom (keySet |> emptyAdapt (\_ -> Possible))
                Up
                (\element_ diffSoFar ->
                    diffSoFar
                        |> elementRemove sorting
                            (element_ |> (sorting |> Typed.untag |> .key))
                )


{-| Unresolved element state while merging a "first" and "second" structure

  - only in the `First` structure
  - only in the `Second` structure
  - in both `FirstSecond`

`FirstSecond` has a tuple instead of a more descriptive record to make matching easier

-}
type FirstOrSecondOrBoth first second
    = First first
    | Second second
    | FirstSecond ( first, second )


{-| Most powerful way of combining 2 [`KeySet`](#KeySet)s

Traverses all the keys from both [`KeySet`](#KeySet)s _from lowest to highest_,
accumulating whatever you want
for when a key appears in either the [`FirstOrSecondOrBoth`](#FirstOrSecondOrBoth)
of the [`KeySet`](#KeySet)s

You will find this as "merge" in most other dictionaries/sets

-}
fold2From :
    accumulated
    ->
        (FirstOrSecondOrBoth firstElement secondElement
         -> (accumulated -> accumulated)
        )
    ->
        ({ first :
            { sorting : Sorting firstElement key firstTag
            , set : Emptiable (KeySet firstElement firstTag) firstPossiblyOrNever_
            }
         , second :
            { sorting : Sorting secondElement key secondTag
            , set : Emptiable (KeySet secondElement secondTag) secondPossiblyOrNever_
            }
         }
         -> accumulated
        )
fold2From initial reduce { first, second } =
    let
        secondAccumulate :
            firstElement
            ->
                ({ list : List secondElement, accumulated : accumulated }
                 -> { list : List secondElement, accumulated : accumulated }
                )
        secondAccumulate firstElement =
            \soFar ->
                case soFar.list of
                    [] ->
                        { list = []
                        , accumulated =
                            soFar.accumulated |> reduce (First firstElement)
                        }

                    secondElement :: secondRest ->
                        case
                            (first.sorting |> Typed.untag |> .keyOrder)
                                (secondElement |> (second.sorting |> Typed.untag |> .key))
                                (firstElement |> (first.sorting |> Typed.untag |> .key))
                        of
                            LT ->
                                secondAccumulate firstElement
                                    { list = secondRest
                                    , accumulated =
                                        soFar.accumulated |> reduce (Second secondElement)
                                    }

                            GT ->
                                { list = secondElement :: secondRest
                                , accumulated =
                                    soFar.accumulated |> reduce (First firstElement)
                                }

                            EQ ->
                                { list = secondRest
                                , accumulated =
                                    soFar.accumulated
                                        |> reduce (FirstSecond ( firstElement, secondElement ))
                                }
    in
    let
        secondAccumulated =
            first.set
                |> foldFrom
                    { list = second.set |> toList Up
                    , accumulated = initial
                    }
                    Up
                    secondAccumulate
    in
    secondAccumulated.list
        |> List.Linear.foldFrom secondAccumulated.accumulated
            Up
            (\secondLeftoverElement soFar ->
                soFar |> reduce (Second secondLeftoverElement)
            )
