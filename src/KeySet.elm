module KeySet exposing
    ( KeySet
    , Sorting, sortingKey
    , only, fromStack, fromList
    , size, element, end
    , insert, elementRemove, elementAlter
    , map, mapTry
    , unifyWith, except, intersect
    , mergeFrom, BaseOrIncomingOrBoth(..)
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
@docs mergeFrom, BaseOrIncomingOrBoth


## transform

@docs toList
@docs foldFrom, foldOnto, fold

-}

import Emptiable exposing (Emptiable(..), emptyAdapt, fillMap, fillMapFlat, filled)
import KeySet.Internal
import Linear exposing (Direction(..))
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

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

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
    KeySet.Internal.KeySetFilled element tag


{-| Configure what's considered a key inside a [`KeySet`](#KeySet)

Create using [`sortingKey`](#sortingKey), as shown in

  - [`KeySet` example](#KeySet)
  - [readme example](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/)

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
  - [readme example](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/)

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


{-| Create a [`KeysSet`](#KeySet) with one given element
-}
only : element -> Emptiable (KeySet element tag_) never_
only singleElement =
    KeySet.Internal.only singleElement


{-| Convert to a [`KeysSet`](#KeySet),
ignoring elements whose keys already exist earlier in the `List`

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
            |> List.foldl
                (\el soFar -> soFar |> insert sorting el)
                Emptiable.empty


{-| Convert to a [`KeysSet`](#KeySet),
ignoring elements whose keys already exist earlier in the [`Stack`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Stack)

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
                            Up
                            (\el soFar -> soFar |> insert sorting el)
                )



-- scan


{-| Determine the number of elements in the [`KeysSet`](#KeySet).
It takes constant time to determine the size.
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

    import KeySet exposing (Dict)

    type Animal
        = Mouse
        | Cat

    animals : Dict String Animal
    animals =
        KeySet.fromList [ ( "Tom", Cat ), ( "Jerry", Mouse ) ]

    KeySet.element (String.Order.greaterEarlier (Char.Order.alphabetically Order.lowerUpper)) "Tom" animals
    --> Just Cat

    KeySet.element (String.Order.greaterEarlier (Char.Order.alphabetically Order.lowerUpper)) "Jerry" animals
    --> Just Mouse

    KeySet.element (String.Order.greaterEarlier (Char.Order.alphabetically Order.lowerUpper)) "Spike" animals
    --> Nothing

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
treeElement keyConfig key =
    \tree_ ->
        tree_
            |> fillMap filled
            |> fillMapFlat
                (\treeFilled ->
                    case keyConfig.keyOrder key (treeFilled |> Tree2.element |> keyConfig.key) of
                        LT ->
                            treeFilled |> Tree2.children |> .left |> treeElement keyConfig key

                        GT ->
                            treeFilled |> Tree2.children |> .right |> treeElement keyConfig key

                        EQ ->
                            treeFilled |> Tree2.element |> filled
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
        KeySet.fromList User.byName
            [ { name = "Bob", age = 19, height = 1.82 }
            , { name = "Alice", age = 28, height = 1.65 }
            , { name = "Chuck", age = 33, height = 1.75 }
            ]

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
        keySet
            |> tree
            |> Tree2.end direction



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
                        Empty emptiable

                    Filled element_ ->
                        keySet |> insert sorting element_

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

    import KeySet

    KeySet.fromList
        [ "Bob", "Alice" ]
        |> KeySet.toList Up
    --> [ "Alice", "Bob" ]

    KeySet.fromList
        [ "Bob", "Alice" ]
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
                            \soFar ->
                                soFar |> insert mappedKey elementMapped
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


{-| Describes an un[merge](#mergeFrom)d element whose key is present

  - Only in the `Incoming` [`KeySet`](#KeySet)
  - Only in the current = `Base` [`KeySet`](#KeySet)
  - In `Both`

-}
type BaseOrIncomingOrBoth base incoming
    = Base base
    | Incoming incoming
    | Both { base : base, incoming : incoming }


{-| Most general way of combining with a [`KeySet`](#KeySet)

Traverses all the keys from lowest to highest,
accumulating whatever you want
for when a given key appears

  - Only in the incoming [`KeySet`](#KeySet)
  - Only in the current [`KeySet`](#KeySet)
  - In both

-}
mergeFrom :
    accumulated
    -> Sorting baseElement key tag_
    ->
        (BaseOrIncomingOrBoth baseElement incomingElement
         -> (accumulated -> accumulated)
        )
    ->
        { key : incomingElement -> key
        , set : Emptiable (KeySet incomingElement incomingTag_) incomingPossiblyOrNever_
        }
    ->
        (Emptiable (KeySet baseElement baseTag_) possiblyOrNever_
         -> accumulated
        )
mergeFrom initial sorting reduce left =
    let
        stepAll :
            baseElement
            ->
                ({ incoming : List incomingElement, semiAccumulated : accumulated }
                 -> { incoming : List incomingElement, semiAccumulated : accumulated }
                )
        stepAll rElement =
            \{ incoming, semiAccumulated } ->
                case incoming of
                    [] ->
                        { incoming = []
                        , semiAccumulated =
                            semiAccumulated |> reduce (Base rElement)
                        }

                    incomingElement :: incomingRest ->
                        case
                            (sorting |> Typed.untag |> .keyOrder)
                                (incomingElement |> left.key)
                                (rElement |> (sorting |> Typed.untag |> .key))
                        of
                            LT ->
                                stepAll rElement
                                    { incoming = incomingRest
                                    , semiAccumulated =
                                        semiAccumulated |> reduce (Incoming incomingElement)
                                    }

                            GT ->
                                { incoming =
                                    incomingElement :: incomingRest
                                , semiAccumulated =
                                    semiAccumulated |> reduce (Base rElement)
                                }

                            EQ ->
                                { incoming = incomingRest
                                , semiAccumulated =
                                    semiAccumulated
                                        |> reduce (Both { base = rElement, incoming = incomingElement })
                                }
    in
    \keySetBase ->
        let
            stepped =
                keySetBase
                    |> foldFrom
                        { incoming = left.set |> toList Up
                        , semiAccumulated = initial
                        }
                        Up
                        stepAll
        in
        stepped.incoming
            |> List.foldl
                (\incomingElement soFar ->
                    soFar |> reduce (Incoming incomingElement)
                )
                stepped.semiAccumulated
