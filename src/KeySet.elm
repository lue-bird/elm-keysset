module KeySet exposing
    ( KeySet
    , only, fromStack, fromList
    , size, element, end
    , insert, elementRemove, elementAlter
    , map, mapTry
    , unifyWith, except, intersect
    , fold2From, FirstOrSecondOrBoth(..)
    , toStack, toList
    , foldFrom, foldFromOne, fold
    )

{-| Lookup with one key

@docs KeySet


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

@docs toStack, toList
@docs foldFrom, foldFromOne, fold

-}

import Emptiable exposing (Emptiable(..), emptyAdapt, filled)
import KeySet.Internal
import Linear exposing (Direction(..))
import List.Linear
import Order exposing (Ordering)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Tree2
import Typed exposing (Checked, Public, Typed)


{-| 🗃️ Non-empty AVL-tree-based look-up. Elements and keys can be of any type

[`insert`](#insert), [`elementRemove`](#elementRemove), [`element`](#element), [`end`](#end) runtime is `log n`

    import KeySet exposing (KeySet)
    import Stack
    import User exposing (User(..))

    users : Emptiable (KeySet User User.ByName) never_
    users =
        KeySet.fromStack User.byName
            (Stack.topBelow
                (User { name = "Alice", age = 28, height = 1.65 })
                [ User { name = "Bob", age = 19, height = 1.82 }
                , User { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

where

    module User exposing (ByName, User(..), byName)

    import KeySet
    import Order

    type User
        = User { name : String, age : Int, height : Float }

    type ByName
        = ByName

    byName : Order.Key User String ByName
    byName =
        Order.key .name
            { tag = ByName
            , order = String.Order.greaterEarlier (Char.Order.alphabetically Case.lowerUpper)
            }

-}
type alias KeySet element tag =
    KeySet.Internal.KeySet element tag



{- TODO include somewhere as doc

   Configure what's considered a key inside a [`KeySet`](#KeySet)

   Create using [`sortingKey`](#sortingKey), as shown in

     - [`KeySet` example](#KeySet)
     - [readme example](https://dark.elm.dmy.fr/packages/lue-bird/elm-keysset/latest/#KeySet)

   You can just ignore the `Typed` thing but if you're curious → [`typed-value`](https://dark.elm.dmy.fr/packages/lue-bird/elm-typed-value/latest/)

   By which aspect = key and in which key `Order` the elements are [sorted](Order#Key)

   For example, to create a [`Order.Key`](Order#Key) to use with a simple set, use

       Order.key identity ..tag..order..

-}
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
    Order.Key element key_ tag
    -> List element
    -> Emptiable (KeySet element tag) Possibly
fromList orderKey =
    \list ->
        list
            |> List.foldr
                (\el soFar -> soFar |> insert orderKey el)
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
    Order.Key element key_ tag
    -> Emptiable (Stacked element) possiblyOrNever
    -> Emptiable (KeySet element tag) possiblyOrNever
fromStack orderKey =
    \stack ->
        stack
            |> Emptiable.mapFlat
                (\stacked ->
                    stacked
                        |> filled
                        |> Stack.foldFromOne only
                            Down
                            (\el soFar -> soFar |> insert orderKey el)
                )



-- scan


{-| Number of elements in the [`KeysSet`](#KeySet).
Runtime `1`

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


{-| Access the element associated with a given key.
If no element with the given key is not present, `Emptiable.empty`

    import Emptiable exposing (Emptiable, filled, Emptiable.map)
    import Stack
    import KeySet exposing (KeySet)
    import Case
    import Char.Order
    import String.Order

    type alias Animal =
        { name : String
        , kind : AnimalKind
        }

    type AnimalKind
        = Mouse
        | Cat

    animals : Emptiable (KeySet Animal { byName : () }) never_
    animals =
        KeySet.fromStack animalByName
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
                String.Order.greaterEarlier (Char.Order.alphabetically Case.lowerUpper)
            }

    animals |> KeySet.element animalByName "Tom" |> Emptiable.map .kind
    --> filled Cat

    animals |> KeySet.element animalByName "Jerry" |> Emptiable.map .kind
    --> filled Mouse

    animals |> KeySet.element animalByName "Spike" |> Emptiable.map .kind
    --> Emptiable.empty

-}
element :
    Order.Key element key tag
    -> key
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable element Possibly
        )
element orderKey keyToAccess =
    \keySet ->
        keySet
            |> tree
            |> emptyAdapt (\_ -> Possible)
            |> treeElement (orderKey |> Typed.untag) keyToAccess


treeElement :
    { toKey : element -> key
    , keyOrder : ( key, key ) -> Order
    }
    -> key
    ->
        (Emptiable (Tree2.Branch element) Possibly
         -> Emptiable element Possibly
        )
treeElement orderKey key =
    \tree_ ->
        tree_
            |> Emptiable.map filled
            |> Emptiable.mapFlat
                (\treeFilled ->
                    case orderKey.keyOrder ( key, treeFilled |> Tree2.trunk |> orderKey.toKey ) of
                        LT ->
                            treeFilled |> Tree2.children |> .left |> treeElement orderKey key

                        GT ->
                            treeFilled |> Tree2.children |> .right |> treeElement orderKey key

                        EQ ->
                            treeFilled |> Tree2.trunk |> filled
                )


{-| Get the element associated with a key at the end looking in a given `Direction`

  - minimum → `end Down`
  - maximum → `end Up`

[`KeySet`](#KeySet) is `empty` → Nothing

    import Linear exposing (Direction(..))
    import Emptiable exposing (Emptiable)
    import Stack
    import KeySet exposing (KeySet)
    import Case
    import Char.Order
    import String.Order


    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

    users : Emptiable (KeySet User { userByName : () }) never_
    users =
        KeySet.fromStack userByName
            (Stack.topBelow
                { name = "Bob", age = 19, height = 1.82 }
                [ { name = "Alice", age = 28, height = 1.65 }
                , { name = "Chuck", age = 33, height = 1.75 }
                ]
            )

    -- ↓ and tag should be opaque
    userByName : Order.Key User String { userByName : () }
    userByName =
        Order.key .name
            { tag = { userByName = () }
            , order =
                String.Order.greaterEarlier (Char.Order.alphabetically Case.lowerUpper)
            }

    users |> KeySet.end Down
    --> { name = "Alice", age = 28, height = 1.65 }

    users |> KeySet.end Up
    --> { name = "Chuck", age = 33, height = 1.75 }

Notice how we safely avoided returning a `Maybe`
through the use of [`Emptiable ... Never`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable)

If you don't know whether the [`KeySet`](#KeySet) will be empty

    users
        |> Emptiable.map (\us -> us |> filled |> KeySet.end Up)
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
Replaces the element when there is a key collision
-}
insert :
    Order.Key element key_ tag
    -> element
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) never_
        )
insert orderKey elementToInsert =
    \keySet ->
        keySet |> KeySet.Internal.insert orderKey elementToInsert


{-| Remove its element whose key matches the given one.
If the key is not found, no changes are made
-}
elementRemove :
    Order.Key element key tag
    -> key
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) Possibly
        )
elementRemove orderKey keyToRemove =
    \keySet ->
        keySet
            |> KeySet.Internal.elementRemove orderKey keyToRemove


{-| Change the element for a given key in a given way

  - The input is the same as asking for the [`element`](#element) with the same key
  - Outputting `empty` is the same as calling [`elementRemove`](#elementRemove) with the same key

Operations with `Emptiable` that might be handy

  - [`Emptiable.map`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#Emptiable.map)
  - [`Emptiable.mapFlat`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#Emptiable.mapFlat)
  - [`fillElseOnEmpty`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#fillElseOnEmpty)
  - [`filled`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/Emptiable#filled)

-}
elementAlter :
    Order.Key element key tag
    -> key
    ->
        (Emptiable element Possibly
         -> Emptiable element elementAlteredEmptyPossiblyOrNever
        )
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) elementAlteredEmptyPossiblyOrNever
        )
elementAlter orderKey keyToAlter emptiableElementTryMap =
    \keySet ->
        case keySet |> element orderKey keyToAlter of
            Empty _ ->
                case Emptiable.empty |> emptiableElementTryMap of
                    Empty emptiable ->
                        keySet |> emptyAdapt (\_ -> emptiable)

                    Filled elementNew ->
                        keySet |> insert orderKey elementNew

            Filled elementToAlter ->
                case elementToAlter |> filled |> emptiableElementTryMap of
                    Empty emptiable ->
                        keySet
                            |> elementRemove orderKey keyToAlter
                            |> emptyAdapt (\_ -> emptiable)

                    Filled elementAltered ->
                        keySet |> insert orderKey elementAltered



-- transform


{-| Convert to a `List` sorted by keys
in a given [`Direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Linear#Direction)

    import Linear exposing (Direction(..))
    import Stack
    import KeySet
    import Case
    import Char.Order
    import String.Order

    -- tag should be opaque in a separate module
    nameAlphabetical : Order.Key String String { alphabetical : () }
    nameAlphabetical =
        Order.key identity
            { tag = { alphabetical = () }
            , order = String.Order.greaterEarlier (Char.Order.alphabetically Case.lowerUpper)
            }

    KeySet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeySet.toList Up
    --> [ "Alice", "Bob" ]

    KeySet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeySet.toList Down
    --> [ "Bob", "Alice" ]

to carry over information about (non-)emptiness → [`toStack`](#toStack)

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


{-| Convert to a `List` sorted by keys
in a given [`Direction`](https://dark.elm.dmy.fr/packages/lue-bird/elm-linear-direction/latest/Linear#Direction)

    import Linear exposing (Direction(..))
    import Stack
    import KeySet
    import Case
    import Char.Order
    import String.Order

    -- tag should be opaque in a separate module
    nameAlphabetical : Order.Key String String { alphabetical : () }
    nameAlphabetical =
        Order.key identity
            { tag = { alphabetical = () }
            , order = String.Order.greaterEarlier (Char.Order.alphabetically Case.lowerUpper)
            }

    KeySet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice" ])
        |> KeySet.toStack Up
    --> Stack.topBelow "Alice" [ "Bob" ]

    KeySet.fromStack nameAlphabetical
        (Stack.topBelow "Bob" [ "Alice", "Christoph" ])
        |> KeySet.toStack Down
    --> Stack.topBelow "Christoph" [ "Bob", "Alice" ]

The cool thing is that information about (non-)emptiness is carried over to the stack

-}
toStack :
    Linear.Direction
    ->
        (Emptiable (KeySet element tag_) possiblyOrNever
         -> Emptiable (Stacked element) possiblyOrNever
        )
toStack direction =
    \keySet ->
        keySet
            |> Emptiable.map filled
            |> Emptiable.mapFlat
                (\keySetFilled ->
                    keySetFilled
                        |> foldFromOne Stack.one
                            (direction |> Linear.opposite)
                            (\element_ soFar -> soFar |> Stack.onTopLay element_)
                )


{-| Change each element based on its current value.
Runtime `n * log n` because mapped keys could be different (many other dicts/sets have runtime `n`)
-}
map :
    (element -> mappedElement)
    -> Order.Key mappedElement mappedKey_ mappedTag
    ->
        (Emptiable (KeySet element tag_) possiblyOrNever
         -> Emptiable (KeySet mappedElement mappedTag) possiblyOrNever
        )
map elementChange mappedKey =
    \keySet ->
        keySet
            |> Emptiable.mapFlat
                (\keySetFilled ->
                    keySetFilled
                        |> filled
                        |> foldFromOne (\end_ -> end_ |> elementChange |> only)
                            Up
                            (\element_ soFar ->
                                soFar |> insert mappedKey (element_ |> elementChange)
                            )
                )


{-| Try to change each element based on its current value.
Runtime `n * log n` just like [`KeySet.map`](#map) because mapped keys could be different

    {-| Keep only elements that pass a given test.
    Often called "filter"
    -}
    when orderKey isGood =
        KeySet.mapTry
            (\element ->
                if element |> isGood then
                    Just element

                else
                    Nothing
            )
            orderKey

-}
mapTry :
    (element -> Emptiable mappedElement possiblyOrNever)
    -> Order.Key mappedElement mappedKey_ mappedTag
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
then reducing what's accumulated
in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import Stack
    import KeySet
    import Int.Order

    KeySet.fromStack intIncreasing
        (Stack.topBelow 1 [ 2, 8, 16 ])
        |> KeySet.fold Down (\n soFar -> soFar - n)
    --> 5

    -- ↓ and tag should be opaque
    intIncreasing : Order.Key Int Int { increasing : () }
    intIncreasing =
        Order.key identity
            { tag = { increasing = () }
            , order = Int.Order.increasing
            }

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
        keySet |> foldFromOne identity direction reduce


{-| Fold, starting from one end element transformed to the initial accumulation value,
then reducing what's accumulated in a given [`Direction`](https://package.elm-lang.org/packages/lue-bird/elm-linear-direction/latest/)

    import Linear exposing (Direction(..))
    import Stack
    import Int.Order
    import KeySet

    KeySet.fromStack intIncreasing
        (Stack.topBelow 234 [ 345, 543 ])
        |> KeySet.foldFromOne Stack.only Up Stack.onTopLay
    --> Stack.topBelow 543 [ 345, 234 ]
    --  the type knows it's never empty

    -- ↓ and tag should be opaque
    intIncreasing : Order.Key Int Int { increasing : () }
    intIncreasing =
        Order.key identity
            { tag = { increasing = () }
            , order = Int.Order.increasing
            }

A simpler version is [`fold`](#fold)

    KeySet.fold =
        KeySet.foldFromOne identity

-}
foldFromOne :
    (element -> accumulated)
    -> Linear.Direction
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (KeySet element tag_) Never
         -> accumulated
        )
foldFromOne firstToInitial direction reduce =
    \keySet ->
        keySet
            |> tree
            |> Tree2.foldFromOne firstToInitial direction reduce


{-| Fold over its elements from an initial accumulator value either

  - `Up` from lowest key to highest
  - `Down` from highest key to lowest

```
import Linear exposing (Direction(..))
import Stack
import Int.Order
import KeySet

KeySet.fromStack intIncreasing
    (Stack.topBelow 234 [ 345, 543 ])
    |> KeySet.foldFrom [] Down (::)
--> [ 234, 345, 543 ]

KeySet.fromStack intIncreasing
    (Stack.topBelow False [ True, False ])
    |> KeySet.foldFrom 0 Up (||)
--> True

-- ↓ and tag should be opaque
intIncreasing : Order.Key Int Int { increasing : () }
intIncreasing =
    Order.key identity
        { tag = { increasing = () }
        , order = Int.Order.increasing
        }
```

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


{-| Combine with another [`KeySet`](#KeySet).
On key collision, keep the current [`KeySet`](#KeySet)'s element
-}
unifyWith :
    Order.Key element key_ tag
    -> Emptiable (KeySet element tag) incomingPossiblyOrNever_
    ->
        (Emptiable (KeySet element tag) possiblyOrNever
         -> Emptiable (KeySet element tag) possiblyOrNever
        )
unifyWith orderKey toCombineWith =
    \keySet ->
        toCombineWith
            |> foldFrom keySet
                Up
                (\rightElement soFar ->
                    soFar |> insert orderKey rightElement
                )


{-| Keep each element whose key also appears in a given [`KeySet`](#KeySet)
-}
intersect :
    Order.Key element key_ tag
    -> Emptiable (KeySet element tag) incomingPossiblyOrNever_
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) Possibly
        )
intersect orderKey toIntersectWith =
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
                        |> element orderKey
                            (element_ |> (orderKey |> Typed.untag |> .toKey))
                )
                orderKey


{-| Keep only those elements whose keys don't appear in the given [`KeySet`](#KeySet)
-}
except :
    Order.Key element key_ tag
    -> Emptiable (KeySet element tag) incomingPossiblyOrNever_
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) Possibly
        )
except orderKey toExclude =
    \keySet ->
        toExclude
            |> foldFrom (keySet |> emptyAdapt (\_ -> Possible))
                Up
                (\element_ diffSoFar ->
                    diffSoFar
                        |> elementRemove orderKey
                            (element_ |> (orderKey |> Typed.untag |> .toKey))
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
            { orderKey : Order.Key firstElement key firstTag
            , set : Emptiable (KeySet firstElement firstTag) firstPossiblyOrNever_
            }
         , second :
            { orderKey : Order.Key secondElement key secondTag
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
                            (first.orderKey |> Typed.untag |> .keyOrder)
                                ( secondElement |> (second.orderKey |> Typed.untag |> .toKey)
                                , firstElement |> (first.orderKey |> Typed.untag |> .toKey)
                                )
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
