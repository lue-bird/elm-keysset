module Set.Any exposing
    ( Set
    , empty, singleton, insert, insertAll, remove, merge
    , isEmpty, size, element, memberOf, areEqual
    , foldFrom
    , toList, fromList
    , alter, elementAlter, justs
    )

{-| A set for elements with explicit unique aspects and possible duplicate parts.

@docs Set


## create

@docs empty, singleton, insert, insertAll, remove, update, merge


## scan

@docs isEmpty, size, element, memberOf, areEqual


# Transform

@docs map, foldFrom


# Lists

@docs keys, values, toList, fromList

The internal implementation is a left-Leaning Set.Any.Internal.Red Set.Any.Internal.Black Tree (LLRB Tree).
More information about this implementation can be found at the following links:

  - <http://www.cs.princeton.edu/~rs/talks/LLRB/LLRB.pdf>
  - <http://www.cs.princeton.edu/~rs/talks/LLRB/RedBlack.pdf>

The short of it is, that in addition to the regular rules for RB trees,
no right references can be red.

-}

import Conversion exposing (Conversion, ConversionPreserving)
import Linear exposing (DirectionLinear(..))
import Order exposing (Ordering)
import Set
import Set.Any.Internal


type Set element ordered unordered
    = Set
        { internal : element -> { ordered : ordered, unordered : unordered }
        , public : { ordered : ordered, unordered : unordered } -> element
        }
        (Internal ordered unordered)


type Internal compared notCompared
    = Internal (Set.Any.Internal.Dict compared notCompared)


empty :
    ( Ordering ordered
    , { internal : element -> { ordered : ordered, unordered : unordered }
      , public : { ordered : ordered, unordered : unordered } -> element
      }
    )
    -> Set element ordered unordered
empty ( order, elementOrderedUnorderedConversion ) =
    Set.Any.Internal.empty order
        |> Set elementOrderedUnorderedConversion


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.
-}
type alias Dict key value =
    Internal.Dict.Dict key value


{-| Create an empty dictionary.
-}
empty : Sorter k -> Dict k v
empty order =
    Set.Any.Internal.Leaf order


{-| Check if two dictionaries are equal.
-}
areEqual : ( Set element k v, Set element k v ) -> Bool
areEqual =
    \( first, second ) ->
        case ( first, second ) of
            ( Set.Any.Internal.Leaf _, Set.Any.Internal.Leaf _ ) ->
                True

            ( Set.Any.Internal.Node _ _ _ fValue fLeft fRight, Set.Any.Internal.Node _ _ _ sValue sLeft sRight ) ->
                fValue == sValue && areEqual fLeft sLeft && areEqual fRight sRight

            ( Set.Any.Internal.Leaf _, Set.Any.Internal.Node _ _ _ _ _ ) ->
                False

            ( Set.Any.Internal.Node _ _ _ _ _, Set.Any.Internal.Leaf _ ) ->
                False


{-| Determine if a dictionary is empty.
isEmpty empty == True
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
    case dict of
        Set.Any.Internal.Leaf _ ->
            True

        Set.Any.Internal.Node _ _ _ _ _ _ ->
            False


{-| Create a dictionary with one key-value pair.
-}
singleton : Sorter k -> k -> v -> Dict k v
singleton order key value =
    -- Root is always black
    Set.Any.Internal.Node order Set.Any.Internal.Black key value (Set.Any.Internal.Leaf order) (Set.Any.Internal.Leaf order)


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict k v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> Dict k v -> Int
sizeHelp n dict =
    case dict of
        Set.Any.Internal.Leaf _ ->
            n

        Set.Any.Internal.Node _ _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
    get "Tom" animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
element :
    ( ordered -> uniquePartToFind, uniquePartToFind )
    -> Set element ordered unordered
    -> Maybe element
element targetKey =
    \dict ->
        case dict of
            Set.Any.Internal.Leaf _ ->
                Nothing

            Set.Any.Internal.Node order _ key value left right ->
                case Sort.toOrder order targetKey key of
                    LT ->
                        element targetKey left

                    GT ->
                        element targetKey right

                    EQ ->
                        Just value


{-| Insert a key-value pair in a dictionary. Replaces value when that key is
already present.
-}
insert : k -> v -> Dict k v -> Dict k v
insert key value dict =
    case insertHelp key value dict of
        Set.Any.Internal.Node order Set.Any.Internal.Red k v l r ->
            Set.Any.Internal.Node order Set.Any.Internal.Black k v l r

        x ->
            x


insertHelp : k -> v -> Dict k v -> Dict k v
insertHelp key value dict =
    case dict of
        Set.Any.Internal.Leaf order ->
            -- New nodes are always red. If it violates the rules, it will be fixed
            -- when balancing.
            Set.Any.Internal.Node order Set.Any.Internal.Red key value (Set.Any.Internal.Leaf order) (Set.Any.Internal.Leaf order)

        Set.Any.Internal.Node order nColor nKey nValue nLeft nRight ->
            case Sort.toOrder order key nKey of
                LT ->
                    balance order nColor nKey nValue (insertHelp key value nLeft) nRight

                GT ->
                    balance order nColor nKey nValue nLeft (insertHelp key value nRight)

                EQ ->
                    Set.Any.Internal.Node order nColor nKey value nLeft nRight


balance : Sorter k -> Color -> k -> v -> Dict k v -> Dict k v -> Dict k v
balance order color key value left right =
    case right of
        Set.Any.Internal.Node _ Set.Any.Internal.Red rElement rLeft rRight ->
            case left of
                Set.Any.Internal.Node _ Set.Any.Internal.Red lElement lLeft lRight ->
                    Set.Any.Internal.Node
                        order
                        Set.Any.Internal.Red
                        key
                        value
                        (Set.Any.Internal.Node order Set.Any.Internal.Black lElement lLeft lRight)
                        (Set.Any.Internal.Node order Set.Any.Internal.Black rElement rLeft rRight)

                _ ->
                    Set.Any.Internal.Node order color rElement (Set.Any.Internal.Node order Set.Any.Internal.Red key value left rLeft) rRight

        _ ->
            case left of
                Set.Any.Internal.Node _ Set.Any.Internal.Red lElement (Set.Any.Internal.Node _ Set.Any.Internal.Red llK llV llLeft llRight) lRight ->
                    Set.Any.Internal.Node
                        order
                        Set.Any.Internal.Red
                        lK
                        lV
                        (Set.Any.Internal.Node order Set.Any.Internal.Black llK llV llLeft llRight)
                        (Set.Any.Internal.Node order Set.Any.Internal.Black key value lRight right)

                _ ->
                    Set.Any.Internal.Node order color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : k -> Dict k v -> Dict k v
remove targetKey dict =
    case removeHelp targetKey dict of
        Set.Any.Internal.Node order Set.Any.Internal.Red k v l r ->
            Set.Any.Internal.Node order Set.Any.Internal.Black k v l r

        x ->
            x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : k -> Dict k v -> Dict k v
removeHelp targetKey dict =
    case dict of
        (Set.Any.Internal.Leaf _) as leaf ->
            leaf

        Set.Any.Internal.Node order color key value left right ->
            case order targetKey key of
                LT ->
                    case left of
                        Set.Any.Internal.Node _ Set.Any.Internal.Black _ _ lLeft _ ->
                            case lLeft of
                                Set.Any.Internal.Node _ Set.Any.Internal.Red _ _ _ _ ->
                                    Set.Any.Internal.Node order color key value (removeHelp targetKey left) right

                                _ ->
                                    case moveRedLeft dict of
                                        Set.Any.Internal.Node _ movedColor movedKey movedValue movedLeft movedRight ->
                                            balance order movedColor movedKey movedValue (removeHelp targetKey movedLeft) movedRight

                                        (Set.Any.Internal.Leaf _) as leaf ->
                                            leaf

                        _ ->
                            Set.Any.Internal.Node order color key value (removeHelp targetKey left) right

                _ ->
                    removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT : k -> Dict k v -> Color -> k -> v -> Dict k v -> Dict k v -> Dict k v
removeHelpPrepEQGT targetKey dict color key value left right =
    case left of
        Set.Any.Internal.Node order Set.Any.Internal.Red lElement lLeft lRight ->
            Set.Any.Internal.Node
                order
                color
                lK
                lV
                lLeft
                (Set.Any.Internal.Node order Set.Any.Internal.Red key value lRight right)

        _ ->
            case right of
                Set.Any.Internal.Node _ Set.Any.Internal.Black _ _ (Set.Any.Internal.Node _ Set.Any.Internal.Black _ _ _ _) _ ->
                    moveRedRight dict

                Set.Any.Internal.Node _ Set.Any.Internal.Black _ _ (Set.Any.Internal.Leaf _) _ ->
                    moveRedRight dict

                _ ->
                    dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : k -> Dict k v -> Dict k v
removeHelpEQGT targetKey dict =
    case dict of
        Set.Any.Internal.Node order color key value left right ->
            if targetKey == key then
                case getMin right of
                    Set.Any.Internal.Node _ _ minKey minValue _ _ ->
                        balance order color minKey minValue left (removeMin right)

                    (Set.Any.Internal.Leaf _) as leaf ->
                        leaf

            else
                balance order color key value left (removeHelp targetKey right)

        (Set.Any.Internal.Leaf _) as leaf ->
            leaf


getMin : Dict k v -> Dict k v
getMin dict =
    case dict of
        Set.Any.Internal.Node _ _ _ _ ((Set.Any.Internal.Node _ _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict


removeMin : Dict k v -> Dict k v
removeMin dict =
    case dict of
        Set.Any.Internal.Node order color key value ((Set.Any.Internal.Node _ lColor _ _ lLeft _) as left) right ->
            case lColor of
                Set.Any.Internal.Black ->
                    case lLeft of
                        Set.Any.Internal.Node _ Set.Any.Internal.Red _ _ _ _ ->
                            Set.Any.Internal.Node order color key value (removeMin left) right

                        _ ->
                            case moveRedLeft dict of
                                Set.Any.Internal.Node _ movedColor movedKey movedValue movedLeft movedRight ->
                                    balance order movedColor movedKey movedValue (removeMin movedLeft) movedRight

                                (Set.Any.Internal.Leaf _) as leaf ->
                                    leaf

                _ ->
                    Set.Any.Internal.Node order color key value (removeMin left) right

        Set.Any.Internal.Node _ _ _ _ ((Set.Any.Internal.Leaf _) as leaf) _ ->
            leaf

        (Set.Any.Internal.Leaf _) as leaf ->
            leaf


moveRedLeft : Dict k v -> Dict k v
moveRedLeft dict =
    case dict of
        Set.Any.Internal.Node order clr k v (Set.Any.Internal.Node _ lClr lElement lLeft lRight) (Set.Any.Internal.Node _ rClr rElement ((Set.Any.Internal.Node _ Set.Any.Internal.Red rlK rlV rlL rlR) as rLeft) rRight) ->
            Set.Any.Internal.Node
                order
                Set.Any.Internal.Red
                rlK
                rlV
                (Set.Any.Internal.Node order Set.Any.Internal.Black k v (Set.Any.Internal.Node order Set.Any.Internal.Red lElement lLeft lRight) rlL)
                (Set.Any.Internal.Node order Set.Any.Internal.Black rElement rlR rRight)

        Set.Any.Internal.Node order clr k v (Set.Any.Internal.Node _ lClr lElement lLeft lRight) (Set.Any.Internal.Node _ rClr rElement rLeft rRight) ->
            case clr of
                Set.Any.Internal.Black ->
                    Set.Any.Internal.Node
                        order
                        Set.Any.Internal.Black
                        k
                        v
                        (Set.Any.Internal.Node order Set.Any.Internal.Red lElement lLeft lRight)
                        (Set.Any.Internal.Node order Set.Any.Internal.Red rElement rLeft rRight)

                Set.Any.Internal.Red ->
                    Set.Any.Internal.Node
                        order
                        Set.Any.Internal.Black
                        k
                        v
                        (Set.Any.Internal.Node order Set.Any.Internal.Red lElement lLeft lRight)
                        (Set.Any.Internal.Node order Set.Any.Internal.Red rElement rLeft rRight)

        _ ->
            dict


moveRedRight : Set.Any.Internal.Dict k v -> Set.Any.Internal.Dict k v
moveRedRight dict =
    case dict of
        Set.Any.Internal.Node order clr element_ (Set.Any.Internal.Node _ lClr lElement (Set.Any.Internal.Node _ Set.Any.Internal.Red llK llV llLeft llRight) lRight) (Set.Any.Internal.Node _ rClr rElement rLeft rRight) ->
            Set.Any.Internal.Node
                order
                Set.Any.Internal.Red
                lK
                lV
                (Set.Any.Internal.Node order Set.Any.Internal.Black llK llV llLeft llRight)
                (Set.Any.Internal.Node order Set.Any.Internal.Black element_ lRight (Set.Any.Internal.Node order Set.Any.Internal.Red rElement rLeft rRight))

        Set.Any.Internal.Node order color element_ (Set.Any.Internal.Node _ lColor lElement lLeft lRight) (Set.Any.Internal.Node _ rColor rElement rLeft rRight) ->
            case color of
                Set.Any.Internal.Black ->
                    Set.Any.Internal.Node
                        order
                        Set.Any.Internal.Black
                        k
                        v
                        (Set.Any.Internal.Node order Set.Any.Internal.Red lElement lLeft lRight)
                        (Set.Any.Internal.Node order Set.Any.Internal.Red rElement rLeft rRight)

                Set.Any.Internal.Red ->
                    Set.Any.Internal.Node
                        order
                        Set.Any.Internal.Black
                        k
                        v
                        (Set.Any.Internal.Node order Set.Any.Internal.Red lElement lLeft lRight)
                        (Set.Any.Internal.Node order Set.Any.Internal.Red rElement rLeft rRight)

        _ ->
            dict


{-| Update the value of a dictionary for a specific key with a given function.
The given function gets the current value as a parameter and its return value
determines if the value is updated or removed. New key-value pairs can be
stored too.
-}
elementAlter : k -> (Maybe v -> Maybe v) -> Dict k v -> Dict k v
elementAlter key elementAlter_ dict =
    case dict |> element key |> elementAlter_ of
        Nothing ->
            remove key dict

        Just value ->
            insert key value dict



-- TRANSFORM


{-| Alter every element based on its current value.
-}
alter : (element -> element) -> Set element k v -> Set element k v
alter elementAlter =
    \dict ->
        case dict of
            Set.Any.Internal.Leaf order ->
                Set.Any.Internal.Leaf order

            Set.Any.Internal.Node order color element left right ->
                Set.Any.Internal.Node order
                    color
                    (elementAlter element)
                    (alter elementAlter left)
                    (alter elementAlter right)


{-| Fold over the elements in a [`Set`](#Set), in order

  - `Up`: from lowest key to highest key
  - `Down` from highest key to lowest key

-}
foldFrom :
    ( accumulationValue
    , DirectionLinear
    , element -> accumulationValue -> accumulationValue
    )
    -> Set element k v
    -> accumulationValue
foldFrom ( accumulationValueInitial, direction, reduce ) =
    case direction of
        Down ->
            Set.Any.Internal.foldr ( accumulationValueInitial, reduce )

        Up ->
            \dict ->
                case dict of
                    Set.Any.Internal.Leaf _ ->
                        accumulationValueInitial

                    Set.Any.Internal.Node _ _ element left right ->
                        foldl reduce (reduce element (foldl reduce accumulationValueInitial left)) right



-- COMBINE


{-| Take all the key-value pairs in the first dictionary and
`insert` them into the second dictionary.
-}
insertAll : Dict k v -> Dict k v -> Dict k v
insertAll newElements original =
    case ( newElements, original ) of
        ( _, Set.Any.Internal.Leaf _ ) ->
            newElements

        ( Set.Any.Internal.Leaf _, _ ) ->
            original

        ( Set.Any.Internal.Node order _ _ _ _, _ ) ->
            let
                ( lt, gt ) =
                    foldl (unionAccumulator order) ( [], toList original ) newElements
            in
            Set.Any.Internal.fromSortedList
                order
                False
                (List.foldl (\e acc -> e :: acc) lt gt)


{-| The most general way of combining two [`Set`](#Set)s. You provide three
accumulators for when a given key appears:

1.  Only in the first dictionary
2.  In both dictionaries
3.  Only in the second dictionary
    You then traverse all the keys, building up whatever you want.
    The given `Sorter` will be used to compare the keys.

-}
merge :
    Ordering k
    -> (k -> a -> result -> result)
    -> (k -> a -> b -> result -> result)
    -> (k -> b -> result -> result)
    -> Dict k a
    -> Dict k b
    -> result
    -> result
merge order leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    case order lKey rKey of
                        LT ->
                            stepState rKey rValue ( rest, leftStep lKey lValue result )

                        GT ->
                            ( list, rightStep rKey rValue result )

                        EQ ->
                            ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers



-- LISTS


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Set element ordered unordered -> List element
toList dict =
    foldFrom ( [], Down, (::) ) dict


{-| Convert an association list into a dictionary.
-}
fromList :
    Ordering orderable
    -> List element
    -> Set element orderable unordered
fromList order list =
    case list of
        headElement :: tailElements ->
            let
                ( sorted, remainder ) =
                    splitSortedHelp order [] headElement tailElements
            in
            List.foldl
                (\( k, v ) dict -> insert k v dict)
                (Set.Any.Internal.fromSortedList order False sorted)
                remainder

        [] ->
            Set.Any.Internal.Leaf order


{-| Split a list into its sorted prefix and the remainder. The sorted prefix
is returned in reversed order.
-}
splitSortedHelp : Ordering k -> List ( k, v ) -> ( k, v ) -> List ( k, v ) -> ( List ( k, v ), List ( k, v ) )
splitSortedHelp order sorted (( k1, _ ) as p1) list =
    case list of
        (( k2, _ ) as p2) :: rest ->
            case order k1 k2 of
                LT ->
                    splitSortedHelp order (p1 :: sorted) p2 rest

                _ ->
                    ( sorted, p1 :: list )

        [] ->
            ( p1 :: sorted, [] )
