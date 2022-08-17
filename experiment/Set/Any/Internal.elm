module Set.Any.Internal exposing
    ( Color(..)
    , Dict(..)
    , foldrFrom
    , fromSortedList
    , getRange
    , keyList
    , ordering
    , unionAccumulator
    , validateInvariants
    )

import Order exposing (Ordering)
import Value exposing (Value)



-- TYPES --


type Dict ordered unordered
    = Leaf (Ordering ordered)
    | Node (Ordering ordered) Color { ordered : ordered, unordered : unordered } (Dict ordered unordered) (Dict ordered unordered)


{-| The color of a Node. Leafs are considered black.
-}
type Color
    = Black
    | Red


getRange : Dict ordered unordered -> Maybe ( ordered, ordered )
getRange dict =
    case dict of
        Leaf _ ->
            Nothing

        Node _ _ { ordered } left right ->
            Just ( getMinKeyHelp ordered left, getMaxKeyHelp ordered right )


getMinKeyHelp : ordered -> Dict ordered unordered -> ordered
getMinKeyHelp minKey dict =
    case dict of
        Leaf _ ->
            minKey

        Node _ _ newMin left _ ->
            getMinKeyHelp newMin.ordered left


getMaxKeyHelp : ordered -> Dict ordered unordered -> ordered
getMaxKeyHelp maxKey dict =
    case dict of
        Leaf _ ->
            maxKey

        Node _ _ newMax _ right ->
            getMaxKeyHelp newMax.ordered right


ordering : Dict ordered unordered -> Ordering ordered
ordering =
    \dict ->
        case dict of
            Leaf order ->
                order

            Node order _ _ _ _ ->
                order


unionAccumulator :
    Ordering k
    -> k
    -> unordered
    -> ( List ( k, unordered ), List ( k, unordered ) )
    -> ( List ( k, unordered ), List ( k, unordered ) )
unionAccumulator order lKey lVal ( result, rList ) =
    case rList of
        [] ->
            ( ( lKey, lVal ) :: result, [] )

        ( rKey, rVal ) :: rRest ->
            case order lKey rKey of
                LT ->
                    ( ( lKey, lVal ) :: result, rList )

                GT ->
                    unionAccumulator order lKey lVal ( ( rKey, rVal ) :: result, rRest )

                EQ ->
                    ( ( lKey, lVal ) :: result, rRest )


{-| Convert an association list with sorted and distinct keys into a dictionary.
-}
fromSortedList :
    Ordering ordered
    -> Bool
    -> List { ordered : ordered, unordered : unordered }
    -> Dict ordered unordered
fromSortedList order isAscending list =
    case list of
        [] ->
            Leaf order

        element :: rest ->
            sortedListToNodeList order isAscending [] element rest
                |> fromNodeList order isAscending


{-| Gather up a NodeList one level at a time, in successive passes of alternating
direction, until a single root-node remains.
-}
fromNodeList :
    Ordering ordered
    -> Bool
    -> NodeList ordered unordered
    -> Dict ordered unordered
fromNodeList order isReversed =
    \nodeList ->
        case nodeList of
            ( node, [] ) ->
                node

            ( a, ( element1, b ) :: list ) ->
                fromNodeList order
                    (not isReversed)
                    (accumulateNodeList
                        order
                        isReversed
                        []
                        a
                        element1
                        b
                        list
                    )


{-| Represents a non-empty list of nodes separated by key-value pairs.
-}
type alias NodeList ordered unordered =
    ( Dict ordered unordered
    , List
        ( { ordered : ordered, unordered : unordered }
        , Dict ordered unordered
        )
    )


{-| Convert a non-empty association list to the bottom level of nodes separated
by key-value pairs. (reverses order)
-}
sortedListToNodeList :
    Ordering ordered
    -> Bool
    -> List ( { ordered : ordered, unordered : unordered }, Dict ordered unordered )
    -> { ordered : ordered, unordered : unordered }
    -> List { ordered : ordered, unordered : unordered }
    -> NodeList ordered unordered
sortedListToNodeList order isAsc listReversed element1 list =
    case list of
        [] ->
            ( Node order Black element1 (Leaf order) (Leaf order), listReversed )

        element2 :: [] ->
            if isAsc then
                ( Node order Black element2 (Node order Red element1 (Leaf order) (Leaf order)) (Leaf order)
                , listReversed
                )

            else
                ( Node order Black element1 (Node order Red element2 (Leaf order) (Leaf order)) (Leaf order)
                , listReversed
                )

        element2 :: element3 :: [] ->
            ( Node order Black element3 (Leaf order) (Leaf order)
            , ( element2, Node order Black element1 (Leaf order) (Leaf order) )
                :: listReversed
            )

        element2 :: element3 :: element4 :: rest ->
            if isAsc then
                sortedListToNodeList order
                    isAsc
                    (( element3, Node order Black element2 (Node order Red element1 (Leaf order) (Leaf order)) (Leaf order) )
                        :: listReversed
                    )
                    element4
                    rest

            else
                sortedListToNodeList order
                    isAsc
                    (( element3, Node order Black element1 (Node order Red element2 (Leaf order) (Leaf order)) (Leaf order) )
                        :: listReversed
                    )
                    element4
                    rest


{-| Gather up a NodeList to the next level. (reverses order)
-}
accumulateNodeList :
    Ordering ordered
    -> Bool
    -> List ( { ordered : ordered, unordered : unordered }, Dict ordered unordered )
    -> Dict ordered unordered
    -> { ordered : ordered, unordered : unordered }
    -> Dict ordered unordered
    -> List ( { ordered : ordered, unordered : unordered }, Dict ordered unordered )
    -> NodeList ordered unordered
accumulateNodeList order isReversed revList a element1 b list =
    case list of
        [] ->
            if isReversed then
                ( Node order Black element1 b a, revList )

            else
                ( Node order Black element1 a b, revList )

        ( element2, c ) :: [] ->
            if isReversed then
                ( Node order Black element1 (Node order Red element2 c b) a
                , revList
                )

            else
                ( Node order Black element2 (Node order Red element1 a b) c
                , revList
                )

        ( element2, c ) :: ( element3, d ) :: [] ->
            if isReversed then
                ( Node order Black element3 d c
                , ( element2, Node order Black element1 b a )
                    :: revList
                )

            else
                ( Node order Black element3 c d
                , ( element2, Node order Black element1 a b )
                    :: revList
                )

        ( element2, c ) :: ( element3, d ) :: ( element4, e ) :: rest ->
            if isReversed then
                accumulateNodeList order
                    isReversed
                    (( element3, Node order Black element1 (Node order Red element2 c b) a )
                        :: revList
                    )
                    d
                    element4
                    e
                    rest

            else
                accumulateNodeList order
                    isReversed
                    (( element3, Node order Black element2 (Node order Red element1 a b) c )
                        :: revList
                    )
                    d
                    element4
                    e
                    rest


keyList : Dict ordered unordered -> List ordered
keyList =
    foldrFrom ( [], \{ ordered } -> (::) ordered )


foldrFrom :
    ( accumulationValue
    , { ordered : ordered, unordered : unordered }
      -> accumulationValue
      -> accumulationValue
    )
    -> Dict ordered unordered
    -> accumulationValue
foldrFrom ( accumulationValueInitial, reduce ) =
    \dict ->
        case dict of
            Leaf _ ->
                accumulationValueInitial

            Node _ _ element left right ->
                foldrFrom
                    ( reduce element
                        (foldrFrom
                            ( accumulationValueInitial, reduce )
                            right
                        )
                    , reduce
                    )
                    left



-- VALIDATION --


validateInvariants : Dict ordered unordered -> String
validateInvariants =
    \dict ->
        case dict |> bst of
            NotBst ->
                "Not in symmetric order"

            Bst ->
                if not (check23 dict) then
                    "Not a 2-3 tree"

                else
                    case dict |> balanceCheck of
                        Unbalanced ->
                            "Not balanced"

                        Balanced ->
                            ""


type Bst
    = Bst
    | NotBst


bst : Dict ordered unordered -> Bst
bst dict =
    bstHelp (ordering dict) Bst (keyList dict)


bstHelp : Ordering unordered -> Bst -> List unordered -> Bst
bstHelp order soFar =
    \keyList_ ->
        case keyList_ of
            [] ->
                soFar

            _ :: [] ->
                soFar

            element0 :: element1 :: after2 ->
                bstHelp
                    order
                    (case ( soFar, order element0 element1 ) of
                        ( Bst, LT ) ->
                            Bst

                        ( _, _ ) ->
                            NotBst
                    )
                    (element1 :: after2)


check23 : Dict ordered unordered -> Bool
check23 =
    \dict ->
        check23Help dict dict


check23Help : Dict ordered unordered -> Dict ordered unordered -> Bool
check23Help root =
    \node ->
        case node of
            Leaf _ ->
                True

            Node _ color_ _ left right ->
                case color right of
                    Red ->
                        False

                    Black ->
                        if node /= root && color_ == Red && (left |> color) == Red then
                            False

                        else
                            check23Help root left
                                && check23Help root right


color : Dict ordered unordered_ -> Color
color =
    \(Node _ color_ _ _ _) ->
        color_


balanceCheck : Dict ordered unordered_ -> Balance
balanceCheck =
    \dict ->
        dict
            |> balanceCheckHelp
                { blacks =
                    dict |> balanceCheckBlacksHelp { blacks = 0 }
                }


balanceCheckBlacksHelp : { blacks : Int } -> Dict ordered unordered_ -> Int
balanceCheckBlacksHelp { blacks } =
    \node ->
        case node of
            Leaf _ ->
                blacks

            Node _ color_ _ left _ ->
                case color_ of
                    Red ->
                        left |> balanceCheckBlacksHelp { blacks = blacks }

                    Black ->
                        left |> balanceCheckBlacksHelp { blacks = blacks + 1 }


type Balance
    = Balanced
    | Unbalanced


balanceCheckHelp : { blacks : Int } -> Dict ordered unordered_ -> Balance
balanceCheckHelp { blacks } =
    \node ->
        case node of
            Leaf _ ->
                case blacks of
                    0 ->
                        Balanced

                    _ ->
                        Unbalanced

            Node _ color_ _ left right ->
                let
                    nextBlacks =
                        case color_ of
                            Red ->
                                blacks

                            Black ->
                                blacks - 1
                in
                case
                    ( left |> balanceCheckHelp { blacks = nextBlacks }
                    , right |> balanceCheckHelp { blacks = nextBlacks }
                    )
                of
                    ( Balanced, Balanced ) ->
                        Balanced

                    ( Unbalanced, _ ) ->
                        Unbalanced

                    ( _, Unbalanced ) ->
                        Unbalanced
