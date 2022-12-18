module KeySet.Internal exposing
    ( KeySet
    , only
    , size, tree
    , insert, elementRemove
    )

{-| Lookup with one key

@docs KeySet


## create

@docs only


## scan

@docs size, tree


## alter

@docs insert, elementRemove

-}

import Emptiable exposing (Emptiable(..), emptyAdapt, fill, filled)
import Linear exposing (Direction(..))
import Order exposing (Ordering)
import Possibly exposing (Possibly(..))
import Tree2 exposing (Branch)
import Typed exposing (Checked, Public, Typed)
import Util exposing (when)


type KeySet element tag
    = KeySet
        { tree : Emptiable (Branch element) Never
        , size : Int
        }



-- create


only : element -> Emptiable (KeySet element tag_) never_
only singleElement =
    KeySet
        { size = 1
        , tree = singleElement |> Tree2.leaf
        }
        |> filled



-- scan


size : Emptiable (KeySet element_ tag_) possiblyOrNever_ -> Int
size =
    \keySet ->
        case keySet of
            Empty _ ->
                0

            Filled (KeySet internal) ->
                internal.size


tree :
    Emptiable (KeySet element tag_) possiblyOrNever
    -> Emptiable (Branch element) possiblyOrNever
tree =
    \keySet ->
        keySet
            |> Emptiable.map
                (\(KeySet internal) -> internal.tree |> fill)



-- alter


insert :
    Order.Key element key_ tag
    -> element
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) never_
        )
insert orderKey elementToInsert =
    \keySet ->
        let
            inserted =
                keySet
                    |> tree
                    |> emptyAdapt (\_ -> Possible)
                    |> treeInsert (Order.withKey orderKey) elementToInsert
        in
        inserted.tree
            |> Emptiable.map
                (\branch ->
                    KeySet
                        { size =
                            keySet
                                |> size
                                |> when inserted.sizeIncreased (\n -> n + 1)
                        , tree = branch |> filled
                        }
                )


treeInsert :
    (element -> element -> Order)
    -> element
    ->
        (Emptiable (Branch element) Possibly
         ->
            { sizeIncreased : Bool
            , tree : Emptiable (Branch element) never_
            }
        )
treeInsert ordering elementToInsert =
    \tree_ ->
        case tree_ |> Emptiable.map filled of
            Empty _ ->
                { sizeIncreased = True
                , tree = elementToInsert |> Tree2.leaf
                }

            Filled treeFilled ->
                case ordering elementToInsert (treeFilled |> Tree2.trunk) of
                    EQ ->
                        { sizeIncreased = False
                        , tree =
                            treeFilled
                                |> emptyAdapt never
                                |> Tree2.trunkAlter (\_ -> elementToInsert)
                        }

                    LT ->
                        let
                            leftWithInserted =
                                treeFilled
                                    |> Tree2.children
                                    |> .left
                                    |> treeInsert ordering elementToInsert
                        in
                        { sizeIncreased = leftWithInserted.sizeIncreased
                        , tree =
                            Tree2.branch
                                (treeFilled |> Tree2.trunk)
                                { left = leftWithInserted.tree |> emptyAdapt (\_ -> Possible)
                                , right = treeFilled |> Tree2.children |> .right
                                }
                        }

                    GT ->
                        let
                            rightWithInserted =
                                treeFilled
                                    |> Tree2.children
                                    |> .right
                                    |> treeInsert ordering elementToInsert
                        in
                        { sizeIncreased = rightWithInserted.sizeIncreased
                        , tree =
                            Tree2.branch
                                (treeFilled |> Tree2.trunk)
                                { left = treeFilled |> Tree2.children |> .left
                                , right = rightWithInserted.tree |> emptyAdapt (\_ -> Possible)
                                }
                        }


elementRemove :
    Order.Key element key tag
    -> key
    ->
        (Emptiable (KeySet element tag) possiblyOrNever_
         -> Emptiable (KeySet element tag) Possibly
        )
elementRemove orderKey keyToRemove =
    \keySet ->
        case
            keySet
                |> tree
                |> emptyAdapt (\_ -> Possible)
                |> treeElementRemove (orderKey |> Typed.untag) keyToRemove
        of
            Emptiable.Empty _ ->
                keySet |> emptyAdapt (\_ -> Possible)

            Emptiable.Filled nextRoot ->
                nextRoot
                    |> Emptiable.map
                        (\nextBranch ->
                            KeySet
                                { tree = nextBranch |> filled
                                , size = (keySet |> size) - 1
                                }
                        )


treeElementRemove :
    { toKey : element -> key
    , keyOrder : ( key, key ) -> Order
    }
    -> key
    ->
        (Emptiable (Branch element) Possibly
         -> Emptiable (Emptiable (Branch element) Possibly) Possibly
        )
treeElementRemove keyConfig key =
    \tree_ ->
        tree_
            |> Emptiable.map filled
            |> Emptiable.mapFlat
                (\treeFilled ->
                    case keyConfig.keyOrder ( key, treeFilled |> Tree2.trunk |> keyConfig.toKey ) of
                        LT ->
                            treeFilled
                                |> Tree2.children
                                |> .left
                                |> treeElementRemove keyConfig key
                                |> Emptiable.map
                                    (\nextLeft ->
                                        Tree2.branch
                                            (treeFilled |> Tree2.trunk)
                                            { left = nextLeft
                                            , right = treeFilled |> Tree2.children |> .right
                                            }
                                    )

                        GT ->
                            treeFilled
                                |> Tree2.children
                                |> .right
                                |> treeElementRemove keyConfig key
                                |> Emptiable.map
                                    (\nextRight ->
                                        Tree2.branch
                                            (treeFilled |> Tree2.trunk)
                                            { left = treeFilled |> Tree2.children |> .left
                                            , right = nextRight
                                            }
                                    )

                        EQ ->
                            (if
                                (treeFilled |> Tree2.children |> .left |> Tree2.height)
                                    < (treeFilled |> Tree2.children |> .right |> Tree2.height)
                             then
                                case treeFilled |> Tree2.children |> .right of
                                    Empty _ ->
                                        treeFilled |> Tree2.children |> .left

                                    Filled rightBranch ->
                                        Tree2.branch
                                            (rightBranch |> filled |> Tree2.end Down)
                                            { left = treeFilled |> Tree2.children |> .left
                                            , right = rightBranch |> filled |> Tree2.endRemove Down
                                            }

                             else
                                case treeFilled |> Tree2.children |> .left of
                                    Empty _ ->
                                        treeFilled |> Tree2.children |> .right

                                    Filled leftBranch ->
                                        Tree2.branch
                                            (leftBranch |> filled |> Tree2.end Up)
                                            { left = leftBranch |> filled |> Tree2.endRemove Up
                                            , right = treeFilled |> Tree2.children |> .right
                                            }
                            )
                                |> filled
                )
