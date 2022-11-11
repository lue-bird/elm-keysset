module KeySet.Internal exposing
    ( KeySetFilled
    , only
    , size, tree
    , insert, elementRemove
    )

{-| Lookup with one key

@docs KeySetFilled


## create

@docs only


## scan

@docs size, tree


## alter

@docs insert, elementRemove

-}

import Emptiable exposing (Emptiable(..), emptyAdapt, fill, fillMap, fillMapFlat, filled)
import Linear exposing (Direction(..))
import Order exposing (Ordering)
import Possibly exposing (Possibly(..))
import Tree2 exposing (Branch)
import Typed exposing (Checked, Public, Typed)


type KeySetFilled element tag
    = KeySet
        { tree : Emptiable (Branch element) Never
        , size : Int
        }


type alias Sorting element key tag =
    Typed
        Checked
        tag
        Public
        { key : element -> key
        , keyOrder :
            Ordering key
        }


only : element -> Emptiable (KeySetFilled element tag_) never_
only singleElement =
    KeySet
        { size = 1
        , tree = Tree2.leaf singleElement
        }
        |> filled


size : Emptiable (KeySetFilled element_ tag_) possiblyOrNever_ -> Int
size =
    \keySet ->
        case keySet of
            Empty _ ->
                0

            Filled (KeySet keySetInternal) ->
                keySetInternal.size



-- scan


tree :
    Emptiable (KeySetFilled element tag_) possiblyOrNever
    -> Emptiable (Branch element) possiblyOrNever
tree =
    \keySet ->
        keySet
            |> fillMap
                (\(KeySet keySetInternal) ->
                    keySetInternal.tree |> fill
                )


insert :
    Sorting element key_ tag
    -> element
    ->
        (Emptiable (KeySetFilled element tag) possiblyOrNever_
         -> Emptiable (KeySetFilled element tag) never_
        )
insert sorting elementToInsert =
    \keySet ->
        let
            inserted =
                keySet
                    |> tree
                    |> emptyAdapt (\_ -> Possible)
                    |> treeInsert (elementKeyOrder sorting) elementToInsert

            nextCount =
                if inserted.sizeIncreased then
                    \n -> n + 1

                else
                    identity
        in
        inserted.tree
            |> fillMap
                (\branch ->
                    KeySet { size = keySet |> size |> nextCount, tree = branch |> filled }
                )



-- alter


elementKeyOrder : Sorting element key_ tag_ -> Ordering element
elementKeyOrder =
    \elementUnique ->
        Order.by
            (elementUnique |> Typed.untag |> .key)
            (elementUnique |> Typed.untag |> .keyOrder)


treeInsert :
    Ordering element
    -> element
    ->
        (Emptiable (Branch element) Possibly
         ->
            { sizeIncreased : Bool
            , tree : Emptiable (Branch element) never_
            }
        )
treeInsert ordering elementToInsert tree_ =
    case tree_ |> fillMap filled of
        Empty _ ->
            { sizeIncreased = True
            , tree = Tree2.leaf elementToInsert
            }

        Filled treeFilled ->
            case ordering elementToInsert (treeFilled |> Tree2.trunk) of
                EQ ->
                    { sizeIncreased = False
                    , tree =
                        treeFilled
                            |> Tree2.trunkAlter (\_ -> elementToInsert)
                            |> emptyAdapt never
                    }

                LT ->
                    let
                        insertedForNextLeft =
                            treeFilled
                                |> Tree2.children
                                |> .left
                                |> treeInsert ordering elementToInsert
                    in
                    { sizeIncreased = insertedForNextLeft.sizeIncreased
                    , tree =
                        Tree2.branch
                            (treeFilled |> Tree2.trunk)
                            { left = insertedForNextLeft.tree |> emptyAdapt (\_ -> Possible)
                            , right = treeFilled |> Tree2.children |> .right
                            }
                    }

                GT ->
                    let
                        insertedForNextRight =
                            treeFilled
                                |> Tree2.children
                                |> .right
                                |> treeInsert ordering elementToInsert
                    in
                    { sizeIncreased = insertedForNextRight.sizeIncreased
                    , tree =
                        Tree2.branch
                            (treeFilled |> Tree2.trunk)
                            { left = treeFilled |> Tree2.children |> .left
                            , right = insertedForNextRight.tree |> emptyAdapt (\_ -> Possible)
                            }
                    }


elementRemove :
    Sorting element key tag
    -> key
    ->
        (Emptiable (KeySetFilled element tag) possiblyOrNever_
         -> Emptiable (KeySetFilled element tag) Possibly
        )
elementRemove sorting keyToRemove =
    \keySet ->
        case
            keySet
                |> tree
                |> emptyAdapt (\_ -> Possible)
                |> treeElementRemove (sorting |> Typed.untag) keyToRemove
        of
            Emptiable.Empty _ ->
                keySet |> emptyAdapt (\_ -> Possible)

            Emptiable.Filled nextRoot ->
                nextRoot
                    |> fillMap
                        (\nextBranch ->
                            KeySet
                                { tree = nextBranch |> filled
                                , size = (keySet |> size) - 1
                                }
                        )


treeElementRemove :
    { key : element -> key
    , keyOrder : Ordering key
    }
    -> key
    ->
        (Emptiable (Branch element) Possibly
         -> Emptiable (Emptiable (Branch element) Possibly) Possibly
        )
treeElementRemove keyConfig key =
    \tree_ ->
        tree_
            |> fillMap filled
            |> fillMapFlat
                (\treeFilled ->
                    case keyConfig.keyOrder key (treeFilled |> Tree2.trunk |> keyConfig.key) of
                        LT ->
                            treeFilled
                                |> Tree2.children
                                |> .left
                                |> treeElementRemove keyConfig key
                                |> fillMap
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
                                |> fillMap
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
                                        Tree2.branchUnbalanced
                                            (rightBranch |> filled |> Tree2.end Down)
                                            { left = treeFilled |> Tree2.children |> .left
                                            , right = rightBranch |> filled |> Tree2.endRemove Down
                                            }

                             else
                                case treeFilled |> Tree2.children |> .left of
                                    Empty _ ->
                                        treeFilled |> Tree2.children |> .right

                                    Filled leftBranch ->
                                        Tree2.branchUnbalanced
                                            (leftBranch |> filled |> Tree2.end Up)
                                            { left = leftBranch |> filled |> Tree2.endRemove Up
                                            , right = treeFilled |> Tree2.children |> .right
                                            }
                            )
                                |> filled
                )
