module Tree2.Sorted exposing
    ( treeElement, treeExcept, treeInsertIfNoCollision, treeRemove
    , fold2FromOne, fold2From
    )

{-| sorted tree

@docs treeElement, treeExcept, treeInsertIfNoCollision, treeRemove
@docs fold2FromOne, fold2From

-}

import And exposing (And)
import AndOr exposing (AndOr)
import Emptiable exposing (Emptiable(..), filled)
import Linear exposing (Direction(..))
import Or
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)
import Tree2


{-| The argument should tell where to search further
-}
treeElement :
    (element -> Order)
    ->
        (Emptiable (Tree2.Branch element) Possibly
         -> Emptiable element Possibly
        )
treeElement location =
    \tree_ ->
        tree_
            |> Emptiable.mapFlat
                (\branch ->
                    let
                        treeFilled =
                            branch |> filled
                    in
                    case treeFilled |> Tree2.trunk |> location of
                        EQ ->
                            treeFilled |> Tree2.trunk |> filled

                        LT ->
                            treeFilled |> Tree2.children |> .down |> treeElement location

                        GT ->
                            treeFilled |> Tree2.children |> .up |> treeElement location
                )


treeInsertIfNoCollision :
    (( element, element ) -> Order)
    -> element
    ->
        (Emptiable (Tree2.Branch element) Possibly
         -> Emptiable (Tree2.Branch element) never_
        )
treeInsertIfNoCollision order toInsert =
    \tree_ ->
        case tree_ of
            Empty _ ->
                toInsert |> Tree2.one

            Filled branch ->
                let
                    treeFilled =
                        branch |> filled
                in
                case ( toInsert, treeFilled |> Tree2.trunk ) |> order of
                    EQ ->
                        -- to alter
                        -- treeFilled
                        --     |> Emptiable.emptyAdapt never
                        --     |> Tree2.trunkAlter (\_ -> elementToInsert)
                        treeFilled |> Emptiable.emptyAdapt never

                    LT ->
                        treeFilled
                            |> Tree2.childrenDownAlter
                                (\down -> down |> treeInsertIfNoCollision order toInsert)

                    GT ->
                        treeFilled
                            |> Tree2.childrenUpAlter
                                (\up -> up |> treeInsertIfNoCollision order toInsert)


{-| The argument should tell where to search further
-}
treeRemove :
    (element -> Order)
    ->
        (Emptiable (Tree2.Branch element) Never
         -> Emptiable (Tree2.Branch element) Possibly
        )
treeRemove locationToRemove =
    \treeFilled ->
        case treeFilled |> Tree2.trunk |> locationToRemove of
            LT ->
                treeFilled
                    |> Tree2.childrenDownAlter
                        (\down ->
                            down
                                |> Emptiable.mapFlat
                                    (\downBranch ->
                                        downBranch
                                            |> filled
                                            |> treeRemove locationToRemove
                                    )
                        )

            GT ->
                treeFilled
                    |> Tree2.childrenUpAlter
                        (\up ->
                            up
                                |> Emptiable.mapFlat
                                    (\upBranch ->
                                        upBranch
                                            |> filled
                                            |> treeRemove locationToRemove
                                    )
                        )

            EQ ->
                let
                    children =
                        treeFilled |> Tree2.children
                in
                if
                    (children.down |> Tree2.height)
                        < (children.up |> Tree2.height)
                then
                    case children.up of
                        Empty _ ->
                            children.down

                        Filled upBranch ->
                            let
                                upTreeFilled =
                                    upBranch |> filled
                            in
                            Tree2.branch
                                (upTreeFilled |> Tree2.end Down)
                                { children
                                    | up = upTreeFilled |> Tree2.removeEnd Down
                                }

                else
                    -- down >= up
                    case children.down of
                        Empty _ ->
                            children.up

                        Filled downBranch ->
                            let
                                downTreeFilled =
                                    downBranch |> filled
                            in
                            Tree2.branch
                                (downTreeFilled |> Tree2.end Up)
                                { children
                                    | down = downTreeFilled |> Tree2.removeEnd Up
                                }


treeExcept :
    (( element, element ) -> Order)
    -> Emptiable (Tree2.Branch element) exceptionsPossiblyOrNever_
    ->
        (Emptiable (Tree2.Branch element) possiblyOrNever_
         -> Emptiable (Tree2.Branch element) Possibly
        )
treeExcept order exceptions =
    \tree ->
        tree
            |> Tree2.foldFrom
                (exceptions |> Emptiable.emptyAdapt (\_ -> Possible))
                Up
                (\collision soFar ->
                    soFar
                        |> Emptiable.mapFlat
                            (\branch ->
                                branch
                                    |> filled
                                    |> treeRemove (\el -> ( collision, el ) |> order)
                            )
                )


fold2FromOne :
    (( key, key ) -> Order)
    -> (AndOr firstElement secondElement -> accumulated)
    ->
        (AndOr firstElement secondElement
         -> (accumulated -> accumulated)
        )
    ->
        (And
            { key : firstElement -> key
            , tree : Emptiable (Tree2.Branch firstElement) Never
            }
            { key : secondElement -> key
            , tree :
                Emptiable (Tree2.Branch secondElement) secondPossiblyOrNever_
            }
         -> accumulated
        )
fold2FromOne keyOrder startToInitialFolded reduce ( first, second ) =
    let
        secondAccumulate :
            firstElement
            ->
                ({ secondRemainder : Emptiable (Stacked secondElement) Possibly
                 , accumulated : Maybe accumulated
                 }
                 ->
                    { secondRemainder : Emptiable (Stacked secondElement) Possibly
                    , accumulated : accumulated
                    }
                )
        secondAccumulate firstElement =
            \soFar ->
                let
                    reduceOrInitializeWith with =
                        case soFar.accumulated of
                            Nothing ->
                                startToInitialFolded with

                            Just soFarAccumulated ->
                                soFarAccumulated |> reduce with
                in
                case soFar.secondRemainder of
                    Emptiable.Empty _ ->
                        { secondRemainder = Emptiable.empty
                        , accumulated =
                            reduceOrInitializeWith (AndOr.Only (Or.First firstElement))
                        }

                    Emptiable.Filled secondStacked ->
                        let
                            secondStackFilled : Emptiable (Stacked secondElement) never_
                            secondStackFilled =
                                secondStacked |> filled
                        in
                        case
                            ( firstElement |> first.key
                            , secondStackFilled |> Stack.top |> second.key
                            )
                                |> keyOrder
                        of
                            EQ ->
                                { secondRemainder = secondStackFilled |> Stack.removeTop
                                , accumulated =
                                    reduceOrInitializeWith (AndOr.Both ( firstElement, secondStackFilled |> Stack.top ))
                                }

                            LT ->
                                { secondRemainder = secondStackFilled |> Emptiable.emptyAdapt (\_ -> Possible)
                                , accumulated =
                                    reduceOrInitializeWith (AndOr.Only (Or.First firstElement))
                                }

                            GT ->
                                secondAccumulate firstElement
                                    { secondRemainder = secondStackFilled |> Stack.removeTop
                                    , accumulated =
                                        reduceOrInitializeWith (AndOr.Only (Or.Second (secondStackFilled |> Stack.top)))
                                            |> Just
                                    }

        secondAccumulated :
            { secondRemainder : Emptiable (Stacked secondElement) Possibly
            , accumulated : accumulated
            }
        secondAccumulated =
            first.tree
                |> Tree2.foldFromOne
                    (\firstStart ->
                        secondAccumulate firstStart
                            { secondRemainder =
                                second.tree
                                    |> Tree2.toStack
                                    |> Emptiable.emptyAdapt (\_ -> Possible)
                            , accumulated = Nothing
                            }
                    )
                    Up
                    (\firstElement soFar ->
                        secondAccumulate firstElement
                            { accumulated = soFar.accumulated |> Just
                            , secondRemainder = soFar.secondRemainder
                            }
                    )
    in
    secondAccumulated.secondRemainder
        |> Stack.foldFrom secondAccumulated.accumulated
            Up
            (\secondRemainderElement soFar ->
                soFar |> reduce (AndOr.Only (Or.Second secondRemainderElement))
            )


fold2From :
    (( key, key ) -> Order)
    -> folded
    ->
        (AndOr firstElement secondElement
         -> (folded -> folded)
        )
    ->
        (And
            { key : firstElement -> key
            , tree : Emptiable (Tree2.Branch firstElement) firstPossiblyOrNever_
            }
            { key : secondElement -> key
            , tree :
                Emptiable (Tree2.Branch secondElement) secondPossiblyOrNever_
            }
         -> folded
        )
fold2From keyOrder initial reduce =
    \( first, second ) ->
        case first.tree of
            Emptiable.Empty _ ->
                second.tree
                    |> Tree2.foldFrom initial
                        Up
                        (\el -> el |> Or.Second |> AndOr.Only |> reduce)

            Emptiable.Filled firstBranch ->
                ( { tree = firstBranch |> filled
                  , key = first.key
                  }
                , second
                )
                    |> fold2FromOne
                        keyOrder
                        (\start -> initial |> reduce start)
                        reduce
