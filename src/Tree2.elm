module Tree2 exposing
    ( Branch, Children
    , one, branch
    , size, height, children, trunk, end
    , childrenDownAlter, childrenUpAlter, removeEnd, map
    , toStack
    , foldFrom, foldFromOne
    , foldUntilCompleteFrom, foldUntilCompleteFromOne
    , foldNavigateFrom
    )

{-| Tree with a branching factor of 2

@docs Branch, Children


## create

`Emptiable.empty`,

@docs one, branch


## scan

@docs size, height, children, trunk, end


## alter

@docs childrenDownAlter, childrenUpAlter, removeEnd, map


## transform

@docs toStack
@docs foldFrom, foldFromOne
@docs foldUntilCompleteFrom, foldUntilCompleteFromOne
@docs foldNavigateFrom

-}

import Emptiable exposing (Emptiable(..), empty, emptyAdapt, fill, filled)
import Linear exposing (Direction(..))
import PartialOrComplete exposing (PartialOrComplete)
import Possibly exposing (Possibly(..))
import Stack exposing (Stacked)


{-| Binary tree with at least one element

Together with [`lue-bird/elm-emptiness-typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/)

    import Emptiable exposing (Emptiable)
    import Possibly exposing (Possibly)

    --- for constructed values

    -- tree can be empty
    Emptiable (Branch ...) Possibly

    -- tree can't be empty
    -- Emptiable (Branch ...) never_

    --- for arguments

    -- tree shouldn't be empty
    Emptiable (Branch ...) Never

    -- tree can be empty or not
    Emptiable (Branch ...) possiblyOrNever_

-}
type Branch element
    = Branch
        { trunk : element
        , children : Children element
        , childrenHeight : Int
        }


{-| 2 sub-trees
-}
type alias Children element =
    { down : Emptiable (Branch element) Possibly
    , up : Emptiable (Branch element) Possibly
    }



-- scan


branchInfo :
    Emptiable (Branch element) Never
    ->
        { trunk : element
        , children : Children element
        , childrenHeight : Int
        }
branchInfo =
    \tree ->
        let
            (Branch branch_) =
                tree |> fill
        in
        branch_


trunk : Emptiable (Branch element) Never -> element
trunk =
    \tree -> tree |> branchInfo |> .trunk


children : Emptiable (Branch element) Never -> Children element
children =
    \tree -> tree |> branchInfo |> .children


childrenHeight : Emptiable (Branch element_) Never -> Int
childrenHeight =
    \tree -> tree |> branchInfo |> .childrenHeight


height : Emptiable (Branch element_) possiblyOrNever_ -> Int
height =
    \tree ->
        case tree of
            Empty _ ->
                0

            Filled branch_ ->
                1 + (branch_ |> filled |> childrenHeight)


{-| Runtime `n`
-}
size : Emptiable (Branch element_) possiblyOrNever_ -> Int
size =
    \tree ->
        case tree of
            Empty _ ->
                0

            Filled branch_ ->
                let
                    children_ : Children element_
                    children_ =
                        branch_ |> filled |> children
                in
                1
                    + (children_ |> .down |> sizeRecurse)
                    + (children_ |> .up |> sizeRecurse)


sizeRecurse : Emptiable (Branch element_) possiblyOrNever_ -> Int
sizeRecurse =
    size



-- create


one : element -> Emptiable (Branch element) never_
one singleElementLeaf =
    Branch
        { trunk = singleElementLeaf
        , children = { down = empty, up = empty }
        , childrenHeight = 0
        }
        |> filled


branchUnbalanced :
    element
    -> Children element
    -> Emptiable (Branch element) never_
branchUnbalanced trunkElement children_ =
    Branch
        { trunk = trunkElement
        , children = children_
        , childrenHeight =
            max
                (children_.down |> height)
                (children_.up |> height)
        }
        |> filled



{-



   branch :
       element
       -> Children element
       -> Emptiable (Branch element) never_
   branch p pChildren =
       case ( pChildren.down, pChildren.up ) of
           ( Emptiable.Empty _, Emptiable.Empty _ ) ->
               one p

           ( Emptiable.Filled (Branch l), Emptiable.Empty _ ) ->
               if l.childrenHeight >= 1 then
                   rotateUp p l.trunk l.children.down l.children.up pChildren.up

               else
                   Branch { childrenHeight = 1 + l.childrenHeight, trunk = p, children = { down = pChildren.down, up = pChildren.up } } |> filled

           ( Emptiable.Empty _, Emptiable.Filled (Branch r) ) ->
               if r.childrenHeight >= 1 then
                   rotateDown p pChildren.down r.trunk r.children.down r.children.up

               else
                   Branch { childrenHeight = 1 + r.childrenHeight, trunk = p, children = { down = pChildren.down, up = pChildren.up } } |> filled

           ( Emptiable.Filled (Branch l), Emptiable.Filled (Branch r) ) ->
               if l.childrenHeight - r.childrenHeight <= -2 then
                   rotateDown p pChildren.down r.trunk r.children.down r.children.up

               else if l.childrenHeight - r.childrenHeight >= 2 then
                   rotateUp p l.trunk l.children.down l.children.up pChildren.up

               else
                   branchUnbalanced p { down = pChildren.down, up = pChildren.up }


   rotateDown :
       element
       -> Emptiable (Branch element) Possibly
       -> element
       -> Emptiable (Branch element) Possibly
       -> Emptiable (Branch element) Possibly
       -> Emptiable (Branch element) never_
   rotateDown pivot pivotDown upTrunk upDown upUp =
       case upDown of
           Emptiable.Empty _ ->
               branchUnbalanced upTrunk { down = filled (Branch { childrenHeight = height pivotDown, trunk = pivot, children = { down = pivotDown, up = empty } }), up = upUp }

           Emptiable.Filled (Branch l) ->
               if l.childrenHeight + 1 > height upUp then
                   branchUnbalanced l.trunk { down = branchUnbalanced pivot { down = pivotDown, up = l.children.down }, up = branchUnbalanced upTrunk { down = l.children.up, up = upUp } }

               else
                   branchUnbalanced upTrunk { down = branchUnbalanced pivot { down = pivotDown, up = upDown }, up = upUp }


   rotateUp :
       element
       -> element
       -> Emptiable (Branch element) Possibly
       -> Emptiable (Branch element) Possibly
       -> Emptiable (Branch element) Possibly
       -> Emptiable (Branch element) never_
   rotateUp pivot downTrunk downDown downUp pivotUp =
       case downUp of
           Emptiable.Empty _ ->
               branchUnbalanced downTrunk { down = downDown, up = filled (Branch { childrenHeight = height pivotUp, trunk = pivot, children = { down = empty, up = pivotUp } }) }

           Emptiable.Filled (Branch r) ->
               if height downDown < r.childrenHeight + 1 then
                   branchUnbalanced r.trunk { down = branchUnbalanced downTrunk { down = downDown, up = r.children.down }, up = branchUnbalanced pivot { down = r.children.up, up = pivotUp } }

               else
                   branchUnbalanced downTrunk { down = downDown, up = branchUnbalanced pivot { down = downUp, up = pivotUp } }

-}


{-| Branch off to sub-trees down and up. Will balance it out
-}
branch :
    element
    -> Children element
    -> Emptiable (Branch element) never_
branch pivotTrunk pivotChildren =
    case
        ( pivotChildren.down |> Emptiable.map filled
        , pivotChildren.up |> Emptiable.map filled
        )
    of
        ( Empty _, Empty _ ) ->
            branchUnbalanced pivotTrunk { down = empty, up = empty }

        ( Filled downFilled, Empty _ ) ->
            if (downFilled |> childrenHeight) >= 1 then
                rotateUp pivotTrunk { down = downFilled, pivotUp = pivotChildren.up }

            else
                branchUnbalanced pivotTrunk pivotChildren

        ( Empty _, Filled upFilled ) ->
            if (upFilled |> childrenHeight) >= 1 then
                rotateDown pivotTrunk { pivotDown = pivotChildren.down, up = upFilled }

            else
                branchUnbalanced pivotTrunk pivotChildren

        ( Filled downFilled, Filled upFilled ) ->
            let
                downToUpImbalance =
                    (downFilled |> childrenHeight) - (upFilled |> childrenHeight)
            in
            if downToUpImbalance <= -2 then
                rotateDown pivotTrunk { pivotDown = pivotChildren.down, up = upFilled }

            else if downToUpImbalance >= 2 then
                rotateUp pivotTrunk { down = downFilled, pivotUp = pivotChildren.up }

            else
                branchUnbalanced pivotTrunk pivotChildren


rotateDown :
    element
    ->
        ({ pivotDown : Emptiable (Branch element) Possibly
         , up : Emptiable (Branch element) Never
         }
         -> Emptiable (Branch element) never_
        )
rotateDown pivotTrunk { pivotDown, up } =
    let
        upChildren : Children element
        upChildren =
            up |> children

        new : { trunk : element, downUp : Emptiable (Branch element) Possibly, up : Emptiable (Branch element) Possibly }
        new =
            case upChildren.down of
                Empty _ ->
                    { trunk = up |> trunk
                    , downUp = empty
                    , up = upChildren.up
                    }

                Filled upDownBranch ->
                    let
                        upDownFilled =
                            upDownBranch |> filled
                    in
                    if (upDownFilled |> height) > (upChildren.up |> height) then
                        { trunk = upDownFilled |> trunk
                        , downUp = upDownFilled |> children |> .down
                        , up =
                            branchUnbalanced
                                (up |> trunk)
                                { down = upDownFilled |> children |> .up
                                , up = upChildren.up
                                }
                        }

                    else
                        { trunk = up |> trunk
                        , downUp = upChildren.down
                        , up = upChildren.up
                        }
    in
    branchUnbalanced
        new.trunk
        { down =
            branchUnbalanced
                pivotTrunk
                { down = pivotDown
                , up = new.downUp
                }
        , up = new.up
        }


rotateUp :
    element
    ->
        ({ down : Emptiable (Branch element) Never
         , pivotUp : Emptiable (Branch element) Possibly
         }
         -> Emptiable (Branch element) never_
        )
rotateUp pivotTrunk { down, pivotUp } =
    let
        downChildren : Children element
        downChildren =
            down |> children

        new : { trunk : element, down : Emptiable (Branch element) Possibly, upDown : Emptiable (Branch element) Possibly }
        new =
            case downChildren.up of
                Empty _ ->
                    { trunk = down |> trunk
                    , down = downChildren.down
                    , upDown = empty
                    }

                Filled downUpBranch ->
                    let
                        downUpFilled =
                            downUpBranch |> filled
                    in
                    if (downChildren.down |> height) < (downUpFilled |> height) then
                        { trunk = downUpFilled |> trunk
                        , down =
                            branchUnbalanced
                                (down |> trunk)
                                { down = downChildren.down
                                , up = downUpFilled |> children |> .down
                                }
                        , upDown = downUpFilled |> children |> .up
                        }

                    else
                        { trunk = down |> trunk
                        , down = downChildren.down
                        , upDown = downChildren.up
                        }
    in
    branchUnbalanced
        new.trunk
        { down = new.down
        , up =
            branchUnbalanced
                pivotTrunk
                { down = new.upDown
                , up = pivotUp
                }
        }



-- scan


end :
    Linear.Direction
    ->
        (Emptiable (Branch element) Never
         -> element
        )
end direction =
    case direction of
        Up ->
            endUp

        Down ->
            endDown


endDown : Emptiable (Branch element) Never -> element
endDown =
    \tree ->
        case tree |> children |> .down of
            Empty _ ->
                tree |> trunk

            Filled downBranch ->
                downBranch |> filled |> endDown


endUp : Emptiable (Branch element) Never -> element
endUp =
    \tree ->
        case tree |> children |> .up of
            Empty _ ->
                tree |> trunk

            Filled upBranch ->
                upBranch |> filled |> endUp



-- alter


childrenUpAlter :
    (Emptiable (Branch element) Possibly
     -> Emptiable (Branch element) possiblyOrNever_
    )
    ->
        (Emptiable (Branch element) Never
         -> Emptiable (Branch element) never_
        )
childrenUpAlter childrenUpChange =
    \treeFilled ->
        let
            children_ : Children element
            children_ =
                treeFilled |> children
        in
        branch
            (treeFilled |> trunk)
            { children_
                | up =
                    children_
                        |> .up
                        |> childrenUpChange
                        |> Emptiable.emptyAdapt (\_ -> Possible)
            }


childrenDownAlter :
    (Emptiable (Branch element) Possibly
     -> Emptiable (Branch element) possiblyOrNever_
    )
    ->
        (Emptiable (Branch element) Never
         -> Emptiable (Branch element) never_
        )
childrenDownAlter childrenDownChange =
    \treeFilled ->
        let
            children_ : Children element
            children_ =
                treeFilled |> children
        in
        branch
            (treeFilled |> trunk)
            { children_
                | down =
                    children_
                        |> .down
                        |> childrenDownChange
                        |> Emptiable.emptyAdapt (\_ -> Possible)
            }


removeEnd :
    Linear.Direction
    ->
        (Emptiable (Branch element) Never
         -> Emptiable (Branch element) Possibly
        )
removeEnd direction =
    case direction of
        Up ->
            endRemoveUp

        Down ->
            endRemoveDown


endRemoveDown :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) Possibly
endRemoveDown =
    \tree ->
        let
            children_ : Children element
            children_ =
                tree |> children
        in
        case children_.down of
            Empty _ ->
                children_.up

            Filled downBranch ->
                branch
                    (tree |> trunk)
                    { children_
                        | down = downBranch |> filled |> endRemoveDown
                    }


endRemoveUp :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) Possibly
endRemoveUp =
    \tree ->
        let
            children_ : Children element
            children_ =
                tree |> children
        in
        case children_.up of
            Empty _ ->
                children_.down

            Filled upBranch ->
                branch
                    (tree |> trunk)
                    { children_
                        | up = upBranch |> filled |> endRemoveUp
                    }


map :
    (element -> mappedElement)
    -> Emptiable (Branch element) possiblyOrNever
    -> Emptiable (Branch mappedElement) possiblyOrNever
map elementChange =
    \tree ->
        tree
            |> Emptiable.map
                (\branch_ -> branch_ |> branchMap elementChange)


branchMap : (element -> mappedElement) -> (Branch element -> Branch mappedElement)
branchMap elementChange =
    \(Branch branch_) ->
        Branch
            { trunk = branch_.trunk |> elementChange
            , childrenHeight = branch_.childrenHeight
            , children = branch_.children |> childrenMap elementChange
            }


childrenMap : (element -> mappedElement) -> (Children element -> Children mappedElement)
childrenMap elementChange =
    \children_ ->
        { down = children_.down |> map elementChange
        , up = children_.up |> map elementChange
        }



-- transform


foldFromOne :
    (element -> accumulated)
    -> Linear.Direction
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (Branch element) Never
         -> accumulated
        )
foldFromOne firstToInitial direction reduce =
    \tree ->
        tree
            |> removeEnd (direction |> Linear.directionOpposite)
            |> foldFrom
                (tree
                    |> end (direction |> Linear.directionOpposite)
                    |> firstToInitial
                )
                direction
                reduce


toStack :
    Emptiable (Branch element) possiblyOrNever
    -> Emptiable (Stacked element) possiblyOrNever
toStack =
    \tree ->
        tree
            |> Emptiable.mapFlat
                (\branch_ ->
                    branch_ |> filled |> foldFromOne Stack.one Down Stack.onTopLay
                )


foldFrom :
    accumulated
    -> Linear.Direction
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (Branch element) possiblyOrNever_
         -> accumulated
        )
foldFrom initial direction reduce =
    \tree ->
        let
            treePossiblyEmpty =
                tree |> emptyAdapt (\_ -> Possible)
        in
        case direction of
            Up ->
                treePossiblyEmpty |> foldUpFrom initial reduce

            Down ->
                treePossiblyEmpty |> foldDownFrom initial reduce


foldUpFrom :
    accumulated
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (Branch element) Possibly
         -> accumulated
        )
foldUpFrom initial reduce =
    \tree ->
        case tree of
            Empty _ ->
                initial

            Filled branch_ ->
                let
                    treeFilled =
                        branch_ |> filled
                in
                treeFilled
                    |> children
                    |> .up
                    |> foldUpFrom
                        (treeFilled
                            |> children
                            |> .down
                            |> foldUpFrom initial reduce
                            |> reduce (treeFilled |> trunk)
                        )
                        reduce


foldDownFrom :
    accumulated
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (Branch element) Possibly
         -> accumulated
        )
foldDownFrom initial reduce =
    \tree ->
        case tree of
            Empty _ ->
                initial

            Filled branch_ ->
                let
                    treeFilled : Emptiable (Branch element) never_
                    treeFilled =
                        branch_ |> filled
                in
                treeFilled
                    |> children
                    |> .down
                    |> foldDownFrom
                        (treeFilled
                            |> children
                            |> .up
                            |> foldDownFrom initial reduce
                            |> reduce (treeFilled |> trunk)
                        )
                        reduce


foldNavigateFrom :
    folded
    ->
        ({ trunk : element, children : { down : () -> folded, up : () -> folded } }
         -> folded
        )
    -> (Emptiable (Branch element) Possibly -> folded)
foldNavigateFrom initial reduce =
    \tree ->
        case tree of
            Empty _ ->
                initial

            Filled branch_ ->
                let
                    treeFilled : Emptiable (Branch element) never_
                    treeFilled =
                        branch_ |> filled
                in
                { trunk = treeFilled |> trunk
                , children =
                    { down =
                        \() ->
                            treeFilled
                                |> children
                                |> .down
                                |> foldNavigateFrom initial reduce
                    , up =
                        \() ->
                            treeFilled
                                |> children
                                |> .up
                                |> foldNavigateFrom initial reduce
                    }
                }
                    |> reduce


foldUntilCompleteUpFrom :
    foldedPartial
    -> (element -> (foldedPartial -> PartialOrComplete foldedPartial foldedComplete))
    ->
        (Emptiable (Branch element) Possibly
         -> PartialOrComplete foldedPartial foldedComplete
        )
foldUntilCompleteUpFrom initialFoldedPartial reduceStep =
    \tree2 ->
        case tree2 of
            Emptiable.Empty _ ->
                initialFoldedPartial |> PartialOrComplete.Partial

            Emptiable.Filled branch_ ->
                let
                    treeFilled : Emptiable (Branch element) never_
                    treeFilled =
                        branch_ |> filled
                in
                (treeFilled |> children |> .down)
                    |> foldUntilCompleteUpFrom initialFoldedPartial reduceStep
                    |> PartialOrComplete.onPartialMapFlat
                        (\afterDownPartial ->
                            afterDownPartial
                                |> reduceStep (treeFilled |> trunk)
                                |> PartialOrComplete.onPartialMapFlat
                                    (\afterDownAndTrunkPartial ->
                                        (treeFilled |> children |> .up)
                                            |> foldUntilCompleteUpFrom afterDownAndTrunkPartial reduceStep
                                    )
                        )


foldUntilCompleteDownFrom :
    foldedPartial
    -> (element -> (foldedPartial -> PartialOrComplete foldedPartial foldedComplete))
    ->
        (Emptiable (Branch element) Possibly
         -> PartialOrComplete foldedPartial foldedComplete
        )
foldUntilCompleteDownFrom initialFolded reduceStep =
    \tree2 ->
        case tree2 of
            Emptiable.Empty _ ->
                initialFolded |> PartialOrComplete.Partial

            Emptiable.Filled branch_ ->
                let
                    treeFilled : Emptiable (Branch element) never_
                    treeFilled =
                        branch_ |> filled
                in
                (treeFilled |> children |> .up)
                    |> foldUntilCompleteDownFrom initialFolded reduceStep
                    |> PartialOrComplete.onPartialMapFlat
                        (\afterDownPartial ->
                            afterDownPartial
                                |> reduceStep (treeFilled |> trunk)
                                |> PartialOrComplete.onPartialMapFlat
                                    (\afterDownAndTrunkPartial ->
                                        (treeFilled |> children |> .down)
                                            |> foldUntilCompleteDownFrom afterDownAndTrunkPartial reduceStep
                                    )
                        )


foldUntilCompleteFrom :
    foldedPartial
    -> Linear.Direction
    ->
        (element
         -> (foldedPartial -> PartialOrComplete foldedPartial foldedComplete)
        )
    ->
        (Emptiable (Branch element) Possibly
         -> PartialOrComplete foldedPartial foldedComplete
        )
foldUntilCompleteFrom initialFoldedPartial direction reduceStep =
    case direction of
        Down ->
            \tree_ -> tree_ |> foldUntilCompleteDownFrom initialFoldedPartial reduceStep

        Up ->
            \tree_ -> tree_ |> foldUntilCompleteUpFrom initialFoldedPartial reduceStep


foldUntilCompleteFromOne :
    (element -> PartialOrComplete foldedPartial foldedComplete)
    -> Linear.Direction
    ->
        (element
         -> (foldedPartial -> PartialOrComplete foldedPartial foldedComplete)
        )
    ->
        (Emptiable (Branch element) Never
         -> PartialOrComplete foldedPartial foldedComplete
        )
foldUntilCompleteFromOne startElementStep direction reduceStep =
    case direction of
        Down ->
            \tree_ -> tree_ |> foldUntilCompleteDownFromOne startElementStep reduceStep

        Up ->
            \tree_ -> tree_ |> foldUntilCompleteUpFromOne startElementStep reduceStep


foldUntilCompleteUpFromOne :
    (element -> PartialOrComplete foldedPartial foldedComplete)
    -> (element -> (foldedPartial -> PartialOrComplete foldedPartial foldedComplete))
    ->
        (Emptiable (Branch element) Never
         -> PartialOrComplete foldedPartial foldedComplete
        )
foldUntilCompleteUpFromOne startElementStep reduceStep =
    \treeFilled ->
        let
            foldedAfterDownAndTrunk : PartialOrComplete foldedPartial foldedComplete
            foldedAfterDownAndTrunk =
                case treeFilled |> children |> .down of
                    Emptiable.Empty Possible ->
                        treeFilled |> trunk |> startElementStep

                    Emptiable.Filled downBranch ->
                        downBranch
                            |> filled
                            |> foldUntilCompleteUpFromOne startElementStep reduceStep
                            |> PartialOrComplete.onPartialMapFlat
                                (\afterDownPartial ->
                                    afterDownPartial |> reduceStep (treeFilled |> trunk)
                                )
        in
        foldedAfterDownAndTrunk
            |> PartialOrComplete.onPartialMapFlat
                (\afterDownAndTrunkPartial ->
                    (treeFilled |> children |> .up)
                        |> foldUntilCompleteUpFrom afterDownAndTrunkPartial reduceStep
                )


foldUntilCompleteDownFromOne :
    (element -> PartialOrComplete foldedPartial foldedComplete)
    -> (element -> (foldedPartial -> PartialOrComplete foldedPartial foldedComplete))
    ->
        (Emptiable (Branch element) Never
         -> PartialOrComplete foldedPartial foldedComplete
        )
foldUntilCompleteDownFromOne startElementStep reduceStep =
    \treeFilled ->
        let
            foldedAfterUpAndTrunk =
                case treeFilled |> children |> .up of
                    Emptiable.Empty Possible ->
                        treeFilled |> trunk |> startElementStep

                    Emptiable.Filled upBranch ->
                        upBranch
                            |> filled
                            |> foldUntilCompleteDownFromOne startElementStep reduceStep
                            |> PartialOrComplete.onPartialMapFlat
                                (\afterDownPartial ->
                                    afterDownPartial |> reduceStep (treeFilled |> trunk)
                                )
        in
        foldedAfterUpAndTrunk
            |> PartialOrComplete.onPartialMapFlat
                (\afterDownAndTrunkPartial ->
                    (treeFilled |> children |> .down)
                        |> foldUntilCompleteDownFrom afterDownAndTrunkPartial reduceStep
                )
