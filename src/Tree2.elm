module Tree2 exposing
    ( Branch, Children
    , one, branch
    , size, height, children, trunk, end
    , trunkAlter, childrenLeftAlter, childrenRightAlter, removeEnd
    , foldFrom, foldFromOne
    )

{-| Tree with a branching factor of 2

@docs Branch, Children


## create

`Emptiable.empty`,

@docs one, branch


## scan

@docs size, height, children, trunk, end


## alter

@docs trunkAlter, childrenLeftAlter, childrenRightAlter, removeEnd


## transform

@docs foldFrom, foldFromOne

-}

import Emptiable exposing (Emptiable(..), empty, emptyAdapt, fill, filled)
import Linear exposing (Direction(..))
import Possibly exposing (Possibly(..))


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
        { element : element
        , children : Children element
        , childrenHeight : Int
        }


{-| 2 sub-trees
-}
type alias Children element =
    { left : Emptiable (Branch element) Possibly
    , right : Emptiable (Branch element) Possibly
    }



--


trunk : Emptiable (Branch element) Never -> element
trunk =
    \tree ->
        let
            (Branch branch_) =
                tree |> fill
        in
        branch_.element


children : Emptiable (Branch element) Never -> Children element
children =
    \tree ->
        let
            (Branch branch_) =
                tree |> fill
        in
        branch_.children


childrenHeight : Emptiable (Branch element_) Never -> Int
childrenHeight =
    \tree ->
        let
            (Branch branch_) =
                tree |> fill
        in
        branch_.childrenHeight


height : Emptiable (Branch element_) possiblyOrNever_ -> Int
height =
    \tree ->
        case tree |> Emptiable.map filled of
            Empty _ ->
                0

            Filled treeFilled ->
                1 + (treeFilled |> childrenHeight)


{-| Runtime `n`
-}
size : Emptiable (Branch element_) possiblyOrNever_ -> Int
size =
    \tree ->
        case tree |> Emptiable.map filled of
            Empty _ ->
                0

            Filled treeFilled ->
                1
                    + (treeFilled |> children |> .left |> sizeRecurse)
                    + (treeFilled |> children |> .right |> sizeRecurse)


sizeRecurse : Emptiable (Branch element_) possiblyOrNever_ -> Int
sizeRecurse =
    size



-- create


branchUnbalanced :
    element
    -> Children element
    -> Emptiable (Branch element) never_
branchUnbalanced trunkElement children_ =
    Branch
        { element = trunkElement
        , children = children_
        , childrenHeight =
            max
                (children_.left |> height)
                (children_.right |> height)
        }
        |> filled


one : element -> Emptiable (Branch element) never_
one singleElementLeaf =
    branchUnbalanced
        singleElementLeaf
        { left = empty
        , right = empty
        }


{-| Branch off to sub-trees left and right. Will balance it out
-}
branch :
    element
    -> Children element
    -> Emptiable (Branch element) never_
branch pivotTrunk pivotChildren =
    case
        ( pivotChildren.left |> Emptiable.map filled
        , pivotChildren.right |> Emptiable.map filled
        )
    of
        ( Empty _, Empty _ ) ->
            branchUnbalanced pivotTrunk { left = empty, right = empty }

        ( Filled leftFilled, Empty _ ) ->
            if (leftFilled |> childrenHeight) >= 1 then
                rotateRight pivotTrunk leftFilled pivotChildren.right

            else
                branchUnbalanced pivotTrunk pivotChildren

        ( Empty _, Filled rightFilled ) ->
            if (rightFilled |> childrenHeight) >= 1 then
                rotateLeft pivotTrunk pivotChildren.left rightFilled

            else
                branchUnbalanced pivotTrunk pivotChildren

        ( Filled leftFilled, Filled rightFilled ) ->
            let
                leftToRightImbalance =
                    (leftFilled |> childrenHeight) - (rightFilled |> childrenHeight)
            in
            if leftToRightImbalance <= -2 then
                rotateLeft pivotTrunk pivotChildren.left rightFilled

            else if leftToRightImbalance >= 2 then
                rotateRight pivotTrunk leftFilled pivotChildren.right

            else
                branchUnbalanced pivotTrunk pivotChildren


rotateLeft :
    element
    -> Emptiable (Branch element) Possibly
    -> Emptiable (Branch element) Never
    -> Emptiable (Branch element) never_
rotateLeft pivotTrunk pivotLeft right =
    let
        new =
            case right |> children |> .left |> Emptiable.map filled of
                Empty _ ->
                    { trunk = right |> trunk
                    , leftRight = empty
                    , right = right |> children |> .right
                    }

                Filled rightLeftFilled ->
                    if (rightLeftFilled |> height) > (right |> children |> .right |> height) then
                        { trunk = rightLeftFilled |> trunk
                        , leftRight =
                            rightLeftFilled |> children |> .left
                        , right =
                            branchUnbalanced
                                (right |> trunk)
                                { left = rightLeftFilled |> children |> .right
                                , right = right |> children |> .right
                                }
                        }

                    else
                        { trunk = right |> trunk
                        , leftRight = right |> children |> .left
                        , right = right |> children |> .right
                        }
    in
    branchUnbalanced
        new.trunk
        { left =
            branchUnbalanced
                pivotTrunk
                { left = pivotLeft
                , right = new.leftRight
                }
        , right = new.right
        }


rotateRight :
    element
    -> Emptiable (Branch element) Never
    -> Emptiable (Branch element) Possibly
    -> Emptiable (Branch element) never_
rotateRight pivotTrunk left pivotRight =
    let
        new =
            case left |> children |> .right |> Emptiable.map filled of
                Empty _ ->
                    { trunk = left |> trunk
                    , left = left |> children |> .left
                    , rightLeft = empty
                    }

                Filled leftRightFilled ->
                    if (left |> children |> .left |> height) < (leftRightFilled |> height) then
                        { trunk = leftRightFilled |> trunk
                        , left =
                            branchUnbalanced
                                (left |> trunk)
                                { left = left |> children |> .left
                                , right = leftRightFilled |> children |> .left
                                }
                        , rightLeft = leftRightFilled |> children |> .right
                        }

                    else
                        { trunk = left |> trunk
                        , left = left |> children |> .left
                        , rightLeft = left |> children |> .right
                        }
    in
    branchUnbalanced
        new.trunk
        { left = new.left
        , right =
            branchUnbalanced pivotTrunk
                { left = new.rightLeft
                , right = pivotRight
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
        case tree |> children |> .left of
            Empty _ ->
                tree |> trunk

            Filled leftBranch ->
                leftBranch |> filled |> endDown


endUp : Emptiable (Branch element) Never -> element
endUp =
    \tree ->
        case tree |> children |> .right of
            Empty _ ->
                tree |> trunk

            Filled rightBranch ->
                rightBranch |> filled |> endUp



-- alter


trunkAlter :
    (element -> element)
    ->
        (Emptiable (Branch element) possiblyOrNever
         -> Emptiable (Branch element) possiblyOrNever
        )
trunkAlter elementChange =
    \tree ->
        tree
            |> Emptiable.map filled
            |> Emptiable.map
                (\treeFilled ->
                    { element = treeFilled |> trunk |> elementChange
                    , children = treeFilled |> children
                    , childrenHeight = treeFilled |> childrenHeight
                    }
                        |> Branch
                )


childrenRightAlter :
    (Emptiable (Branch element) Possibly
     -> Emptiable (Branch element) possiblyOrNever_
    )
    ->
        (Emptiable (Branch element) Never
         -> Emptiable (Branch element) never_
        )
childrenRightAlter childrenRightChange =
    \treeFilled ->
        let
            children_ : Children element
            children_ =
                treeFilled |> children
        in
        branch
            (treeFilled |> trunk)
            { children_
                | right =
                    children_
                        |> .right
                        |> childrenRightChange
                        |> Emptiable.emptyAdapt (\_ -> Possible)
            }


childrenLeftAlter :
    (Emptiable (Branch element) Possibly
     -> Emptiable (Branch element) possiblyOrNever_
    )
    ->
        (Emptiable (Branch element) Never
         -> Emptiable (Branch element) never_
        )
childrenLeftAlter childrenLeftChange =
    \treeFilled ->
        let
            children_ : Children element
            children_ =
                treeFilled |> children
        in
        branch
            (treeFilled |> trunk)
            { children_
                | left =
                    children_
                        |> .left
                        |> childrenLeftChange
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
            children_ =
                tree |> children
        in
        case children_.left of
            Empty _ ->
                children_.right

            Filled leftBranch ->
                branch
                    (tree |> trunk)
                    { children_
                        | left = leftBranch |> filled |> endRemoveDown
                    }


endRemoveUp :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) Possibly
endRemoveUp =
    \tree ->
        let
            children_ =
                tree |> children
        in
        case children_.right of
            Empty _ ->
                children_.left

            Filled rightBranch ->
                branch
                    (tree |> trunk)
                    { children_
                        | right = rightBranch |> filled |> endRemoveUp
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
            |> removeEnd (direction |> Linear.opposite)
            |> foldFrom
                (tree
                    |> end (direction |> Linear.opposite)
                    |> firstToInitial
                )
                direction
                reduce


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
        case tree |> Emptiable.map filled of
            Empty _ ->
                initial

            Filled treeFilled ->
                treeFilled
                    |> children
                    |> .right
                    |> foldUpFrom
                        (treeFilled
                            |> children
                            |> .left
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
        case tree |> Emptiable.map filled of
            Empty _ ->
                initial

            Filled treeFilled ->
                treeFilled
                    |> children
                    |> .left
                    |> foldDownFrom
                        (treeFilled
                            |> children
                            |> .right
                            |> foldDownFrom initial reduce
                            |> reduce (treeFilled |> trunk)
                        )
                        reduce
