module Tree2 exposing
    ( Branch, Children
    , leaf, branch, branchUnbalanced
    , height, children, trunk, end
    , trunkAlter, endRemove
    , foldFrom, foldOnto
    )

{-| Binary tree

@docs Branch, Children


## create

`Emptiable.empty`,

@docs leaf, branch, branchUnbalanced


## scan

@docs height, children, trunk, end


## alter

@docs trunkAlter, endRemove


## transform

@docs foldFrom, foldOnto

-}

import Emptiable exposing (Emptiable(..), empty, emptyAdapt, fill, fillMap, filled)
import Linear exposing (Direction(..))
import Possibly exposing (Possibly(..))


{-| Binary tree with at least one element

Together with [`lue-bird/elm-emptiness-typed`](https://dark.elm.dmy.fr/packages/lue-bird/elm-emptiness-typed/latest/)

    import Emptiable exposing (Emptiable)
    import Possibly exposing (Possibly)ä

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
        , height : Int
        }


{-| 2 sub-trees
-}
type alias Children element =
    { left : Emptiable (Branch element) Possibly
    , right : Emptiable (Branch element) Possibly
    }



--


height : Emptiable (Branch element_) possiblyOrNever_ -> Int
height =
    \tree ->
        case tree of
            Empty _ ->
                0

            Filled (Branch branch_) ->
                branch_.height


children : Emptiable (Branch element) Never -> Children element
children =
    \tree ->
        let
            (Branch branch_) =
                tree |> fill
        in
        branch_.children


trunk : Emptiable (Branch element) Never -> element
trunk =
    \tree ->
        let
            (Branch branch_) =
                tree |> fill
        in
        branch_.element



-- create


{-| Will balance it out. Don't need to? → [`branchUnbalanced`](#branchUnbalanced)
-}
branch :
    element
    -> Children element
    -> Emptiable (Branch element) never_
branch trunkElement children_ =
    branchUnbalanced trunkElement children_
        |> balance


balance :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) never_
balance =
    \tree ->
        case
            ( tree |> children |> .left |> fillMap filled
            , tree |> children |> .right |> fillMap filled
            )
        of
            ( Empty _, Empty _ ) ->
                leaf (tree |> trunk)

            ( Filled childrenLeft, Empty _ ) ->
                if (childrenLeft |> height) >= 2 then
                    rotateRight
                        (tree |> trunk)
                        (childrenLeft |> trunk)
                        (childrenLeft |> children)
                        empty

                else
                    branchUnbalanced
                        (tree |> trunk)
                        { left = childrenLeft |> emptyAdapt (\_ -> Possible)
                        , right = empty
                        }

            ( Empty _, Filled childrenRight ) ->
                if (childrenRight |> height) >= 2 then
                    rotateLeft
                        (tree |> trunk)
                        empty
                        (childrenRight |> trunk)
                        (childrenRight |> children)

                else
                    branchUnbalanced
                        (tree |> trunk)
                        { left = empty
                        , right = childrenRight |> emptyAdapt (\_ -> Possible)
                        }

            ( Filled childrenLeft, Filled childrenRight ) ->
                let
                    leftMinusRight =
                        (childrenLeft |> height) - (childrenRight |> height)
                in
                if leftMinusRight <= -2 then
                    rotateLeft
                        (tree |> trunk)
                        (childrenLeft |> emptyAdapt (\_ -> Possible))
                        (childrenRight |> trunk)
                        (childrenRight |> children)

                else if leftMinusRight >= 2 then
                    rotateRight
                        (tree |> trunk)
                        (childrenLeft |> trunk)
                        (childrenLeft |> children)
                        (childrenRight |> emptyAdapt (\_ -> Possible))

                else
                    tree |> emptyAdapt never


rotateLeft :
    element
    -> Emptiable (Branch element) Possibly
    -> element
    -> Children element
    -> Emptiable (Branch element) never_
rotateLeft pivotTrunk pivotLeft rightTrunk rightChildren =
    case rightChildren.left |> fillMap filled of
        Empty _ ->
            branchUnbalanced
                pivotTrunk
                { left =
                    branchUnbalanced
                        pivotTrunk
                        { left = pivotLeft
                        , right = empty
                        }
                , right = rightChildren.right
                }

        Filled rightLeft ->
            if (rightLeft |> height) >= (rightChildren.right |> height) then
                branchUnbalanced
                    rightTrunk
                    { left =
                        branchUnbalanced
                            pivotTrunk
                            { left = pivotLeft
                            , right = rightChildren.left
                            }
                    , right = rightChildren.right
                    }

            else
                branchUnbalanced
                    (rightLeft |> trunk)
                    { left =
                        branchUnbalanced
                            pivotTrunk
                            { left = pivotLeft
                            , right = rightLeft |> children |> .left
                            }
                    , right =
                        branchUnbalanced
                            rightTrunk
                            { left = rightLeft |> children |> .right
                            , right = rightChildren.right
                            }
                    }


rotateRight :
    element
    -> element
    -> Children element
    ->
        (Emptiable (Branch element) Possibly
         -> Emptiable (Branch element) never_
        )
rotateRight pivotTrunk leftTrunk leftChildren pivotRight =
    case leftChildren.right |> fillMap filled of
        Empty _ ->
            branchUnbalanced
                leftTrunk
                { left = leftChildren.left
                , right =
                    branchUnbalanced
                        pivotTrunk
                        { left = empty
                        , right = pivotRight
                        }
                }

        Filled leftRight ->
            if (leftChildren.left |> height) >= (leftRight |> height) then
                branchUnbalanced
                    leftTrunk
                    { left = leftChildren.left
                    , right =
                        branchUnbalanced
                            pivotTrunk
                            { left = leftChildren.right
                            , right = pivotRight
                            }
                    }

            else
                branchUnbalanced
                    (leftRight |> trunk)
                    { left =
                        branchUnbalanced
                            leftTrunk
                            { left = leftChildren.left
                            , right = leftRight |> children |> .left
                            }
                    , right =
                        branchUnbalanced
                            pivotTrunk
                            { left = leftRight |> children |> .right
                            , right = pivotRight
                            }
                    }


branchUnbalanced :
    element
    -> Children element
    -> Emptiable (Branch element) never_
branchUnbalanced trunkElement children_ =
    Branch
        { element = trunkElement
        , children = children_
        , height =
            1
                + max
                    (children_ |> .left |> height)
                    (children_ |> .right |> height)
        }
        |> filled


leaf : element -> Emptiable (Branch element) never_
leaf singleElement =
    branchUnbalanced
        singleElement
        { left = empty
        , right = empty
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
            |> fillMap filled
            |> fillMap
                (\treeFilled ->
                    { element = treeFilled |> trunk |> elementChange
                    , children = treeFilled |> children
                    , height = treeFilled |> height
                    }
                        |> Branch
                )


endRemove :
    Linear.Direction
    ->
        (Emptiable (Branch element) Never
         -> Emptiable (Branch element) Possibly
        )
endRemove direction =
    case direction of
        Up ->
            endRemoveUp

        Down ->
            endRemoveDown


endRemoveDown :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) Possibly
endRemoveDown tree =
    case tree |> children |> .left of
        Empty _ ->
            tree |> children |> .right

        Filled leftBranch ->
            branch
                (tree |> trunk)
                { left = leftBranch |> filled |> endRemoveDown
                , right = tree |> children |> .right
                }


endRemoveUp :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) Possibly
endRemoveUp tree =
    case tree |> children |> .right of
        Empty _ ->
            tree |> children |> .left

        Filled rightBranch ->
            branch
                (tree |> trunk)
                { left = tree |> children |> .left
                , right = rightBranch |> filled |> endRemoveUp
                }



-- transform


foldOnto :
    (element -> accumulated)
    -> Linear.Direction
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (Branch element) Never
         -> accumulated
        )
foldOnto firstToInitial direction reduce =
    \tree ->
        tree
            |> endRemove (direction |> Linear.opposite)
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
                treePossiblyEmpty |> foldDown reduce initial


foldUpFrom :
    accumulated
    -> (element -> (accumulated -> accumulated))
    ->
        (Emptiable (Branch element) Possibly
         -> accumulated
        )
foldUpFrom initial reduce =
    \tree ->
        case tree |> fillMap filled of
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


foldDown :
    (element -> (accumulated -> accumulated))
    -> accumulated
    ->
        (Emptiable (Branch element) Possibly
         -> accumulated
        )
foldDown reduce initial =
    \tree ->
        case tree |> fillMap filled of
            Empty _ ->
                initial

            Filled treeFilled ->
                treeFilled
                    |> children
                    |> .left
                    |> foldDown reduce
                        (treeFilled
                            |> children
                            |> .right
                            |> foldDown reduce initial
                            |> reduce (treeFilled |> trunk)
                        )
