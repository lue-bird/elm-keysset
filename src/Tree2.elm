module Tree2 exposing
    ( Branch, Children2
    , leaf, branch, branchUnbalanced
    , height, children, end
    , endRemove, elementAlter
    , foldFrom, foldOnto
    , trunk
    )

{-| Binary tree

@docs Branch, Children2


## create

`Emptiable.empty`,

@docs leaf, branch, branchUnbalanced


## scan

@docs height, children, end, element


## alter

@docs endRemove, elementAlter


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
        , children : Children2 element
        , height : Int
        }


{-| 2 sub-trees
-}
type alias Children2 element =
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


children : Emptiable (Branch element) Never -> Children2 element
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


balance :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) never_
balance =
    \tree ->
        let
            branch_ =
                tree |> fill |> filled
        in
        case ( branch_ |> children |> .left, branch_ |> children |> .right ) of
            ( Empty _, Empty _ ) ->
                leaf (branch_ |> trunk)

            ( Filled childrenLeftBranch, Empty _ ) ->
                let
                    childrenLeft =
                        childrenLeftBranch |> filled
                in
                if (childrenLeft |> height) > 1 then
                    rotateRight
                        (branch_ |> trunk)
                        (childrenLeft |> trunk)
                        (childrenLeft |> children)
                        empty

                else
                    branchUnbalanced
                        (branch_ |> trunk)
                        { left = childrenLeft
                        , right = empty
                        }

            ( Empty _, Filled childrenRightBranch ) ->
                let
                    childrenRight =
                        childrenRightBranch |> filled
                in
                if (childrenRight |> height) > 1 then
                    rotateLeft
                        (branch_ |> trunk)
                        empty
                        (childrenRight |> trunk)
                        (childrenRight |> children)

                else
                    branchUnbalanced
                        (branch_ |> trunk)
                        { left = empty, right = childrenRight }

            ( Filled childrenLeftBranch, Filled childrenRightBranch ) ->
                let
                    childrenLeft =
                        childrenLeftBranch |> filled

                    childrenRight =
                        childrenRightBranch |> filled
                in
                if ((childrenLeft |> height) - (childrenRight |> height)) < -1 then
                    rotateLeft
                        (branch_ |> trunk)
                        childrenLeft
                        (childrenRight |> trunk)
                        (childrenRight |> children)

                else if ((childrenLeft |> height) - (childrenRight |> height)) > 1 then
                    rotateRight
                        (branch_ |> trunk)
                        (childrenLeft |> trunk)
                        (childrenLeft |> children)
                        childrenRight

                else
                    branchUnbalanced
                        (branch_ |> trunk)
                        { left = childrenLeft
                        , right = childrenRight
                        }


rotateLeft :
    element
    -> Emptiable (Branch element) Possibly
    -> element
    ->
        (Children2 element
         -> Emptiable (Branch element) never_
        )
rotateLeft pElement pLeft rElement rightChildren =
    case rightChildren |> .left of
        Empty _ ->
            branchUnbalanced pElement
                { left =
                    branchUnbalanced
                        pElement
                        { left = pLeft, right = empty }
                , right = rightChildren |> .right
                }

        Filled (Branch childrenLeft) ->
            if childrenLeft.height >= (rightChildren |> .right |> height) then
                branchUnbalanced rElement
                    { left =
                        branchUnbalanced
                            pElement
                            { left = pLeft, right = rightChildren |> .left }
                    , right = rightChildren |> .right
                    }

            else
                branchUnbalanced
                    childrenLeft.element
                    { left =
                        branchUnbalanced
                            pElement
                            { left = pLeft, right = childrenLeft.children |> .left }
                    , right =
                        branchUnbalanced
                            rElement
                            { left = childrenLeft.children |> .right
                            , right = rightChildren |> .right
                            }
                    }


rotateRight :
    element
    -> element
    -> Children2 element
    ->
        (Emptiable (Branch element) Possibly
         -> Emptiable (Branch element) never_
        )
rotateRight pElement leftElement leftChildren pRight =
    case leftChildren |> .right of
        Empty _ ->
            branchUnbalanced leftElement
                { left = leftChildren |> .left
                , right =
                    branchUnbalanced
                        pElement
                        { left = empty
                        , right = pRight
                        }
                }

        Filled (Branch childrenRight) ->
            if (leftChildren |> .left |> height) >= childrenRight.height then
                branchUnbalanced leftElement
                    { left = leftChildren |> .left
                    , right =
                        branchUnbalanced pElement
                            { left = leftChildren |> .right, right = pRight }
                    }

            else
                branchUnbalanced childrenRight.element
                    { left =
                        branchUnbalanced leftElement
                            { left = leftChildren |> .left
                            , right = childrenRight.children |> .left
                            }
                    , right =
                        branchUnbalanced pElement
                            { left = childrenRight.children |> .right, right = pRight }
                    }


{-| Will balance it out. Don't need to? → [`branchUnbalanced`](#branchUnbalanced)
-}
branch :
    element
    -> Children2 element
    -> Emptiable (Branch element) never_
branch trunkElement children_ =
    branchUnbalanced trunkElement children_
        |> balance


branchUnbalanced :
    element
    -> Children2 element
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
        let
            (Branch branch_) =
                tree |> fill
        in
        case branch_.children |> .left of
            Empty _ ->
                branch_.element

            Filled leftBranch ->
                leftBranch |> filled |> endDown


endUp : Emptiable (Branch element) Never -> element
endUp =
    \tree ->
        let
            (Branch branch_) =
                tree |> fill
        in
        case branch_.children |> .right of
            Empty _ ->
                branch_.element

            Filled rightBranch ->
                rightBranch |> filled |> endUp



-- alter


elementAlter :
    (element -> element)
    ->
        (Emptiable (Branch element) possiblyOrNever
         -> Emptiable (Branch element) possiblyOrNever
        )
elementAlter elementChange =
    \tree ->
        tree
            |> fillMap
                (\branch_ ->
                    let
                        treeFilled =
                            branch_ |> filled
                    in
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
    let
        (Branch branch_) =
            tree |> fill
    in
    case branch_.children |> .left of
        Empty _ ->
            branch_.children |> .right

        Filled leftBranch ->
            branchUnbalanced
                branch_.element
                { left = leftBranch |> filled |> endRemoveDown
                , right = branch_.children |> .right
                }
                |> balance


endRemoveUp :
    Emptiable (Branch element) Never
    -> Emptiable (Branch element) Possibly
endRemoveUp tree =
    let
        (Branch branch_) =
            tree |> fill
    in
    case branch_.children |> .right of
        Empty _ ->
            branch_.children |> .left

        Filled rightBranch ->
            branchUnbalanced
                branch_.element
                { left = branch_.children |> .left
                , right = rightBranch |> filled |> endRemoveUp
                }
                |> balance



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
                            |> reduce
                                (treeFilled |> trunk)
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
                            |> reduce
                                (treeFilled |> trunk)
                        )
