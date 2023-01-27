module KeysSet.Internal exposing (elementCollisions, treeElement, treeExcept, treeInsert, treeRemove)

import ArraySized exposing (ArraySized)
import Emptiable exposing (Emptiable(..), filled)
import Keys exposing (Keys)
import Linear exposing (Direction(..))
import N exposing (Add1, Exactly)
import Possibly exposing (Possibly(..))
import Tree2
import Typed exposing (Checked, Public, Typed)


type alias KeysSet element tag lastKeyIndex =
    Typed
        Checked
        tag
        Public
        { size : Int
        , byKeys :
            ArraySized
                (Tree2.Branch element)
                (Exactly (Add1 lastKeyIndex))
        }


elementCollisions :
    Keys element tags_ keys_ lastKeyIndex
    -> element
    ->
        (KeysSet element tag_ lastKeyIndex
         -> Emptiable (Tree2.Branch element) Possibly
        )
elementCollisions keys toCollideWith =
    \keysSet ->
        keysSet
            |> Typed.untag
            |> .byKeys
            |> ArraySized.and
                (keys
                    |> Keys.toArray
                    |> Typed.untag
                    |> ArraySized.inToNumber
                )
            |> ArraySized.foldFrom Emptiable.empty
                Down
                (\( branch, key ) soFar ->
                    soFar
                        |> (case
                                branch
                                    |> filled
                                    |> treeElement (\el -> ( toCollideWith, el ) |> key)
                            of
                                Emptiable.Empty _ ->
                                    identity

                                Emptiable.Filled collision ->
                                    treeInsert key collision
                           )
                )


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
            |> Emptiable.map filled
            |> Emptiable.mapFlat
                (\treeFilled ->
                    case treeFilled |> Tree2.trunk |> location of
                        EQ ->
                            treeFilled |> Tree2.trunk |> filled

                        LT ->
                            treeFilled |> Tree2.children |> .left |> treeElement location

                        GT ->
                            treeFilled |> Tree2.children |> .right |> treeElement location
                )


treeInsert :
    (( element, element ) -> Order)
    -> element
    ->
        (Emptiable (Tree2.Branch element) Possibly
         -> Emptiable (Tree2.Branch element) never_
        )
treeInsert order elementToInsert =
    \tree_ ->
        case tree_ |> Emptiable.map filled of
            Empty _ ->
                elementToInsert |> Tree2.one

            Filled treeFilled ->
                case ( elementToInsert, treeFilled |> Tree2.trunk ) |> order of
                    EQ ->
                        -- to alter
                        -- treeFilled
                        --     |> Emptiable.emptyAdapt never
                        --     |> Tree2.trunkAlter (\_ -> elementToInsert)
                        treeFilled |> Emptiable.emptyAdapt never

                    LT ->
                        treeFilled
                            |> Tree2.childrenLeftAlter
                                (\left ->
                                    left |> treeInsert order elementToInsert
                                )

                    GT ->
                        treeFilled
                            |> Tree2.childrenRightAlter
                                (\right ->
                                    right |> treeInsert order elementToInsert
                                )


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
        let
            children =
                treeFilled |> Tree2.children
        in
        case treeFilled |> Tree2.trunk |> locationToRemove of
            LT ->
                treeFilled
                    |> Tree2.childrenLeftAlter
                        (\left ->
                            left
                                |> Emptiable.mapFlat
                                    (\leftBranch ->
                                        leftBranch
                                            |> filled
                                            |> treeRemove locationToRemove
                                    )
                        )

            GT ->
                treeFilled
                    |> Tree2.childrenRightAlter
                        (\right ->
                            right
                                |> Emptiable.mapFlat
                                    (\rightBranch ->
                                        rightBranch
                                            |> filled
                                            |> treeRemove locationToRemove
                                    )
                        )

            EQ ->
                if
                    (children.left |> Tree2.height)
                        < (children.right |> Tree2.height)
                then
                    case children.right |> Emptiable.map filled of
                        Empty _ ->
                            children.left

                        Filled rightTreeFilled ->
                            Tree2.branch
                                (rightTreeFilled |> Tree2.end Down)
                                { children
                                    | right = rightTreeFilled |> Tree2.removeEnd Down
                                }

                else
                    -- left >= right
                    case children.left |> Emptiable.map filled of
                        Empty _ ->
                            children.right

                        Filled leftTreeFilled ->
                            Tree2.branch
                                (leftTreeFilled |> Tree2.end Up)
                                { children
                                    | left = leftTreeFilled |> Tree2.removeEnd Up
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
