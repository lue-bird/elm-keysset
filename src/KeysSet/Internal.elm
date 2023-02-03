module KeysSet.Internal exposing (elementCollisions, treeElement, treeExcept, treeInsertIfNoCollision, treeRemove)

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
                                    treeInsertIfNoCollision key collision
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
        case tree_ |> Emptiable.map filled of
            Empty _ ->
                toInsert |> Tree2.one

            Filled treeFilled ->
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
                    case children.up |> Emptiable.map filled of
                        Empty _ ->
                            children.down

                        Filled upTreeFilled ->
                            Tree2.branch
                                (upTreeFilled |> Tree2.end Down)
                                { children
                                    | up = upTreeFilled |> Tree2.removeEnd Down
                                }

                else
                    -- down >= up
                    case children.down |> Emptiable.map filled of
                        Empty _ ->
                            children.up

                        Filled downTreeFilled ->
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
