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
                let
                    children_ =
                        treeFilled |> Tree2.children
                in
                case ( elementToInsert, treeFilled |> Tree2.trunk ) |> order of
                    EQ ->
                        treeFilled
                            |> Emptiable.emptyAdapt never
                            |> Tree2.trunkAlter (\_ -> elementToInsert)

                    LT ->
                        Tree2.branch
                            (treeFilled |> Tree2.trunk)
                            { children_
                                | left =
                                    children_
                                        |> .left
                                        |> treeInsert order elementToInsert
                                        |> Emptiable.emptyAdapt (\_ -> Possible)
                            }

                    GT ->
                        Tree2.branch
                            (treeFilled |> Tree2.trunk)
                            { children_
                                | right =
                                    children_
                                        |> .right
                                        |> treeInsert order elementToInsert
                                        |> Emptiable.emptyAdapt (\_ -> Possible)
                            }


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
                Tree2.branch
                    (treeFilled |> Tree2.trunk)
                    { children
                        | left =
                            children
                                |> .left
                                |> Emptiable.mapFlat
                                    (\leftBranch ->
                                        leftBranch
                                            |> filled
                                            |> treeRemove locationToRemove
                                    )
                    }

            GT ->
                Tree2.branch
                    (treeFilled |> Tree2.trunk)
                    { children
                        | right =
                            children
                                |> .right
                                |> Emptiable.mapFlat
                                    (\rightBranch ->
                                        rightBranch
                                            |> filled
                                            |> treeRemove locationToRemove
                                    )
                    }

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
                                    |> treeRemove
                                        (\el -> ( collision, el ) |> order)
                            )
                )



{- zombies

      treeElementUnsortedAlterHelp :
          (element -> Bool)
          -> (element -> element)
          ->
              (Emptiable (Branch element) Never
               ->
                  -- empty means no changes
                  Emptiable
                      (Emptiable (Branch element) never_)
                      Possibly
              )
      treeElementUnsortedAlterHelp shouldBeAltered elementChange =
          \treeToAlter ->
              let
                  children =
                      treeToAlter |> Tree2.children
              in
              if treeToAlter |> Tree2.trunk |> shouldBeAltered then
                  Tree2.branch
                      (treeToAlter |> Tree2.trunk |> elementChange)
                      children
                      |> filled

              else
                  case children.left |> Emptiable.map filled of
                      Emptiable.Empty _ ->
                          children.right
                              |> Emptiable.mapFlat
                                  (\rightBranch ->
                                      rightBranch
                                          |> filled
                                          |> treeElementUnsortedAlterHelp shouldBeAltered elementChange
                                          |> Emptiable.map
                                              (\rightAlteredResult ->
                                                  Tree2.branch
                                                      (treeToAlter |> Tree2.trunk)
                                                      { left = Emptiable.empty
                                                      , right =
                                                          rightAlteredResult
                                                              |> Emptiable.emptyAdapt (\_ -> Possible)
                                                      }
                                              )
                                  )

                      Emptiable.Filled leftFilled ->
                          let
                              leftAltered =
                                  leftFilled
                                      |> treeElementUnsortedAlterHelp shouldBeAltered elementChange
                          in
                          case leftAltered of
                              Emptiable.Filled leftAlteredResult ->
                                  Tree2.branch
                                      (treeToAlter |> Tree2.trunk)
                                      { left = leftAlteredResult |> Emptiable.emptyAdapt (\_ -> Possible)
                                      , right = children.right
                                      }
                                      |> filled

                              Emptiable.Empty _ ->
                                  children.right
                                      |> Emptiable.mapFlat
                                          (\rightBranch ->
                                              rightBranch
                                                  |> filled
                                                  |> treeElementUnsortedAlterHelp shouldBeAltered elementChange
                                                  |> Emptiable.map
                                                      (\rightAlteredResult ->
                                                          Tree2.branch
                                                              (treeToAlter |> Tree2.trunk)
                                                              { left = children.left
                                                              , right = rightAlteredResult |> Emptiable.emptyAdapt (\_ -> Possible)
                                                              }
                                                      )
                                          )


   treeRemoveUnsorted :
       (element -> Bool)
       ->
           (Tree2.Branch element
            -> Emptiable (Tree2.Branch element) Possibly
           )
   treeRemoveUnsorted removeLocation =
       \treeFilled ->
           treeFilled
               |> treeRemoveUnsortedHelp removeLocation
               |> Emptiable.fillElseOnEmpty
                   (\_ -> treeFilled |> filled)


   treeRemoveUnsortedHelp :
       (element -> Bool)
       ->
           (Tree2.Branch element
            ->
               Emptiable
                   -- Empty means not found
                   (Emptiable (Tree2.Branch element) Possibly)
                   Possibly
           )
   treeRemoveUnsortedHelp isFound =
       \treeFill ->
           let
               treeFilled =
                   treeFill |> filled

               children =
                   treeFilled |> Tree2.children
           in
           if treeFilled |> Tree2.trunk |> isFound then
               (if
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
                                   | right = rightTreeFilled |> Tree2.endRemove Down
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
                                   | left = leftTreeFilled |> Tree2.endRemove Up
                               }
               )
                   |> filled

           else
               let
                   leftRemoved =
                       children.left
                           |> Emptiable.mapFlat
                               (\leftFilled ->
                                   leftFilled
                                       |> treeRemoveUnsortedHelp isFound
                               )
               in
               case leftRemoved of
                   Filled leftRemovedResult ->
                       Tree2.branch
                           (treeFilled |> Tree2.trunk)
                           { children
                               | left = leftRemovedResult
                           }
                           |> filled

                   Empty _ ->
                       case children.right of
                           Emptiable.Empty _ ->
                               Emptiable.empty

                           Emptiable.Filled rightFilled ->
                               rightFilled
                                   |> treeRemoveUnsortedHelp isFound
                                   |> Emptiable.map
                                       (\rightRemovedResult ->
                                           Tree2.branch
                                               (treeFilled |> Tree2.trunk)
                                               { children
                                                   | right = rightRemovedResult
                                               }
                                       )
-}
