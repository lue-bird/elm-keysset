module KeysSet.Internal.Test exposing (suite)

import Character exposing (Character)
import Emptiable exposing (Emptiable, fill)
import Expect
import KeysSet exposing (KeysSet, PreferenceOnCollisions(..))
import KeysSet.Internal
import Linear exposing (Direction(..))
import List.Linear
import N exposing (N1)
import Stack
import Test exposing (Test, fuzz, test)
import Tree2


suite : Test
suite =
    Test.describe "KeysSet.Internal"
        [ elementCollisionSuite
        ]


elementCollisionSuite : Test
elementCollisionSuite =
    let
        element0 : Character
        element0 =
            { id = 0, char = 'A' }

        element1 : Character
        element1 =
            { id = 1, char = 'B' }

        with2 : Emptiable (KeysSet Character Character.ByIdOrChar N1) never_
        with2 =
            KeysSet.fromStack
                Character.byIdOrChar
                (Stack.topBelow element0 [ element1 ])
    in
    Test.describe "elementCollision"
        [ test "finds hardcoded 0"
            (\() ->
                with2
                    |> fill
                    |> KeysSet.Internal.elementCollisions Character.byIdOrChar element0
                    |> Expect.equal
                        (Tree2.one element0)
            )
        , test "finds hardcoded 1"
            (\() ->
                with2
                    |> fill
                    |> KeysSet.Internal.elementCollisions Character.byIdOrChar element1
                    |> Expect.equal
                        (Tree2.one element1)
            )
        , fuzz Character.fuzz
            "finds inserted"
            (\element ->
                Emptiable.empty
                    |> KeysSet.insert ReplaceExisting Character.byIdOrChar element
                    |> fill
                    |> KeysSet.Internal.elementCollisions Character.byIdOrChar element
                    |> Expect.equal
                        (Tree2.one element)
            )
        , test "hardcoded unique list"
            (\() ->
                let
                    elements =
                        "MNOLKQPHIA"
                            |> String.toList
                            |> List.map
                                (\char ->
                                    { char = char, id = Char.toCode char }
                                )

                    keysSet =
                        elements
                            |> List.Linear.foldFrom
                                Emptiable.empty
                                Up
                                (\element ->
                                    KeysSet.insert ReplaceExisting Character.byIdOrChar element
                                )
                in
                elements
                    |> List.map
                        (\element () ->
                            keysSet
                                |> Emptiable.mapFlat
                                    (KeysSet.Internal.elementCollisions Character.byIdOrChar element)
                                |> Expect.equal
                                    (Tree2.one element)
                        )
                    |> (\all -> Expect.all all ())
            )
        ]
