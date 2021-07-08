module UniquenessTest exposing (suite)

import Expect
import KeysDict.Uniqueness as Uniqueness exposing (door)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "KeysDict.Uniqueness"
        [ describe "violated"
            [ test "violated if equal keys" <|
                \() ->
                    door .name
                        |> Uniqueness.violated
                            { name = "smile", symbol = '😊' }
                            { symbol = '🙂', name = "smile" }
                        |> Expect.equal True
            , test "not violated if different keys" <|
                \() ->
                    door .symbol
                        |> Uniqueness.violated
                            { name = "smile", symbol = '😊' }
                            { symbol = '🙂', name = "smile" }
                        |> Expect.equal False
            ]
        ]
