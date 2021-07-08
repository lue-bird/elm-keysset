module UtilTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util exposing (aspect)


suite : Test
suite =
    describe "Util"
        [ describe "aspect examples works"
            [ test "aspect equal example works" <|
                \() ->
                    aspect .name
                        (==)
                        { name = "smile", symbol = '😊' }
                        { symbol = '🙂', name = "smile" }
                        |> Expect.equal True
            , test "abs greater than example works" <|
                \() ->
                    aspect abs (>) 10 -20
                        |> Expect.equal False
            ]
        ]
