module UtilTest exposing (suite)

import Expect
import List.Extra
import Test exposing (Test, describe, test)
import Util exposing (aspect)


suite : Test
suite =
    describe "Util"
        [ describe "aspect"
            [ test "equal example"
                (\() ->
                    aspect .name
                        (==)
                        { name = "smile", symbol = '😊' }
                        { symbol = '🙂', name = "smile" }
                        |> Expect.equal True
                )
            , test "abs greater than example"
                (\() ->
                    aspect abs (>) 10 -20
                        |> Expect.equal False
                )
            ]
        ]
