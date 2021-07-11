module UtilTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util exposing (aspect, equalIgnoringOrder)


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
        , test "equalIgnoringOrder"
            (\() ->
                equalIgnoringOrder [ 1, 2, 3 ] [ 3, 2, 1 ]
                    |> Expect.equal True
            )
        ]
