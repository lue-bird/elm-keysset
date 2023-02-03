module Order.Test exposing (suite)

import Char.Order
import Expect
import Fuzz exposing (Fuzzer)
import Order
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Order"
        [ let
            unicodeNonLetter : Fuzzer Char
            unicodeNonLetter =
                Fuzz.char
                    |> Fuzz.filter
                        (\c ->
                            (c |> Char.isLower |> not) && (c |> Char.isUpper |> not)
                        )
          in
          Test.fuzz
            (Fuzz.pair unicodeNonLetter unicodeNonLetter)
            "non-letter: Char.Order.alphabetically = compare"
            (\( char0, char1 ) ->
                Order.with
                    (Char.Order.alphabetically Char.Order.upperLower)
                    char0
                    char1
                    |> Expect.equal
                        (compare char0 char1)
            )
        ]
