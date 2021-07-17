module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import KeysSet exposing (KeysSet)
import KeysSet.Uniqueness exposing (unique)
import Util exposing (ListOperationResult(..), aspect, removeFirstWithResult)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "KeysSet"
        [ equal
        , fold
        ]


type alias CasedLetter =
    { lowercase : Char
    , uppercase : Char
    }


{-| upperCaseLatin= range 65 90
lowerCaseLatin= range 97 122
-}
casedLetterList : List CasedLetter
casedLetterList =
    List.range 65 90
        |> List.map
            (\upperCaseLatin ->
                { uppercase =
                    Char.fromCode upperCaseLatin
                , lowercase =
                    Char.fromCode (upperCaseLatin + 32)
                }
            )


keysSet : KeysSet CasedLetter
keysSet =
    casedLetterList
        |> List.foldl KeysSet.insert
            (KeysSet.promising
                [ unique .lowercase, unique .uppercase ]
            )


fold : Benchmark
fold =
    Benchmark.compare "fold"
        "KeysSet.fold"
        (\() -> KeysSet.fold (::) [] keysSet)
        "foldr"
        (\() -> foldr (::) [] keysSet)


foldr : (a -> b -> b) -> b -> KeysSet a -> b
foldr reduce initial =
    KeysSet.toList
        >> List.foldr reduce initial


keysSetReversed : KeysSet CasedLetter
keysSetReversed =
    casedLetterList
        |> List.reverse
        |> List.foldl KeysSet.insert
            (KeysSet.promising
                [ unique .uppercase, unique .lowercase ]
            )


equal : Benchmark
equal =
    Benchmark.compare "equal <?> alternative equals"
        "equal"
        (\() ->
            KeysSet.equal keysSet keysSetReversed
        )
        "altEqual"
        (\() ->
            altEqual keysSet keysSetReversed
        )


altEqual :
    KeysSet CasedLetter
    -> KeysSet CasedLetter
    -> Bool
altEqual =
    aspect KeysSet.toList equalLists


equalLists : List el -> List el -> Bool
equalLists aList bList =
    case aList of
        head :: aNextValues ->
            case removeFirstWithResult head bList of
                ChangedList aWithout ->
                    equalLists aNextValues aWithout

                AsBeforeList _ ->
                    False

        [] ->
            List.isEmpty bList
