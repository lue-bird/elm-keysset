module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import MultiSet exposing (MultiSet)
import MultiSet.Uniqueness exposing (unique)
import Util exposing (ListOperationResult(..), aspect, removeFirstWithResult)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "MultiSet"
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


multiSet : MultiSet CasedLetter
multiSet =
    casedLetterList
        |> List.foldl MultiSet.insert
            (MultiSet.promising
                [ unique .lowercase, unique .uppercase ]
            )


fold : Benchmark
fold =
    Benchmark.compare "fold"
        "MultiSet.fold"
        (\() -> MultiSet.fold (::) [] multiSet)
        "foldr"
        (\() -> foldr (::) [] multiSet)


foldr : (a -> b -> b) -> b -> MultiSet a -> b
foldr reduce initial =
    MultiSet.toList
        >> List.foldr reduce initial


multiSetReversed : MultiSet CasedLetter
multiSetReversed =
    casedLetterList
        |> List.reverse
        |> List.foldl MultiSet.insert
            (MultiSet.promising
                [ unique .uppercase, unique .lowercase ]
            )


equal : Benchmark
equal =
    Benchmark.compare "equal <?> alternative equals"
        "equal"
        (\() ->
            MultiSet.equal multiSet multiSetReversed
        )
        "altEqual"
        (\() ->
            altEqual multiSet multiSetReversed
        )


altEqual :
    MultiSet CasedLetter
    -> MultiSet CasedLetter
    -> Bool
altEqual =
    aspect MultiSet.toList equalLists


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
