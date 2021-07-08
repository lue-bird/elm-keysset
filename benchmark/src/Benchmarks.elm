module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import KeysDict exposing (KeysDict)
import KeysDict.Uniqueness exposing (door)
import Util exposing (ListOperationResult(..), aspect, removeFirstWithResult)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "KeysDict"
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


multiDict : KeysDict CasedLetter
multiDict =
    casedLetterList
        |> List.foldl KeysDict.insert
            (KeysDict.enterableBy
                [ door .lowercase, door .uppercase ]
            )


fold : Benchmark
fold =
    Benchmark.compare "fold"
        "KeysDict.fold"
        (\() -> KeysDict.fold (::) [] multiDict)
        "foldr"
        (\() -> foldr (::) [] multiDict)


foldr : (a -> b -> b) -> b -> KeysDict a -> b
foldr reduce initial =
    KeysDict.toList
        >> List.foldr reduce initial


multiDictReversed : KeysDict CasedLetter
multiDictReversed =
    casedLetterList
        |> List.reverse
        |> List.foldl KeysDict.insert
            (KeysDict.enterableBy
                [ door .uppercase, door .lowercase ]
            )


equal : Benchmark
equal =
    Benchmark.compare "equal <?> alternative equals"
        "equal"
        (\() ->
            KeysDict.equal multiDict multiDictReversed
        )
        "altEqual"
        (\() ->
            altEqual multiDict multiDictReversed
        )


altEqual :
    KeysDict CasedLetter
    -> KeysDict CasedLetter
    -> Bool
altEqual =
    aspect KeysDict.toList equalLists


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
