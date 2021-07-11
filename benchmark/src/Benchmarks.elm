module Benchmarks exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)
import KeysDict exposing (KeysDict)
import KeysDict.Uniqueness exposing (unique)
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


keysDict : KeysDict CasedLetter
keysDict =
    casedLetterList
        |> List.foldl KeysDict.insert
            (KeysDict.promising
                [ unique .lowercase, unique .uppercase ]
            )


fold : Benchmark
fold =
    Benchmark.compare "fold"
        "KeysDict.fold"
        (\() -> KeysDict.fold (::) [] keysDict)
        "foldr"
        (\() -> foldr (::) [] keysDict)


foldr : (a -> b -> b) -> b -> KeysDict a -> b
foldr reduce initial =
    KeysDict.toList
        >> List.foldr reduce initial


keysDictReversed : KeysDict CasedLetter
keysDictReversed =
    casedLetterList
        |> List.reverse
        |> List.foldl KeysDict.insert
            (KeysDict.promising
                [ unique .uppercase, unique .lowercase ]
            )


equal : Benchmark
equal =
    Benchmark.compare "equal <?> alternative equals"
        "equal"
        (\() ->
            KeysDict.equal keysDict keysDictReversed
        )
        "altEqual"
        (\() ->
            altEqual keysDict keysDictReversed
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
