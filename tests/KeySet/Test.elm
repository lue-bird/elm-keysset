module KeySet.Test exposing (suite)

import Character
import Emptiable exposing (Emptiable(..), fillMap, filled)
import Expect
import Fuzz
import KeySet exposing (KeySet)
import KeySet.Internal
import Linear exposing (Direction(..))
import List.Extra
import Order exposing (Ordering)
import Possibly exposing (Possibly)
import Stack
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Tree2 exposing (Branch)
import Typed


suite : Test
suite =
    describe "KeySet"
        [ elementAlterSuite
        , elementSuite
        , endSuite
        , exceptSuite
        , foldFromSuite
        , insertSuite
        , intersectSuite
        , mapSuite
        , mapTrySuite
        , fold2FromSuite
        , removeElementSuite
        , sizeSuite
        , toListSuite
        , unifyWithSuite
        ]


toListSuite : Test
toListSuite =
    describe "toList"
        [ test "empty"
            (\_ ->
                Emptiable.empty
                    |> KeySet.toList Up
                    |> Expect.equalLists []
            )
        , test "singleton"
            (\_ ->
                KeySet.only { id = 0, char = 'A' }
                    |> KeySet.toList Up
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "inserts"
            (\_ ->
                Emptiable.empty
                    |> KeySet.insert Character.byId { id = 2, char = 'A' }
                    |> KeySet.insert Character.byId { id = 0, char = 'B' }
                    |> KeySet.insert Character.byId { id = 5, char = 'C' }
                    |> KeySet.insert Character.byId { id = 3, char = 'E' }
                    |> KeySet.insert Character.byId { id = 1, char = 'F' }
                    |> KeySet.insert Character.byId { id = 4, char = 'G' }
                    |> KeySet.insert Character.byId { id = 3, char = 'B' }
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 0, char = 'B' }
                        , { id = 1, char = 'F' }
                        , { id = 2, char = 'A' }
                        , { id = 3, char = 'B' }
                        , { id = 4, char = 'G' }
                        , { id = 5, char = 'C' }
                        ]
            )
        , fuzz (Fuzz.list Character.fuzz)
            "fromList"
            (\list ->
                KeySet.fromList Character.byId list
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        (list
                            |> List.reverse
                            |> List.Extra.uniqueBy .id
                            |> List.sortBy .id
                        )
            )
        ]


validate :
    KeySet.Sorting element key_ tag
    ->
        (Emptiable (KeySet element tag) Possibly
         -> Result String (Emptiable (KeySet element tag) Possibly)
        )
validate sorting =
    \keySet ->
        Result.andThen
            (\( _, s ) ->
                if s == (keySet |> KeySet.size) then
                    Ok keySet

                else
                    Err ("tracking size [" ++ String.fromInt (keySet |> KeySet.size) ++ "] does not match with real one [" ++ String.fromInt s ++ "]")
            )
            (keySet |> KeySet.Internal.tree |> validateHelp sorting)


validateHelp :
    KeySet.Sorting element key_ tag_
    ->
        (Emptiable (Branch element) Possibly
         -> Result String ( Int, Int )
        )
validateHelp sorting tree =
    case tree |> fillMap filled of
        Empty _ ->
            Ok ( 0, 0 )

        Filled treeFilled ->
            let
                checkFurther =
                    Result.andThen
                        (\( ( lh, ls ), ( rh, rs ) ) ->
                            if abs (lh - rh) > 1 then
                                Err
                                    ([ "height diff ["
                                     , treeFilled |> Tree2.trunk |> Debug.toString
                                     , "]: "
                                     , lh |> String.fromInt
                                     , " vs "
                                     , rh |> String.fromInt
                                     ]
                                        |> String.concat
                                    )

                            else
                                Ok ( 1 + max lh rh, 1 + ls + rs )
                        )
                        (Result.map2 Tuple.pair
                            (treeFilled |> Tree2.children |> .left |> validateHelp sorting)
                            (treeFilled |> Tree2.children |> .right |> validateHelp sorting)
                        )
            in
            case
                fillMap (elementOrder sorting (treeFilled |> Tree2.trunk))
                    (treeFilled |> Tree2.children |> .left |> fillMap (filled >> Tree2.trunk))
            of
                Empty _ ->
                    checkFurther

                Filled LT ->
                    [ "element ["
                    , treeFilled |> Tree2.trunk |> Debug.toString
                    , "] is less than left"
                    ]
                        |> String.concat
                        |> Err

                Filled GT ->
                    [ "element ["
                    , treeFilled |> Tree2.trunk |> Debug.toString
                    , "] is more than right"
                    ]
                        |> String.concat
                        |> Err

                Filled EQ ->
                    checkFurther


elementOrder : KeySet.Sorting element key_ tag_ -> Ordering element
elementOrder =
    \sorting ->
        Order.by
            (sorting |> Typed.untag |> .key)
            (sorting |> Typed.untag |> .keyOrder)


insertSuite : Test
insertSuite =
    describe "insert"
        [ fuzz Character.fuzz
            "Emptiable.empty"
            (\element ->
                Emptiable.empty
                    |> KeySet.insert Character.byId element
                    |> validate Character.byId
                    |> Expect.ok
            )
        , test "to left"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 5, char = 'b' }
                    |> validate Character.byId
                    |> Expect.ok
            )
        , test "to left left"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 5, char = 'b' }
                    |> KeySet.insert Character.byId { id = 2, char = 'c' }
                    |> validate Character.byId
                    |> Expect.ok
            )
        , test "to left right"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 5, char = 'b' }
                    |> KeySet.insert Character.byId { id = 2, char = 'c' }
                    |> validate Character.byId
                    |> Expect.ok
            )
        , test "to right"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 15, char = 'b' }
                    |> validate Character.byId
                    |> Expect.ok
            )
        , test "to right left"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 15, char = 'b' }
                    |> KeySet.insert Character.byId { id = 12, char = 'c' }
                    |> validate Character.byId
                    |> Expect.ok
            )
        , test "to right right"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 15, char = 'b' }
                    |> KeySet.insert Character.byId { id = 20, char = 'c' }
                    |> validate Character.byId
                    |> Expect.ok
            )
        , test "M-N-O-L-K-Q-P-H-I-A"
            (\_ ->
                "MNOLKQPHIA"
                    |> String.toList
                    |> List.foldl
                        (\char ->
                            Result.andThen
                                (KeySet.insert Character.byId
                                    { char = char, id = Char.toCode char }
                                    >> validate Character.byId
                                )
                        )
                        (Ok Emptiable.empty)
                    |> Expect.ok
            )
        , fuzz2 (Fuzz.intRange -400 -100)
            (Fuzz.intRange 100 400)
            "ascending keys"
            (\lo hi ->
                List.range lo hi
                    |> List.foldr
                        (\i ->
                            Result.andThen
                                (KeySet.insert Character.byId
                                    { id = i, char = Char.fromCode i }
                                    >> validate Character.byId
                                )
                        )
                        (Ok Emptiable.empty)
                    |> Expect.ok
            )
        , fuzz2 (Fuzz.intRange -400 -100)
            (Fuzz.intRange 100 400)
            "descending keys"
            (\lo hi ->
                List.range lo hi
                    |> List.foldl
                        (\i ->
                            Result.andThen
                                (KeySet.insert Character.byId
                                    { id = i, char = Char.fromCode i }
                                    >> validate Character.byId
                                )
                        )
                        (Ok Emptiable.empty)
                    |> Expect.ok
            )
        , fuzz (Fuzz.list (Fuzz.intRange -200 200))
            "random keys"
            (\list ->
                list
                    |> List.foldl
                        (\i ->
                            Result.andThen
                                (KeySet.insert Character.byId
                                    { id = i, char = Char.fromCode i }
                                    >> validate Character.byId
                                )
                        )
                        (Ok Emptiable.empty)
                    |> Expect.ok
            )
        ]


removeElementSuite : Test
removeElementSuite =
    describe "elementRemove"
        [ fuzz Fuzz.int
            "Emptiable.empty"
            (\key ->
                Emptiable.empty
                    |> KeySet.elementRemove Character.byId key
                    |> validate Character.byId
                    |> Expect.ok
            )
        , fuzz2 Fuzz.int
            Fuzz.int
            "KeySet.only"
            (\put delete ->
                KeySet.only { id = put, char = '0' }
                    |> KeySet.elementRemove Character.byId delete
                    |> validate Character.byId
                    |> Expect.ok
            )
        , fuzz2 (Fuzz.list Character.fuzz)
            (Fuzz.list Fuzz.int)
            "KeySet.fromList"
            (\puts deletes ->
                List.foldl
                    (\key ->
                        Result.andThen
                            (KeySet.elementRemove Character.byId key
                                >> validate Character.byId
                            )
                    )
                    (Ok (KeySet.fromList Character.byId puts))
                    deletes
                    |> Expect.ok
            )
        , fuzz (Fuzz.list Character.fuzz)
            "clear"
            (\list ->
                List.foldl
                    (\{ id } ->
                        Result.andThen
                            (KeySet.elementRemove Character.byId id
                                >> validate Character.byId
                            )
                    )
                    (Ok (KeySet.fromList Character.byId list))
                    list
                    |> Result.map (Expect.equal Emptiable.empty)
                    |> Result.withDefault (Expect.fail "wasn't empty")
            )
        ]


elementAlterSuite : Test
elementAlterSuite =
    describe "elementAlter"
        [ test "empty to empty"
            (\_ ->
                KeySet.fromList Character.byId
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeySet.elementAlter Character.byId 2 (\_ -> Emptiable.empty)
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
            )
        , test "empty to filled"
            (\_ ->
                KeySet.fromList Character.byId
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeySet.elementAlter Character.byId 2 (\_ -> filled { id = 2, char = 'C' })
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }
                        , { id = 1, char = 'B' }
                        , { id = 2, char = 'C' }
                        ]
            )
        , test "filled to empty"
            (\_ ->
                KeySet.fromList Character.byId
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeySet.elementAlter Character.byId 1 (\_ -> Emptiable.empty)
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' } ]
            )
        , test "filled to filled"
            (\_ ->
                KeySet.fromList Character.byId
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeySet.elementAlter Character.byId 1 (fillMap (\c -> { c | char = 'C' }))
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }, { id = 1, char = 'C' } ]
            )
        ]



-- transform


sizeSuite : Test
sizeSuite =
    describe "size"
        [ test "Emptiable.empty"
            (\_ ->
                Emptiable.empty
                    |> KeySet.size
                    |> Expect.equal 0
            )
        , fuzz Character.fuzz
            "KeySet.only"
            (\character ->
                KeySet.only character
                    |> KeySet.size
                    |> Expect.equal 1
            )
        , fuzz (Fuzz.list Character.fuzz)
            "KeySet.fromList"
            (\list ->
                let
                    uniq =
                        List.Extra.uniqueBy .id list
                in
                KeySet.fromList Character.byId list
                    |> KeySet.size
                    |> Expect.equal (List.length uniq)
            )
        ]



-- M A N I P U L A T I O N


elementSuite : Test
elementSuite =
    describe "element"
        [ fuzz Fuzz.int
            "Emptiable.empty"
            (\key ->
                Emptiable.empty
                    |> KeySet.element Character.byId key
                    |> Expect.equal Emptiable.empty
            )
        , fuzz2 Fuzz.int
            Fuzz.int
            "KeySet.only"
            (\x y ->
                KeySet.only { id = x, char = 'A' }
                    |> KeySet.element Character.byId y
                    |> Expect.equal
                        (if x == y then
                            filled { id = x, char = 'A' }

                         else
                            Emptiable.empty
                        )
            )
        , fuzz2 Fuzz.int
            (Fuzz.list Character.fuzz)
            "KeySet.fromList"
            (\key list ->
                KeySet.fromList Character.byId list
                    |> KeySet.element Character.byId key
                    |> Expect.equal
                        (list
                            |> List.reverse
                            |> List.Extra.find (\character -> character.id == key)
                            |> Emptiable.fromMaybe
                        )
            )
        ]


endSuite : Test
endSuite =
    describe "end"
        [ describe "Up"
            [ fuzz Character.fuzz
                "only"
                (\character ->
                    KeySet.only character
                        |> KeySet.end Up
                        |> Expect.equal character
                )
            , fuzz
                (Fuzz.map Stack.fromTopBelow
                    (Fuzz.pair Character.fuzz (Fuzz.list Character.fuzz))
                )
                "fromStack"
                (\stack ->
                    KeySet.fromStack Character.byId stack
                        |> KeySet.end Up
                        |> Expect.equal
                            (stack
                                |> Stack.fold Up
                                    (\element soFar ->
                                        if element.id <= soFar.id then
                                            soFar

                                        else
                                            element
                                    )
                            )
                )
            ]
        , describe "Down"
            [ fuzz Character.fuzz
                "only"
                (\character ->
                    KeySet.only character
                        |> KeySet.end Down
                        |> Expect.equal character
                )
            , fuzz
                (Fuzz.map Stack.fromTopBelow
                    (Fuzz.pair Character.fuzz (Fuzz.list Character.fuzz))
                )
                "fromStack"
                (\stack ->
                    KeySet.fromStack Character.byId stack
                        |> KeySet.end Down
                        |> Expect.equal
                            (stack
                                |> Stack.fold Down
                                    (\element soFar ->
                                        if element.id <= soFar.id then
                                            soFar

                                        else
                                            element
                                    )
                            )
                )
            ]
        ]


mapSuite : Test
mapSuite =
    test "KeySet.map"
        (\_ ->
            [ { id = 3, char = 'A' }
            , { id = 1, char = 'B' }
            , { id = 4, char = 'C' }
            , { id = 5, char = 'D' }
            , { id = 3, char = 'E' }
            ]
                |> KeySet.fromList Character.byId
                |> KeySet.map (\element -> { element | id = element.id * 10 }) Character.byId
                |> KeySet.toList Up
                |> Expect.equalLists
                    [ { id = 10, char = 'B' }
                    , { id = 30, char = 'A' }
                    , { id = 40, char = 'C' }
                    , { id = 50, char = 'D' }
                    ]
        )



-- scan


foldFromSuite : Test
foldFromSuite =
    describe "foldFrom"
        [ fuzz (Fuzz.list Character.fuzz)
            "Up"
            (\list ->
                KeySet.fromList Character.byId list
                    |> KeySet.foldFrom [] Up (\element acc -> element :: acc)
                    |> Expect.equalLists
                        (list
                            |> List.Extra.uniqueBy .id
                            |> List.sortBy .id
                            |> List.reverse
                        )
            )
        , fuzz (Fuzz.list Character.fuzz)
            "Down"
            (\list ->
                KeySet.fromList Character.byId list
                    |> KeySet.foldFrom [] Down (\element acc -> element :: acc)
                    |> Expect.equalLists
                        (list
                            |> List.Extra.uniqueBy .id
                            |> List.sortBy .id
                        )
            )
        ]


mapTrySuite : Test
mapTrySuite =
    describe "mapTry"
        [ test "filter"
            (\_ ->
                [ { id = 3, char = 'A' }
                , { id = 1, char = 'B' }
                , { id = 4, char = 'C' }
                , { id = 5, char = 'D' }
                , { id = 2, char = 'E' }
                ]
                    |> KeySet.fromList Character.byId
                    |> KeySet.mapTry
                        (\element ->
                            if element.id > 3 || element.char == 'B' then
                                filled element

                            else
                                Emptiable.empty
                        )
                        Character.byId
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 1, char = 'B' }
                        , { id = 4, char = 'C' }
                        , { id = 5, char = 'D' }
                        ]
            )
        ]


unifyWithSuite : Test
unifyWithSuite =
    describe "union"
        [ test "left is empty"
            (\_ ->
                Emptiable.empty
                    |> KeySet.unifyWith Character.byId
                        (KeySet.only { id = 0, char = 'A' })
                    |> KeySet.toList Up
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "right is empty"
            (\_ ->
                KeySet.only { id = 0, char = 'A' }
                    |> KeySet.unifyWith Character.byId Emptiable.empty
                    |> KeySet.toList Up
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "unions"
            (\_ ->
                KeySet.fromList Character.byId
                    [ { id = 0, char = 'A' }
                    , { id = 1, char = 'B' }
                    , { id = 2, char = 'c' }
                    , { id = 3, char = 'd' }
                    ]
                    |> KeySet.unifyWith Character.byId
                        (KeySet.fromList Character.byId
                            [ { id = 2, char = 'c' }
                            , { id = 3, char = 'd' }
                            , { id = 4, char = 'e' }
                            , { id = 5, char = 'f' }
                            ]
                        )
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }
                        , { id = 1, char = 'B' }
                        , { id = 2, char = 'c' }
                        , { id = 3, char = 'd' }
                        , { id = 4, char = 'e' }
                        , { id = 5, char = 'f' }
                        ]
            )
        ]



-- T R A N S F O R M


intersectSuite : Test
intersectSuite =
    describe "intersect"
        [ test "left is empty"
            (\_ ->
                KeySet.only { id = 0, char = 'A' }
                    |> KeySet.intersect Character.byId Emptiable.empty
                    |> KeySet.toList Up
                    |> Expect.equalLists []
            )
        , test "right is empty"
            (\_ ->
                Emptiable.empty
                    |> KeySet.intersect Character.byId (KeySet.only { id = 0, char = 'A' })
                    |> KeySet.toList Up
                    |> Expect.equalLists []
            )
        , test "intersects"
            (\_ ->
                KeySet.fromList Character.byId
                    [ { id = 2, char = 'c' }
                    , { id = 3, char = 'd' }
                    , { id = 4, char = 'e' }
                    , { id = 5, char = 'f' }
                    ]
                    |> KeySet.intersect Character.byId
                        (KeySet.fromList Character.byId
                            [ { id = 0, char = 'A' }
                            , { id = 1, char = 'B' }
                            , { id = 2, char = 'c' }
                            , { id = 3, char = 'd' }
                            ]
                        )
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 2, char = 'c' }
                        , { id = 3, char = 'd' }
                        ]
            )
        ]


exceptSuite : Test
exceptSuite =
    describe "diff"
        [ test "left is empty"
            (\_ ->
                Emptiable.empty
                    |> KeySet.except Character.byId
                        (KeySet.only { id = 0, char = 'A' })
                    |> KeySet.toList Up
                    |> Expect.equalLists []
            )
        , test "right is empty"
            (\_ ->
                KeySet.only { id = 0, char = 'A' }
                    |> KeySet.except Character.byId Emptiable.empty
                    |> KeySet.toList Up
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "diffs"
            (\_ ->
                KeySet.fromList Character.byId
                    [ { id = 0, char = 'A' }
                    , { id = 1, char = 'B' }
                    , { id = 2, char = 'c' }
                    , { id = 3, char = 'd' }
                    ]
                    |> KeySet.except Character.byId
                        (KeySet.fromList Character.byId
                            [ { id = 2, char = 'c' }
                            , { id = 3, char = 'd' }
                            , { id = 4, char = 'e' }
                            , { id = 5, char = 'f' }
                            ]
                        )
                    |> KeySet.toList Up
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }
                        , { id = 1, char = 'B' }
                        ]
            )
        ]


fold2FromSuite : Test
fold2FromSuite =
    let
        testFold2 =
            KeySet.fold2From
                []
                (\toBeMerged soFar ->
                    soFar
                        |> (::)
                            (case toBeMerged of
                                KeySet.Second incoming ->
                                    String.fromList (List.repeat incoming.id incoming.char)

                                KeySet.First base ->
                                    String.fromList (List.repeat base.id base.char)

                                KeySet.FirstSecond ( first, second ) ->
                                    String.fromList (List.repeat first.id first.char)
                                        ++ (second.char |> String.fromChar)
                            )
                )
    in
    describe "fold2From"
        [ test "left is empty"
            (\_ ->
                { first =
                    { sorting = Character.byId
                    , set = KeySet.only { id = 0, char = 'A' }
                    }
                , second =
                    { sorting = Character.byId
                    , set = Emptiable.empty
                    }
                }
                    |> testFold2
                    |> Expect.equalLists
                        [ "0A"
                        ]
            )
        , test "right is empty"
            (\_ ->
                { first =
                    { sorting = Character.byId
                    , set = Emptiable.empty
                    }
                , second =
                    { sorting = Character.byId
                    , set = KeySet.only { id = 3, char = 'A' }
                    }
                }
                    |> testFold2
                    |> Expect.equalLists
                        [ "AAA"
                        ]
            )
        , test "merges"
            (\_ ->
                testFold2
                    { first =
                        { sorting = Character.byId
                        , set =
                            KeySet.fromList Character.byId
                                [ { id = 2, char = 'C' }
                                , { id = 3, char = 'd' }
                                , { id = 4, char = 'e' }
                                , { id = 5, char = 'f' }
                                ]
                        }
                    , second =
                        { sorting = Character.byId
                        , set =
                            KeySet.fromList Character.byId
                                [ { id = 0, char = 'A' }
                                , { id = 1, char = 'B' }
                                , { id = 2, char = 'c' }
                                , { id = 3, char = 'd' }
                                ]
                        }
                    }
                    |> Expect.equalLists
                        [ "5f"
                        , "4e"
                        , "DDDd"
                        , "CCc"
                        , "B"
                        , ""
                        ]
            )
        ]
