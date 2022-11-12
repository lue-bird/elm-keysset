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
import Util exposing (recover)


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
        , elementRemoveTest
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
        , test "only"
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
        case
            keySet
                |> KeySet.Internal.tree
                |> validateHelp sorting
        of
            Err error ->
                [ error
                , " for\n\n"
                , keySet |> KeySet.toList Up |> Debug.toString
                ]
                    |> String.concat
                    |> Err

            Ok { size } ->
                if size == (keySet |> KeySet.size) then
                    Ok keySet

                else
                    [ "tracking size ["
                    , keySet |> KeySet.size |> String.fromInt
                    , "] does not match with real one ["
                    , size |> String.fromInt
                    , "]"
                    , " for\n\n"
                    , keySet |> KeySet.toList Up |> Debug.toString
                    ]
                        |> String.concat
                        |> Err


validateHelp :
    KeySet.Sorting element key_ tag_
    ->
        (Emptiable (Branch element) Possibly
         -> Result String { height : Int, size : Int }
        )
validateHelp sorting tree =
    case tree |> fillMap filled of
        Empty _ ->
            { height = 0, size = 0 } |> Ok

        Filled treeFilled ->
            let
                checkFurther =
                    Result.andThen
                        (\children ->
                            if ((children.left.height - children.right.height) |> abs) <= 1 then
                                { height =
                                    1 + max children.left.height children.right.height
                                , size =
                                    1 + children.left.size + children.right.size
                                }
                                    |> Ok

                            else
                                [ "height below ["
                                , treeFilled |> Tree2.trunk |> Debug.toString
                                , "]: "
                                , children.left.height |> String.fromInt
                                , " vs "
                                , children.right.height |> String.fromInt
                                , " - so \n\n"
                                , treeFilled |> Tree2.children |> .left |> Tree2.foldFrom [] Down (::) |> Debug.toString
                                , "\nvs\n"
                                , treeFilled |> Tree2.children |> .right |> Tree2.foldFrom [] Down (::) |> Debug.toString
                                , "\n"
                                ]
                                    |> String.concat
                                    |> Err
                        )
                        (Result.map2 (\left right -> { left = left, right = right })
                            (treeFilled |> Tree2.children |> .left |> validateHelp sorting)
                            (treeFilled |> Tree2.children |> .right |> validateHelp sorting)
                        )
            in
            if
                case treeFilled |> Tree2.children |> .left of
                    Empty _ ->
                        False

                    Filled left ->
                        elementOrder sorting
                            (treeFilled |> Tree2.trunk)
                            (left |> filled |> Tree2.trunk)
                            /= GT
            then
                [ "element ["
                , treeFilled |> Tree2.trunk |> Debug.toString
                , "] is less than left"
                ]
                    |> String.concat
                    |> Err

            else if
                case treeFilled |> Tree2.children |> .right of
                    Empty _ ->
                        False

                    Filled right ->
                        elementOrder sorting
                            (treeFilled |> Tree2.trunk)
                            (right |> filled |> Tree2.trunk)
                            /= LT
            then
                [ "element ["
                , treeFilled |> Tree2.trunk |> Debug.toString
                , "] is more than right"
                ]
                    |> String.concat
                    |> Err

            else
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
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "to left"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 5, char = 'b' }
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "to left left"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 5, char = 'b' }
                    |> KeySet.insert Character.byId { id = 2, char = 'c' }
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "to left right"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 5, char = 'b' }
                    |> KeySet.insert Character.byId { id = 2, char = 'c' }
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "to right"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 15, char = 'b' }
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "to right left"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 15, char = 'b' }
                    |> KeySet.insert Character.byId { id = 12, char = 'c' }
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "to right right"
            (\_ ->
                KeySet.only { id = 10, char = 'a' }
                    |> KeySet.insert Character.byId { id = 15, char = 'b' }
                    |> KeySet.insert Character.byId { id = 20, char = 'c' }
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
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
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2 (Fuzz.intRange -40 -10)
            (Fuzz.intRange 10 40)
            "descending keys"
            (\idLow idHigh ->
                List.range idLow idHigh
                    |> List.foldr
                        (\id ->
                            Result.andThen
                                (KeySet.insert Character.byId
                                    { id = id
                                    , char = Char.fromCode (40 + ('A' |> Char.toCode) + id)
                                    }
                                    >> validate Character.byId
                                )
                        )
                        (Ok Emptiable.empty)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            (Fuzz.intRange -40 -10)
            (Fuzz.intRange 10 40)
            "ascending keys"
            (\idLow idHigh ->
                List.range idLow idHigh
                    |> List.foldl
                        (\id ->
                            Result.andThen
                                (KeySet.insert Character.byId
                                    { id = id
                                    , char = Char.fromCode (40 + ('A' |> Char.toCode) + id)
                                    }
                                    >> validate Character.byId
                                )
                        )
                        (Ok Emptiable.empty)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz (Fuzz.list (Fuzz.intRange -20 20))
            "random ids"
            (\ids ->
                ids
                    |> List.foldl
                        (\id ->
                            Result.andThen
                                (KeySet.insert Character.byId
                                    { id = id
                                    , char = Char.fromCode (20 + ('A' |> Char.toCode) + id)
                                    }
                                    >> validate Character.byId
                                )
                        )
                        (Ok Emptiable.empty)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        ]


elementRemoveTest : Test
elementRemoveTest =
    describe "elementRemove"
        [ fuzz Fuzz.int
            "Emptiable.empty"
            (\key ->
                Emptiable.empty
                    |> KeySet.elementRemove Character.byId key
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2 Fuzz.int
            Fuzz.int
            "only"
            (\put delete ->
                KeySet.only { id = put, char = '0' }
                    |> KeySet.elementRemove Character.byId delete
                    |> validate Character.byId
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            (Fuzz.list Character.fuzz)
            (Fuzz.list Fuzz.int)
            "fromList"
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
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
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
                    |> recover Expect.fail
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
            "only"
            (\character ->
                KeySet.only character
                    |> KeySet.size
                    |> Expect.equal 1
            )
        , fuzz (Fuzz.list Character.fuzz)
            "fromList"
            (\list ->
                let
                    uniq =
                        List.Extra.uniqueBy .id list
                in
                KeySet.fromList Character.byId list
                    |> KeySet.size
                    |> Expect.equal (uniq |> List.length)
            )
        ]


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
            "only"
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
        , fuzz2
            Fuzz.int
            (Fuzz.list Character.fuzz)
            "fromList"
            (\key list ->
                KeySet.fromList Character.byId list
                    |> KeySet.element Character.byId key
                    |> Expect.equal
                        (list
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
                                        if element.id > soFar.id then
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
                KeySet.fromList Character.byId
                    [ { id = 3, char = 'A' }
                    , { id = 1, char = 'B' }
                    , { id = 4, char = 'C' }
                    , { id = 5, char = 'D' }
                    , { id = 2, char = 'E' }
                    ]
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
    describe "unifyWith"
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
                                KeySet.First first ->
                                    (first.id |> String.fromInt) ++ (first.char |> String.fromChar)

                                KeySet.Second second ->
                                    String.fromList (List.repeat second.id second.char)

                                KeySet.FirstSecond ( first, second ) ->
                                    String.fromList (List.repeat first.id first.char)
                                        ++ ((second.id |> String.fromInt)
                                                ++ (second.char |> String.fromChar)
                                           )
                            )
                )
    in
    describe "fold2From"
        [ test "second is empty"
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
        , test "first is empty"
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
                        , "ddd3d"
                        , "CC2c"
                        , "B"
                        , ""
                        ]
            )
        ]
