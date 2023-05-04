module KeysSet.Test exposing (suite)

import ArraySized
import Atom
import BracketPair exposing (BracketPair)
import Character exposing (Character)
import Emptiable exposing (Emptiable(..), filled)
import Expect
import Fuzz
import Keys exposing (Keys)
import KeysSet exposing (KeysSet)
import KeysSet.Internal
import Linear exposing (Direction(..))
import List.Extra
import List.Linear
import N exposing (Add1, N2, n0, n1)
import Possibly exposing (Possibly(..))
import Stack
import Test exposing (Test, describe, fuzz, fuzz2, test)
import Tree2
import User
import Util exposing (recover)


suite : Test
suite =
    describe "KeysSet"
        [ createSuite
        , alterSuite
        , scanTest
        , transformTest
        , combineSuite
        , readmeExamplesTest
        ]


createSuite : Test
createSuite =
    describe "create"
        [ fromListSuite
        , fromStackSuite
        ]


treeToString : Emptiable (Tree2.Branch element_) Possibly -> String
treeToString =
    \tree ->
        case tree of
            Emptiable.Empty _ ->
                "()"

            Emptiable.Filled branch ->
                let
                    treeFilled =
                        branch |> filled
                in
                [ treeFilled
                    |> Tree2.children
                    |> .up
                    |> treeToString
                    |> String.split "\n"
                    |> List.map (\s -> "\t\t\t" ++ s)
                    |> String.join "\n"
                , "\n"
                , treeFilled |> Tree2.trunk |> Debug.toString
                , "\n"
                , treeFilled
                    |> Tree2.children
                    |> .down
                    |> treeToString
                    |> String.split "\n"
                    |> List.map (\s -> "\t\t\t" ++ s)
                    |> String.join "\n"
                ]
                    |> String.concat


validate :
    String
    -> Keys element keys (Add1 lastIndex)
    ->
        (Emptiable (KeysSet element keys (Add1 lastIndex)) Possibly
         -> Result String ()
        )
validate context keys =
    \keysSet ->
        let
            keysArray =
                keys |> Keys.toArray
        in
        keysArray
            |> ArraySized.and
                (ArraySized.upTo (keysArray |> ArraySized.length |> N.subtract n1))
            |> ArraySized.map
                (\( order, index ) ->
                    let
                        tree =
                            keysSet |> Emptiable.mapFlat (KeysSet.Internal.treeForIndex index)
                    in
                    case tree |> validateHelp order of
                        Err error ->
                            [ error
                            , " for index "
                            , index |> N.toString
                            , " in\n\n"
                            , tree |> treeToString
                            ]
                                |> String.concat
                                |> Err

                        Ok _ ->
                            if (tree |> Tree2.size) == (keysSet |> KeysSet.size) then
                                () |> Ok

                            else
                                [ "tracking size "
                                , keysSet |> KeysSet.size |> String.fromInt
                                , " does not match with real one "
                                , tree |> Tree2.size |> String.fromInt
                                , " for\n"
                                , case keysSet of
                                    Emptiable.Empty _ ->
                                        "[]"

                                    Emptiable.Filled fill ->
                                        fill |> KeysSet.Internal.treeForIndex n0 |> Tree2.foldFrom [] Down (::) |> List.map Debug.toString |> String.join " "
                                , " :\n\n"
                                , keysSet |> Debug.toString
                                , "\n\nwhere the tree instead is\n"
                                , tree |> treeToString
                                ]
                                    |> String.concat
                                    |> Err
                )
            |> ArraySized.foldFrom
                (() |> Ok)
                Down
                (\result soFar ->
                    case soFar of
                        Ok _ ->
                            result |> Result.mapError List.singleton

                        Err soFarErrors ->
                            case result of
                                Ok _ ->
                                    soFarErrors |> Err

                                Err another ->
                                    soFarErrors |> (::) another |> Err
                )
            |> Result.mapError
                (\error ->
                    context ++ ": " ++ (error |> String.join "\n\n\n")
                )


validateHelp :
    (( element, element ) -> Order)
    ->
        (Emptiable (Tree2.Branch element) Possibly
         -> Result String { height : Int }
        )
validateHelp order tree =
    case tree of
        Empty _ ->
            { height = 0 } |> Ok

        Filled branch ->
            let
                treeFilled =
                    branch |> filled

                checkFurther =
                    Result.andThen
                        (\children ->
                            if ((children.down.height - children.up.height) |> abs) <= 1 then
                                { height =
                                    1 + max children.down.height children.up.height
                                }
                                    |> Ok

                            else
                                [ "height below "
                                , treeFilled |> Tree2.trunk |> Debug.toString
                                , ": "
                                , children.down.height |> String.fromInt
                                , " vs "
                                , children.up.height |> String.fromInt
                                , " - so \n\n"
                                , treeFilled |> Tree2.children |> .down |> Tree2.foldFrom [] Down (::) |> Debug.toString
                                , "\nvs\n"
                                , treeFilled |> Tree2.children |> .up |> Tree2.foldFrom [] Down (::) |> Debug.toString
                                , "\n"
                                ]
                                    |> String.concat
                                    |> Err
                        )
                        (Result.map2 (\down up -> { down = down, up = up })
                            (treeFilled |> Tree2.children |> .down |> validateHelp order)
                            (treeFilled |> Tree2.children |> .up |> validateHelp order)
                        )
            in
            if
                case treeFilled |> Tree2.children |> .down of
                    Empty _ ->
                        False

                    Filled down ->
                        (( treeFilled |> Tree2.trunk
                         , down |> filled |> Tree2.trunk
                         )
                            |> order
                        )
                            /= GT
            then
                [ "element "
                , treeFilled |> Tree2.trunk |> Debug.toString
                , " is <= down"
                ]
                    |> String.concat
                    |> Err

            else if
                case treeFilled |> Tree2.children |> .up of
                    Empty _ ->
                        False

                    Filled up ->
                        (( treeFilled |> Tree2.trunk
                         , up |> filled |> Tree2.trunk
                         )
                            |> order
                        )
                            /= LT
            then
                [ "element "
                , treeFilled |> Tree2.trunk |> Debug.toString
                , " is >= up"
                ]
                    |> String.concat
                    |> Err

            else
                checkFurther


fromStackSuite : Test
fromStackSuite =
    describe "fromStack"
        [ fuzz (Stack.filledFuzz Character.fuzz)
            "validate"
            (\stack ->
                KeysSet.fromStack Character.keys stack
                    |> validate "fromStack" Character.keys
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        ]


fromListSuite : Test
fromListSuite =
    describe "fromList"
        [ test "hardcoded"
            (\() ->
                KeysSet.fromList
                    BracketPair.keys
                    [ { open = 'b', closed = 'B' }
                    , { open = 'a', closed = 'A' }
                    , { open = 'b', closed = 'C' }
                    , { open = 'c', closed = 'A' }
                    , { open = 'c', closed = 'C' }
                    ]
                    |> KeysSet.toList ( BracketPair.keys, .open )
                    |> Expect.equalLists
                        (KeysSet.fromList
                            BracketPair.keys
                            [ { open = 'b', closed = 'B' }
                            , { open = 'a', closed = 'A' }
                            , { open = 'c', closed = 'C' }
                            ]
                            |> KeysSet.toList ( BracketPair.keys, .open )
                        )
            )
        , fuzz (Fuzz.list Character.fuzz)
            "validate"
            (\list ->
                KeysSet.fromList Character.keys list
                    |> validate "fromList" Character.keys
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        ]


alterSuite : Test
alterSuite =
    describe "alter"
        [ elementAlterIfNoCollisionSuite
        , elementAlterReplacingCollisionsSuite
        , insertIfNoCollisionSuite
        , insertReplacingCollisionsSuite
        , mapSuite
        , mapTrySuite
        , removeSuite
        ]


insertIfNoCollisionSuite : Test
insertIfNoCollisionSuite =
    let
        element0 : Character
        element0 =
            { id = 0, char = 'A' }

        element1 : Character
        element1 =
            { id = 1, char = 'B' }
    in
    describe "insertIfNoCollision"
        [ test "hardcoded does ignores duplicates"
            (\() ->
                KeysSet.fromList Character.keys
                    [ element0, element1 ]
                    |> KeysSet.insertIfNoCollision Character.keys element0
                    |> KeysSet.insertIfNoCollision Character.keys element1
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ element0, element1 ]
            )
        , fuzz (Stack.filledFuzz Character.fuzz)
            "ignores partial duplicate"
            (\stack ->
                let
                    initial =
                        stack
                            |> Stack.foldFrom Emptiable.empty
                                Up
                                (KeysSet.insertIfNoCollision Character.keys)
                in
                initial
                    |> KeysSet.insertIfNoCollision Character.keys (stack |> Stack.top)
                    |> Expect.equal
                        initial
            )
        , test "hardcoded insert |> element works"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertIfNoCollision Character.keys element1
                    |> KeysSet.element ( Character.keys, .id ) element1.id
                    |> Emptiable.map .char
                    |> Expect.equal (filled element1.char)
            )
        , test "hardcoded element of absent element is empty by id"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertIfNoCollision Character.keys element1
                    |> KeysSet.element ( Character.keys, .id ) element0.id
                    |> Expect.equal Emptiable.empty
            )
        , test "hardcoded element of absent element 1 is empty by char"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertIfNoCollision Character.keys element1
                    |> KeysSet.element ( Character.keys, .char ) element1.char
                    |> Emptiable.map .id
                    |> Expect.equal (filled element1.id)
            )
        , test "hardcoded element of absent element 0 is empty by char"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertIfNoCollision Character.keys element1
                    |> KeysSet.element ( Character.keys, .char ) element0.char
                    |> Expect.equal Emptiable.empty
            )
        , fuzz Character.fuzz
            "Emptiable.empty"
            (\element ->
                Emptiable.empty
                    |> KeysSet.insertIfNoCollision Character.keys element
                    |> validate "insertIfNoCollision" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to down"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 5, char = 'b' }
                    |> validate "insertIfNoCollision" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to down down"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 5, char = 'b' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 2, char = 'c' }
                    |> validate "insertIfNoCollision" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to down up"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 5, char = 'b' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 2, char = 'c' }
                    |> validate "insertIfNoCollision" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to up"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 15, char = 'b' }
                    |> validate "insertIfNoCollision" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to up down"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 15, char = 'b' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 12, char = 'c' }
                    |> validate "insertIfNoCollision" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to up up"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 15, char = 'b' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 20, char = 'c' }
                    |> validate "insertIfNoCollision" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "validate M-N-O-L-K-Q-P-H-I-A"
            (\() ->
                "MNOLKQPHIA"
                    |> String.toList
                    |> List.foldl
                        (\char ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertIfNoCollision
                                            Character.keys
                                            { char = char, id = Char.toCode char }
                                        |> validate (char |> String.fromChar) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            (Fuzz.intRange -40 -10)
            (Fuzz.intRange 10 40)
            "validate descending keys"
            (\idLow idHigh ->
                List.range idLow idHigh
                    |> List.foldr
                        (\id ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertIfNoCollision
                                            Character.keys
                                            { id = id
                                            , char = Char.fromCode (40 + ('A' |> Char.toCode) + id)
                                            }
                                        |> validate ("id " ++ (id |> String.fromInt)) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            (Fuzz.intRange -40 -10)
            (Fuzz.intRange 10 40)
            "validate ascending keys"
            (\idLow idHigh ->
                List.range idLow idHigh
                    |> List.foldl
                        (\id ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertIfNoCollision
                                            Character.keys
                                            { id = id
                                            , char = Char.fromCode (40 + ('A' |> Char.toCode) + id)
                                            }
                                        |> validate ("id " ++ (id |> String.fromInt)) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz
            (Fuzz.list Character.fuzz)
            "validate"
            (\characters ->
                characters
                    |> List.foldl
                        (\character ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertIfNoCollision Character.keys character
                                        |> Emptiable.emptyAdapt (\_ -> Possible)
                                        |> validate ("char " ++ (character.char |> String.fromChar)) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        ]


insertReplacingCollisionsSuite : Test
insertReplacingCollisionsSuite =
    let
        element0 : Character
        element0 =
            { id = 0, char = 'A' }

        element1 : Character
        element1 =
            { id = 1, char = 'B' }
    in
    describe "insertReplacingCollisions"
        [ test "hardcoded overwrites duplicate id"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 0, char = 'B' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 1, char = 'A' }
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 0, char = 'B' }, { id = 1, char = 'A' } ]
            )
        , test "hardcoded overwrites duplicate char"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 10, char = 'A' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 11, char = 'B' }
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 10, char = 'A' }, { id = 11, char = 'B' } ]
            )
        , test "hardcoded insert |> element works"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertReplacingCollisions Character.keys element1
                    |> KeysSet.element ( Character.keys, .id ) element1.id
                    |> Emptiable.map .char
                    |> Expect.equal (filled element1.char)
            )
        , test "hardcoded element of absent element is empty by id"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertReplacingCollisions Character.keys element1
                    |> KeysSet.element ( Character.keys, .id ) element0.id
                    |> Expect.equal Emptiable.empty
            )
        , test "hardcoded element of absent element 1 is empty by char"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertReplacingCollisions Character.keys element1
                    |> KeysSet.element ( Character.keys, .char ) element1.char
                    |> Emptiable.map .id
                    |> Expect.equal (filled element1.id)
            )
        , test "hardcoded element of absent element 0 is empty by char"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertReplacingCollisions Character.keys element1
                    |> KeysSet.element ( Character.keys, .char ) element0.char
                    |> Expect.equal Emptiable.empty
            )
        , fuzz Character.fuzz
            "Emptiable.empty"
            (\element ->
                Emptiable.empty
                    |> KeysSet.insertReplacingCollisions Character.keys element
                    |> validate "insertReplacingCollisions" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to down"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 5, char = 'b' }
                    |> validate "insertReplacingCollisions" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to down down"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 5, char = 'b' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 2, char = 'c' }
                    |> validate "insertReplacingCollisions" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to down up"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 5, char = 'b' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 2, char = 'c' }
                    |> validate "insertReplacingCollisions" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to up"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 15, char = 'b' }
                    |> validate "insertReplacingCollisions" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to up down"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 15, char = 'b' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 12, char = 'c' }
                    |> validate "insertReplacingCollisions" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "hardcoded validate to up up"
            (\() ->
                KeysSet.one { id = 10, char = 'a' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 15, char = 'b' }
                    |> KeysSet.insertReplacingCollisions Character.keys { id = 20, char = 'c' }
                    |> validate "insertReplacingCollisions" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , test "validate M-N-O-L-K-Q-P-H-I-A"
            (\() ->
                "MNOLKQPHIA"
                    |> String.toList
                    |> List.foldl
                        (\char ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertReplacingCollisions
                                            Character.keys
                                            { char = char, id = Char.toCode char }
                                        |> validate (char |> String.fromChar) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            (Fuzz.intRange -40 -10)
            (Fuzz.intRange 10 40)
            "validate descending keys"
            (\idLow idHigh ->
                List.range idLow idHigh
                    |> List.foldr
                        (\id ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertReplacingCollisions
                                            Character.keys
                                            { id = id
                                            , char = Char.fromCode (40 + ('A' |> Char.toCode) + id)
                                            }
                                        |> validate ("id " ++ (id |> String.fromInt)) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            (Fuzz.intRange -40 -10)
            (Fuzz.intRange 10 40)
            "validate ascending keys"
            (\idLow idHigh ->
                List.range idLow idHigh
                    |> List.foldl
                        (\id ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertReplacingCollisions
                                            Character.keys
                                            { id = id
                                            , char = Char.fromCode (40 + ('A' |> Char.toCode) + id)
                                            }
                                        |> validate ("id " ++ (id |> String.fromInt)) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz
            (Fuzz.list Character.fuzz)
            "validate"
            (\characters ->
                characters
                    |> List.foldl
                        (\character ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.insertReplacingCollisions Character.keys character
                                        |> Emptiable.emptyAdapt (\_ -> Possible)
                                        |> validate ("char " ++ (character.char |> String.fromChar)) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (Emptiable.empty |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        ]


removeSuite : Test
removeSuite =
    let
        openClosedBrackets =
            KeysSet.one { open = '(', closed = ')' }
    in
    describe "remove"
        [ let
            ab : Emptiable (KeysSet Character Character.Keys N2) Possibly
            ab =
                KeysSet.fromList
                    Character.keys
                    [ { id = 0, char = 'A' }
                    , { id = 1, char = 'B' }
                    ]
          in
          test "hardcoded insert |> remove id leaves it unchanged"
            (\() ->
                ab
                    |> KeysSet.insertIfNoCollision Character.keys { id = 2, char = 'C' }
                    |> KeysSet.remove ( Character.keys, .id ) 2
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        (ab |> KeysSet.toList ( Character.keys, .id ))
            )
        , test "hardcoded nothing to remove"
            (\() ->
                openClosedBrackets
                    |> KeysSet.remove ( BracketPair.keys, .open ) ')'
                    --> no change, .open is never ')'
                    |> Expect.equal openClosedBrackets
            )
        , test "hardcoded something to remove"
            (\() ->
                openClosedBrackets
                    |> KeysSet.remove ( BracketPair.keys, .closed ) ')'
                    |> KeysSet.toList ( BracketPair.keys, .closed )
                    |> Expect.equalLists []
            )
        , fuzz Fuzz.int
            "Emptiable.empty"
            (\id ->
                Emptiable.empty
                    |> KeysSet.remove ( Character.keys, .id ) id
                    |> validate "remove" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            Fuzz.int
            Fuzz.int
            "one"
            (\put delete ->
                KeysSet.one { id = put, char = '0' }
                    |> KeysSet.remove ( Character.keys, .id ) delete
                    |> validate "remove" Character.keys
                    |> Result.map (\() -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz2
            (Fuzz.list Character.fuzz)
            (Fuzz.list Fuzz.int)
            "fromList"
            (\puts deletes ->
                deletes
                    |> List.foldl
                        (\id ->
                            Result.andThen
                                (\keysSet ->
                                    keysSet
                                        |> KeysSet.remove ( Character.keys, .id ) id
                                        |> validate ("remove id " ++ (id |> String.fromInt)) Character.keys
                                        |> Result.map (\() -> keysSet)
                                )
                        )
                        (KeysSet.fromList Character.keys puts |> Ok)
                    |> Result.map (\_ -> Expect.pass)
                    |> recover Expect.fail
            )
        , fuzz (Fuzz.list Character.fuzz)
            "fromList clear"
            (\characters ->
                let
                    full =
                        KeysSet.fromList Character.keys characters
                in
                characters
                    |> List.Linear.foldFrom
                        (full
                            |> validate "fromList" Character.keys
                            |> Result.map (\() -> full)
                        )
                        Up
                        (\{ id } ->
                            Result.andThen
                                (\keysSet ->
                                    let
                                        removed =
                                            keysSet
                                                |> KeysSet.remove ( Character.keys, .id ) id
                                    in
                                    removed
                                        |> validate ("remove id " ++ (id |> String.fromInt)) Character.keys
                                        |> Result.mapError
                                            (\error ->
                                                [ "before:\n\n"
                                                , case keysSet of
                                                    Emptiable.Empty _ ->
                                                        "empty"

                                                    Emptiable.Filled keysSetFill ->
                                                        keysSetFill |> KeysSet.Internal.treeForIndex n1 |> treeToString
                                                , "\n\n"
                                                , error
                                                ]
                                                    |> String.concat
                                            )
                                        |> Result.map (\() -> removed)
                                )
                        )
                    |> Result.map (Expect.equal Emptiable.empty)
                    |> recover Expect.fail
            )
        ]


elementAlterIfNoCollisionSuite : Test
elementAlterIfNoCollisionSuite =
    describe "elementAlterIfNoCollision"
        [ test "replace to same key"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeysSet.elementAlterIfNoCollision ( Character.keys, .id )
                        1
                        (\c -> { c | char = 'C' })
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }, { id = 1, char = 'C' } ]
            )
        , test "don't replace to multiple collisions"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeysSet.elementAlterIfNoCollision ( Character.keys, .id )
                        1
                        (\c -> { c | id = 0 })
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
            )
        ]


elementAlterReplacingCollisionsSuite : Test
elementAlterReplacingCollisionsSuite =
    describe "elementAlterReplacingCollisions"
        [ test "replace to same key"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeysSet.elementAlterReplacingCollisions ( Character.keys, .id )
                        1
                        (\c -> { c | char = 'C' })
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 0, char = 'A' }, { id = 1, char = 'C' } ]
            )
        , test "replace to multiple collisions"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }, { id = 1, char = 'B' } ]
                    |> KeysSet.elementAlterReplacingCollisions ( Character.keys, .id )
                        1
                        (\c -> { c | id = 0 })
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 0, char = 'B' } ]
            )
        ]


mapSuite : Test
mapSuite =
    describe "map"
        [ test "hardcoded alter"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 3, char = 'A' }
                    , { id = 1, char = 'B' }
                    , { id = 4, char = 'C' }
                    , { id = 5, char = 'D' }
                    , { id = 3, char = 'E' }
                    ]
                    |> KeysSet.map (\element -> { element | id = element.id * 10 })
                        Character.keys
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 10, char = 'B' }
                        , { id = 30, char = 'A' }
                        , { id = 40, char = 'C' }
                        , { id = 50, char = 'D' }
                        ]
            )
        ]


mapTrySuite : Test
mapTrySuite =
    describe "mapTry"
        [ test "hardcoded filter"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 3, char = 'A' }
                    , { id = 1, char = 'B' }
                    , { id = 4, char = 'C' }
                    , { id = 5, char = 'D' }
                    , { id = 2, char = 'E' }
                    ]
                    |> KeysSet.mapTry
                        (\element ->
                            if element.id > 3 || element.char == 'B' then
                                element |> filled

                            else
                                Emptiable.empty
                        )
                        Character.keys
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 1, char = 'B' }
                        , { id = 4, char = 'C' }
                        , { id = 5, char = 'D' }
                        ]
            )
        ]


combineSuite : Test
combineSuite =
    describe "combine"
        [ unifyWithSuite
        , intersectSuite
        , exceptSuite
        , fold2FromSuite
        ]


unifyWithSuite : Test
unifyWithSuite =
    describe "unifyWith"
        [ test "down is empty"
            (\() ->
                Emptiable.empty
                    |> KeysSet.unifyWith Character.keys
                        (KeysSet.one { id = 0, char = 'A' })
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "up is empty"
            (\() ->
                KeysSet.one { id = 0, char = 'A' }
                    |> KeysSet.unifyWith Character.keys Emptiable.empty
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "unions"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }
                    , { id = 1, char = 'B' }
                    , { id = 2, char = 'c' }
                    , { id = 3, char = 'd' }
                    ]
                    |> KeysSet.unifyWith Character.keys
                        (KeysSet.fromList Character.keys
                            [ { id = 2, char = 'c' }
                            , { id = 3, char = 'd' }
                            , { id = 4, char = 'e' }
                            , { id = 5, char = 'f' }
                            ]
                        )
                    |> KeysSet.toList ( Character.keys, .id )
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
        [ test "down is empty"
            (\() ->
                KeysSet.one { id = 0, char = 'A' }
                    |> KeysSet.intersect ( Character.keys, .id ) Emptiable.empty
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists []
            )
        , test "up is empty"
            (\() ->
                Emptiable.empty
                    |> KeysSet.intersect ( Character.keys, .id )
                        (KeysSet.one { id = 0, char = 'A' })
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists []
            )
        , test "hardcoded"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 2, char = 'c' }
                    , { id = 3, char = 'd' }
                    , { id = 4, char = 'e' }
                    , { id = 5, char = 'f' }
                    ]
                    |> KeysSet.intersect ( Character.keys, .id )
                        (KeysSet.fromList Character.keys
                            [ { id = 0, char = 'A' }
                            , { id = 1, char = 'B' }
                            , { id = 2, char = 'c' }
                            , { id = 3, char = 'd' }
                            ]
                        )
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 2, char = 'c' }
                        , { id = 3, char = 'd' }
                        ]
            )
        ]


exceptSuite : Test
exceptSuite =
    describe "except"
        [ test "down is empty"
            (\() ->
                Emptiable.empty
                    |> KeysSet.except ( Character.keys, .id )
                        (KeysSet.one { id = 0, char = 'A' }
                            |> KeysSet.toKeys ( Character.keys, .id )
                        )
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists []
            )
        , test "up is empty"
            (\() ->
                KeysSet.one { id = 0, char = 'A' }
                    |> KeysSet.except ( Character.keys, .id )
                        Emptiable.empty
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "hardcoded"
            (\() ->
                KeysSet.fromList Character.keys
                    [ { id = 0, char = 'A' }
                    , { id = 1, char = 'B' }
                    , { id = 2, char = 'c' }
                    , { id = 3, char = 'd' }
                    ]
                    |> KeysSet.except ( Character.keys, .id )
                        (KeysSet.fromList Character.keys
                            [ { id = 2, char = 'c' }
                            , { id = 3, char = 'd' }
                            , { id = 4, char = 'e' }
                            , { id = 5, char = 'f' }
                            ]
                            |> KeysSet.toKeys ( Character.keys, .id )
                        )
                    |> KeysSet.toList ( Character.keys, .id )
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
            KeysSet.fold2From
                []
                (\toBeMerged soFar ->
                    soFar
                        |> (::)
                            (case toBeMerged of
                                KeysSet.First first ->
                                    (first.id |> String.fromInt) ++ (first.char |> String.fromChar)

                                KeysSet.Second second ->
                                    String.fromList (List.repeat second.id second.char)

                                KeysSet.FirstSecond ( first, second ) ->
                                    String.fromList (List.repeat first.id first.char)
                                        ++ ((second.id |> String.fromInt)
                                                ++ (second.char |> String.fromChar)
                                           )
                            )
                )
    in
    describe "fold2From"
        [ test "hardcoded second is empty"
            (\() ->
                { first =
                    { key = ( Character.keys, .id )
                    , set = KeysSet.one { id = 0, char = 'A' }
                    }
                , second =
                    { key = ( Character.keys, .id )
                    , set = Emptiable.empty
                    }
                }
                    |> testFold2
                    |> Expect.equalLists
                        [ "0A"
                        ]
            )
        , test "hardcoded first is empty"
            (\() ->
                { first =
                    { key = ( Character.keys, .id )
                    , set = Emptiable.empty
                    }
                , second =
                    { key = ( Character.keys, .id )
                    , set = KeysSet.one { id = 3, char = 'A' }
                    }
                }
                    |> testFold2
                    |> Expect.equalLists
                        [ "AAA"
                        ]
            )
        , test "hardcoded"
            (\() ->
                testFold2
                    { first =
                        { key = ( Character.keys, .id )
                        , set =
                            KeysSet.fromList Character.keys
                                [ { id = 2, char = 'C' }
                                , { id = 3, char = 'd' }
                                , { id = 4, char = 'e' }
                                , { id = 5, char = 'f' }
                                ]
                        }
                    , second =
                        { key = ( Character.keys, .id )
                        , set =
                            KeysSet.fromList Character.keys
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


scanTest : Test
scanTest =
    describe "scan"
        [ sizeSuite
        , elementSuite
        , endUpSuite
        , endUpSuite
        ]


sizeSuite : Test
sizeSuite =
    describe "size"
        [ test "Emptiable.empty"
            (\() ->
                Emptiable.empty
                    |> KeysSet.size
                    |> Expect.equal 0
            )
        , fuzz Character.fuzz
            "one"
            (\character ->
                KeysSet.one character
                    |> KeysSet.size
                    |> Expect.equal 1
            )
        , fuzz (Fuzz.list Character.fuzz)
            "fromList unique"
            (\list ->
                let
                    unique =
                        list
                            |> List.Extra.uniqueBy .id
                            |> List.Extra.uniqueBy .char
                in
                KeysSet.fromList Character.keys unique
                    |> KeysSet.size
                    |> Expect.equal (unique |> List.length)
            )
        ]


elementSuite : Test
elementSuite =
    describe "element"
        [ fuzz Fuzz.int
            "Emptiable.empty"
            (\id ->
                Emptiable.empty
                    |> KeysSet.element ( Character.keys, .id ) id
                    |> Expect.equal Emptiable.empty
            )
        , fuzz2 Fuzz.int
            Fuzz.int
            "one"
            (\x y ->
                KeysSet.one { id = x, char = 'A' }
                    |> KeysSet.element ( Character.keys, .id ) y
                    |> Expect.equal
                        (if x == y then
                            filled { id = x, char = 'A' }

                         else
                            Emptiable.empty
                        )
            )
        , fuzz2
            Fuzz.int
            (Fuzz.list Character.fuzz
                |> Fuzz.map (List.Extra.uniqueBy .char)
            )
            "fromList"
            (\id list ->
                KeysSet.fromList Character.keys list
                    |> KeysSet.element ( Character.keys, .id ) id
                    |> Expect.equal
                        (list
                            |> List.Extra.find (\character -> character.id == id)
                            |> Emptiable.fromMaybe
                        )
            )
        , let
            casedLetters =
                KeysSet.fromList
                    BracketPair.keys
                    [ { open = 'a', closed = 'A' }
                    , { open = 'b', closed = 'B' }
                    ]

            open char =
                casedLetters
                    |> KeysSet.element ( BracketPair.keys, .closed ) char
                    |> Emptiable.map .open

            closed char =
                casedLetters
                    |> KeysSet.element ( BracketPair.keys, .open ) char
                    |> Emptiable.map .closed
          in
          test "fromList hardcoded"
            (\() ->
                [ open 'a', open 'B', closed 'b', closed 'A' ]
                    |> Expect.equal
                        [ Emptiable.empty, filled 'b', filled 'B', Emptiable.empty ]
            )
        ]


endUpSuite : Test
endUpSuite =
    describe "end Up"
        [ fuzz Character.fuzz
            "one"
            (\character ->
                KeysSet.one character
                    |> KeysSet.end ( Character.keys, .id ) Up
                    |> Expect.equal character
            )
        , fuzz
            (Character.fuzz
                |> Fuzz.andThen
                    (\top ->
                        Fuzz.list Character.fuzz
                            |> Fuzz.map
                                (\list ->
                                    Stack.topBelow
                                        top
                                        (list
                                            |> List.filter (\c -> c.char /= top.char)
                                            |> List.Extra.uniqueBy .char
                                        )
                                )
                    )
            )
            "fromStack"
            (\stack ->
                KeysSet.fromStack Character.keys stack
                    |> KeysSet.end ( Character.keys, .id ) Up
                    |> Expect.equal
                        (stack
                            |> Stack.fold Up
                                (\element soFar ->
                                    if element.id > soFar.id then
                                        element

                                    else
                                        soFar
                                )
                        )
            )
        ]
endUpSuite : Test
endUpSuite =
    describe "end Down"
        [ fuzz Character.fuzz
            "one"
            (\character ->
                KeysSet.one character
                    |> KeysSet.end ( Character.keys, .id ) Down
                    |> Expect.equal character
            )
        , fuzz
            (Character.fuzz
                |> Fuzz.andThen
                    (\top ->
                        Fuzz.list Character.fuzz
                            |> Fuzz.map
                                (\list ->
                                    Stack.topBelow
                                        top
                                        (list
                                            |> List.filter (\c -> c.char /= top.char)
                                            |> List.Extra.uniqueBy .char
                                        )
                                )
                    )
            )
            "fromStack"
            (\stack ->
                KeysSet.fromStack Character.keys stack
                    |> KeysSet.end ( Character.keys, .id ) Down
                    |> Expect.equal
                        (stack
                            |> Stack.fold Up
                                (\element soFar ->
                                    if element.id < soFar.id then
                                        element

                                    else
                                        soFar
                                )
                        )
            )
        , test "hardcoded"
            (\() ->
                KeysSet.fromStack
                    BracketPair.keys
                    (Stack.topBelow
                        { open = 'a', closed = 'B' }
                        [ { open = 'b', closed = 'A' }
                        ]
                    )
                    |> KeysSet.end ( BracketPair.keys, .open ) Down
                    |> Expect.equal
                        { open = 'a', closed = 'B' }
            )
        ]


transformTest : Test
transformTest =
    describe "transform"
        [ foldFromSuite
        , toListSuite
        ]


toListSuite : Test
toListSuite =
    describe "toList"
        [ test "empty"
            (\() ->
                Emptiable.empty
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists []
            )
        , test "one"
            (\() ->
                KeysSet.one { id = 0, char = 'A' }
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists [ { id = 0, char = 'A' } ]
            )
        , test "hardcoded insert"
            (\() ->
                Emptiable.empty
                    |> KeysSet.insertIfNoCollision Character.keys { id = 2, char = 'A' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 0, char = 'B' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 5, char = 'C' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 3, char = 'E' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 1, char = 'F' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 4, char = 'G' }
                    |> KeysSet.insertIfNoCollision Character.keys { id = 3, char = 'B' }
                    |> KeysSet.toList ( Character.keys, .id )
                    |> Expect.equalLists
                        [ { id = 0, char = 'B' }
                        , { id = 1, char = 'F' }
                        , { id = 2, char = 'A' }
                        , { id = 3, char = 'E' }
                        , { id = 4, char = 'G' }
                        , { id = 5, char = 'C' }
                        ]
            )
        , test "hardcoded fromList"
            (\() ->
                KeysSet.fromList
                    BracketPair.keys
                    [ { open = 'a', closed = 'A' }
                    , { open = 'b', closed = 'B' }
                    ]
                    |> KeysSet.toList ( BracketPair.keys, .open )
                    |> Expect.equalLists
                        [ { open = 'a', closed = 'A' }
                        , { open = 'b', closed = 'B' }
                        ]
            )
        , fuzz (Fuzz.list Character.fuzz)
            "toList is unique"
            (\list ->
                let
                    toListResult =
                        KeysSet.fromList Character.keys list
                            |> KeysSet.toList ( Character.keys, .id )
                in
                toListResult
                    |> Expect.equalLists
                        (toListResult
                            |> List.Extra.uniqueBy .char
                            |> List.Extra.uniqueBy .id
                        )
            )
        ]


foldFromSuite : Test
foldFromSuite =
    describe "foldFrom"
        [ fuzz
            (Fuzz.list Character.fuzz
                |> Fuzz.map
                    (\list ->
                        list
                            |> List.Extra.uniqueBy .id
                            |> List.Extra.uniqueBy .char
                    )
            )
            "hardcoded sum"
            (\list ->
                KeysSet.fromList Character.keys list
                    |> KeysSet.foldFrom ( Character.keys, .id ) 0 Up (\c soFar -> soFar + c.id)
                    |> Expect.equal
                        (list
                            |> List.map .id
                            |> List.sum
                        )
            )
        , test "hardcoded any"
            (\() ->
                KeysSet.fromList
                    User.byEmail
                    [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
                    , { username = "gria", priority = 3, email = "miggo@inlook.com" }
                    ]
                    |> KeysSet.foldFrom ( User.byEmail, .email )
                        False
                        Up
                        (\user soFar -> soFar || (user.priority >= 4))
                    |> Expect.equal False
            )
        , test "hardcoded all"
            (\() ->
                KeysSet.fromList
                    User.byEmail
                    [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
                    , { username = "gria", priority = 3, email = "miggo@inlook.com" }
                    ]
                    |> KeysSet.foldFrom ( User.byEmail, .email )
                        True
                        Up
                        (\user soFar -> soFar && (user.priority <= 3))
                    |> Expect.equal True
            )
        ]


readmeExamplesTest : Test
readmeExamplesTest =
    describe "readme examples"
        [ test "braces"
            (\() ->
                let
                    brackets : Emptiable (KeysSet BracketPair BracketPair.Keys N2) Possibly
                    brackets =
                        KeysSet.fromList
                            BracketPair.keys
                            [ { open = '(', closed = ')' }
                            , { open = '{', closed = '}' }
                            ]

                    typeChar char =
                        case
                            brackets |> KeysSet.element ( BracketPair.keys, .open ) char
                        of
                            Emptiable.Filled { closed } ->
                                [ char, closed ] |> String.fromList

                            Emptiable.Empty _ ->
                                case
                                    brackets |> KeysSet.element ( BracketPair.keys, .closed ) char
                                of
                                    Emptiable.Filled { open } ->
                                        [ open, char ] |> String.fromList

                                    Emptiable.Empty _ ->
                                        char |> String.fromChar
                in
                Expect.equal ([ '(', '}' ] |> List.map typeChar)
                    [ "()", "{}" ]
            )
        , test "cased letters"
            (\() ->
                let
                    lowerUppercaseLetters =
                        KeysSet.fromList BracketPair.keys
                            [ { open = 'a', closed = 'A' }
                            , { open = 'b', closed = 'B' }
                            , { open = 'c', closed = 'C' }
                            ]

                    closedFor char =
                        lowerUppercaseLetters
                            |> KeysSet.element ( BracketPair.keys, .open ) char
                            |> Emptiable.map .closed
                in
                [ 'c', 'a', 'x' ]
                    |> List.map closedFor
                    |> Expect.equal
                        [ filled 'C', filled 'A', Emptiable.empty ]
            )
        , test "periodic table"
            (\() ->
                let
                    elements =
                        KeysSet.fromList Atom.keys
                            [ { symbol = "H", name = "Hydrogen", atomicNumber = 1 }
                            , { symbol = "He", name = "Helium", atomicNumber = 2 }
                            ]

                    atomicNumberOfElementWithSymbol : String -> Emptiable Int Possibly
                    atomicNumberOfElementWithSymbol symbol =
                        elements
                            |> KeysSet.element ( Atom.keys, .symbol ) symbol
                            |> Emptiable.map .atomicNumber
                in
                [ atomicNumberOfElementWithSymbol "He"
                , atomicNumberOfElementWithSymbol "Wtf"
                , atomicNumberOfElementWithSymbol "H"
                ]
                    |> Expect.equal
                        [ filled 2, Emptiable.empty, filled 1 ]
            )
        ]
