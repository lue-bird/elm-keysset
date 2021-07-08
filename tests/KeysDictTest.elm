module KeysDictTest exposing (suite)

import AssocList as AssocDict
import Expect
import KeysDict exposing (KeysDict)
import KeysDict.Uniqueness exposing (door)
import Serialize
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "KeysDict"
        [ scanTest
        , inTest
        , removeTest
        , transformTest
        , readmeExamplesTest
        ]


type alias CharWithCode =
    { char : Char, code : Int }


at0 : CharWithCode
at0 =
    { code = 0, char = 'A' }


at1 : CharWithCode
at1 =
    { code = 1, char = 'B' }


with2 : KeysDict CharWithCode
with2 =
    KeysDict.enterableBy
        [ door .code, door .char ]
        |> KeysDict.insert at0
        |> KeysDict.insert at1


type alias BracketMatch =
    { open : Char, closed : Char }


brackets : KeysDict BracketMatch
brackets =
    KeysDict.enterableBy
        [ door .open, door .closed ]
        |> KeysDict.insert { open = '(', closed = ')' }
        |> KeysDict.insert { open = '{', closed = '}' }


scanTest : Test
scanTest =
    describe "scan"
        [ describe "size"
            [ test "empty" <|
                \() ->
                    KeysDict.enterableBy
                        [ door .code, door .char ]
                        |> KeysDict.size
                        |> Expect.equal 0
            , test "same size as a list of unique elements" <|
                \() ->
                    List.range 0 41
                        |> List.map (\i -> ( i, i ))
                        |> List.foldl KeysDict.insert
                            (KeysDict.enterableBy
                                [ door Tuple.first, door Tuple.second ]
                            )
                        |> KeysDict.size
                        |> Expect.equal 42
            ]
        , describe "at"
            (let
                casedLetters =
                    KeysDict.enterableBy
                        [ door .lowercase, door .uppercase ]
                        |> KeysDict.insert { lowercase = 'a', uppercase = 'A' }
                        |> KeysDict.insert { lowercase = 'b', uppercase = 'B' }

                lowercase char =
                    casedLetters
                        |> KeysDict.at { door = .uppercase, key = char }
                        |> Maybe.map .lowercase

                uppercase char =
                    casedLetters
                        |> KeysDict.at { door = .lowercase, key = char }
                        |> Maybe.map .uppercase
             in
             [ test "finds lowercase" <|
                \() ->
                    List.map lowercase [ 'a', 'B' ]
                        |> Expect.equal
                            [ Nothing, Just 'b' ]
             , test "finds uppercase" <|
                \() ->
                    List.map uppercase [ 'b', 'A' ]
                        |> Expect.equal
                            [ Just 'B', Nothing ]
             ]
            )
        , test "equal example works" <|
            \() ->
                let
                    letterCodes =
                        [ { letter = 'a', code = 97 }
                        , { letter = 'b', code = 98 }
                        ]
                            |> List.foldl KeysDict.insert
                                (KeysDict.enterableBy
                                    [ door .letter, door .code ]
                                )

                    fancyCompetingLetterCodes =
                        KeysDict.enterableBy
                            [ door .code, door .letter ]
                            |> KeysDict.insert { code = 98, letter = 'b' }
                            |> KeysDict.insert { code = 97, letter = 'a' }
                in
                KeysDict.equal
                    letterCodes
                    fancyCompetingLetterCodes
                    |> Expect.true "from reversed list equal to before"
        , describe "toList examples work"
            [ test "isEmpty" <|
                \() ->
                    let
                        isEmpty =
                            List.isEmpty << KeysDict.toList
                    in
                    Expect.true "isEmpty for filled False, ifEmpty True"
                        (isEmpty
                            (KeysDict.enterableBy
                                [ door .code, door .char ]
                            )
                            && not (isEmpty with2)
                        )
            , test "most recently inserted" <|
                \() ->
                    let
                        mostRecentlyInserted =
                            List.head << KeysDict.toList
                    in
                    mostRecentlyInserted
                        (KeysDict.enterableBy
                            [ door .lowercase, door .uppercase ]
                            |> KeysDict.insert
                                { lowercase = 'a', uppercase = 'A' }
                            |> KeysDict.insert
                                { lowercase = 'b', uppercase = 'B' }
                        )
                        |> Expect.equal
                            (Just { lowercase = 'b', uppercase = 'B' })
            ]
        ]


inTest : Test
inTest =
    describe "in"
        [ describe "insert"
            [ test "ignored for duplicates" <|
                \() ->
                    KeysDict.size
                        (with2
                            |> KeysDict.insert at0
                            |> KeysDict.insert at1
                        )
                        |> Expect.equal 2
            , test "access code is Just letter of inserted pair" <|
                \() ->
                    KeysDict.enterableBy
                        [ door .code, door .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at { door = .code, key = .code at1 }
                        |> Maybe.map .char
                        |> Expect.equal (Just (.char at1))
            , test "code is Nothing if not of inserted pair" <|
                \() ->
                    KeysDict.enterableBy
                        [ door .code, door .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at { door = .code, key = .code at0 }
                        |> Maybe.map .char
                        |> Expect.equal Nothing
            , test "char is Just left of inserted pair" <|
                \() ->
                    KeysDict.enterableBy
                        [ door .code, door .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at { door = .char, key = .char at1 }
                        |> Maybe.map .code
                        |> Expect.equal (Just (.code at1))
            , test "char is Nothing if not of inserted pair" <|
                \() ->
                    KeysDict.enterableBy
                        [ door .code, door .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at { door = .char, key = .char at0 }
                        |> Maybe.map .code
                        |> Expect.equal Nothing
            , test "insert example" <|
                \() ->
                    let
                        result =
                            KeysDict.enterableBy
                                [ door .lowercase, door .uppercase ]
                                -- lowercase and uppercase are unique keys across each value
                                |> KeysDict.insert { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                                -- put in
                                |> KeysDict.insert { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                                -- put in, because rating is not a key
                                |> KeysDict.insert { lowercase = 'b', uppercase = 'C', rating = 0 }
                                -- ignored, the left value already exists
                                |> KeysDict.insert { lowercase = 'c', uppercase = 'A', rating = 0 }
                                -- ignored, the right value already exists
                                |> KeysDict.insert { lowercase = 'c', uppercase = 'C', rating = 0.6 }

                        --put in
                    in
                    KeysDict.equal
                        result
                        (KeysDict.enterableBy
                            [ door .lowercase, door .uppercase ]
                            |> KeysDict.insert
                                { lowercase = 'c', uppercase = 'C', rating = 0.6 }
                            |> KeysDict.insert
                                { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                            |> KeysDict.insert
                                { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                        )
                        |> Expect.true "keys are unique, others aren't"
            ]
        ]


removeTest : Test
removeTest =
    describe "remove"
        [ test "insert |> remove code leaves it unchanged" <|
            \() ->
                with2
                    |> KeysDict.insert { code = 2, char = 'C' }
                    |> KeysDict.remove { door = .code, key = 2 }
                    |> Expect.equal with2
        , test "insert |> remove char leaves it unchanged" <|
            \() ->
                with2
                    |> KeysDict.insert { code = 2, char = 'C' }
                    |> KeysDict.remove { door = .char, key = 'C' }
                    |> Expect.equal with2
        ]


transformTest : Test
transformTest =
    describe "transform"
        [ test "fold works as in the example" <|
            \() ->
                let
                    openingAndClosing =
                        brackets
                            |> KeysDict.fold
                                (\{ open, closed } ->
                                    (::) (String.fromList [ open, closed ])
                                )
                                []
                in
                Expect.equal
                    openingAndClosing
                    [ "()", "{}" ]
        , test "map works as in the example" <|
            \() ->
                let
                    digitNames =
                        KeysDict.enterableBy
                            [ door .number, door .name ]
                            |> KeysDict.insert { number = 0, name = "zero" }
                            |> KeysDict.insert { number = 1, name = "one" }

                    mathSymbolNames =
                        digitNames
                            |> KeysDict.map
                                (\{ number, name } ->
                                    { symbol = String.fromInt number, name = name }
                                )
                                [ door .symbol, door .name ]
                            |> KeysDict.insert { symbol = "+", name = "plus" }
                in
                Expect.true "mapped KeysDict equal to put up"
                    (KeysDict.equal
                        mathSymbolNames
                        (KeysDict.enterableBy
                            [ door .symbol, door .name ]
                            |> KeysDict.insert { symbol = "0", name = "zero" }
                            |> KeysDict.insert { symbol = "1", name = "one" }
                            |> KeysDict.insert { symbol = "+", name = "plus" }
                        )
                    )
        , test "toAssocList example works" <|
            \() ->
                let
                    casedLetters =
                        KeysDict.enterableBy
                            [ door .lowercase, door .uppercase ]
                            |> KeysDict.insert { uppercase = 'A', lowercase = 'a' }
                            |> KeysDict.insert { uppercase = 'B', lowercase = 'b' }

                    lowerFromUppercase =
                        casedLetters
                            |> KeysDict.toDict { key = .uppercase, value = .lowercase }
                in
                Expect.true "KeysDict.insert |> toDict equal to AssocList.fromList"
                    (AssocDict.eq
                        lowerFromUppercase
                        (AssocDict.fromList [ ( 'A', 'a' ), ( 'B', 'b' ) ])
                    )
        , serializeTest
        ]


serializeTest : Test
serializeTest =
    test "json encoded & decoded is the same" <|
        \() ->
            let
                serializeCharWithCode =
                    Serialize.record CharWithCode
                        |> Serialize.field .char
                            (Serialize.map Char.fromCode
                                Char.toCode
                                Serialize.int
                            )
                        |> Serialize.field .code Serialize.int
                        |> Serialize.finishRecord

                serializeCharWithCodeMultiDict =
                    KeysDict.serialize serializeCharWithCode
                        [ door .code, door .char ]

                encodedDecoded =
                    Serialize.encodeToJson
                        serializeCharWithCodeMultiDict
                        with2
                        |> Serialize.decodeFromJson
                            serializeCharWithCodeMultiDict
            in
            case encodedDecoded of
                Ok decoded ->
                    Expect.true "encoded |> decoded equal to before"
                        (KeysDict.equal decoded with2)

                Err err ->
                    --too lazy to case on import Serialize.Error(..)
                    Expect.fail (Debug.toString err)


type Element
    = Hydrogen
    | Helium


readmeExamplesTest : Test
readmeExamplesTest =
    describe "readme examples work"
        [ test "braces" <|
            \() ->
                let
                    typeChar character =
                        brackets
                            |> KeysDict.at { door = .open, key = character }
                            |> Maybe.map
                                (\{ closed } ->
                                    String.fromList [ character, closed ]
                                )
                            |> Maybe.withDefault
                                (brackets
                                    |> KeysDict.at { door = .closed, key = character }
                                    |> Maybe.map
                                        (\{ open } ->
                                            String.fromList [ open, character ]
                                        )
                                    |> Maybe.withDefault
                                        (String.fromChar character)
                                )
                in
                Expect.equal
                    ([ '(', '}' ] |> List.map typeChar)
                    [ "()", "{}" ]
        , test "cased letters" <|
            \() ->
                let
                    lowerUppercaseLetters =
                        KeysDict.enterableBy
                            [ door .lowercase, door .uppercase ]
                            |> KeysDict.insert
                                { lowercase = 'a', uppercase = 'A' }
                            |> KeysDict.insert
                                { lowercase = 'b', uppercase = 'B' }
                            |> KeysDict.insert
                                { lowercase = 'c', uppercase = 'C' }

                    upperCase char =
                        lowerUppercaseLetters
                            |> KeysDict.at { door = .lowercase, key = char }
                            |> Maybe.map .uppercase
                in
                Expect.equal
                    ([ 'c', 'a', 'x' ] |> List.map upperCase)
                    [ Just 'C', Just 'A', Nothing ]
        , test "periodic table" <|
            \() ->
                let
                    elementAtomicNumberPairdict =
                        KeysDict.enterableBy [ door .element, door .atomicNumber ]
                            |> KeysDict.insert
                                { element = Hydrogen, atomicNumber = 1 }
                            |> KeysDict.insert
                                { element = Helium, atomicNumber = 2 }

                    atomicNumberByElement =
                        elementAtomicNumberPairdict
                            |> KeysDict.toAssocList
                                { key = .element, value = .atomicNumber }
                in
                [ atomicNumberByElement |> AssocDict.get Helium
                , atomicNumberByElement |> AssocDict.get Hydrogen
                ]
                    |> Expect.equal [ Just 2, Just 1 ]
        ]
