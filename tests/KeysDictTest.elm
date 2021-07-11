module KeysDictTest exposing (suite)

import Expect
import KeysDict exposing (KeysDict, unique)
import Serialize
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "KeysDict"
        [ uniquenessTest
        , scanTest
        , inTest
        , removeTest
        , transformTest
        , readmeExamplesTest
        ]


uniquenessTest : Test
uniquenessTest =
    describe "KeysDict.Uniqueness"
        [ describe "unique"
            [ test "equal aspect"
                (\() ->
                    unique .name
                        { name = "smile", symbol = '😊' }
                        { symbol = '🙂', name = "smile" }
                        |> Expect.equal { areUnique = False }
                )
            , test "different aspect"
                (\() ->
                    unique .symbol
                        { name = "smile", symbol = '😊' }
                        { symbol = '🙂', name = "smile" }
                        |> Expect.equal { areUnique = True }
                )
            , test "combined aspect"
                (\() ->
                    unique (\person -> person.firstName ++ person.lastName)
                        { lastName = "jimmy", firstName = "petter" }
                        { lastName = "jimmy", firstName = "greg" }
                        |> Expect.equal { areUnique = True }
                )
            ]
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
    KeysDict.promising
        [ unique .code, unique .char ]
        |> KeysDict.insert at0
        |> KeysDict.insert at1


type alias BracketMatch =
    { open : Char, closed : Char }


brackets : KeysDict BracketMatch
brackets =
    KeysDict.promising
        [ unique .open, unique .closed ]
        |> KeysDict.insert { open = '(', closed = ')' }
        |> KeysDict.insert { open = '{', closed = '}' }


scanTest : Test
scanTest =
    describe "scan"
        [ describe "size"
            [ test "empty" <|
                \() ->
                    KeysDict.promising
                        [ unique .code, unique .char ]
                        |> KeysDict.size
                        |> Expect.equal 0
            , test "same size as a list of unique elements" <|
                \() ->
                    List.range 0 41
                        |> List.map (\i -> ( i, i ))
                        |> List.foldl KeysDict.insert
                            (KeysDict.promising
                                [ unique Tuple.first, unique Tuple.second ]
                            )
                        |> KeysDict.size
                        |> Expect.equal 42
            ]
        , describe "at"
            (let
                casedLetters =
                    KeysDict.promising
                        [ unique .lowercase, unique .uppercase ]
                        |> KeysDict.insert
                            { lowercase = 'a', uppercase = 'A' }
                        |> KeysDict.insert
                            { lowercase = 'b', uppercase = 'B' }

                lowercase char =
                    casedLetters
                        |> KeysDict.at .uppercase char
                        |> Maybe.map .lowercase

                uppercase char =
                    casedLetters
                        |> KeysDict.at .lowercase char
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
                                (KeysDict.promising
                                    [ unique .letter, unique .code ]
                                )

                    fancyCompetingLetterCodes =
                        KeysDict.promising
                            [ unique .code, unique .letter ]
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
                            (KeysDict.promising
                                [ unique .code, unique .char ]
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
                        (KeysDict.promising
                            [ unique .lowercase, unique .uppercase ]
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
                    KeysDict.promising
                        [ unique .code, unique .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at .code at1.code
                        |> Maybe.map .char
                        |> Expect.equal (Just at1.char)
            , test "code is Nothing if not of inserted pair" <|
                \() ->
                    KeysDict.promising
                        [ unique .code, unique .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at .code at0.code
                        |> Maybe.map .char
                        |> Expect.equal Nothing
            , test "char is Just left of inserted pair" <|
                \() ->
                    KeysDict.promising
                        [ unique .code, unique .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at .char at1.char
                        |> Maybe.map .code
                        |> Expect.equal (Just at1.code)
            , test "char is Nothing if not of inserted pair" <|
                \() ->
                    KeysDict.promising
                        [ unique .code, unique .char ]
                        |> KeysDict.insert at1
                        |> KeysDict.at .char at0.char
                        |> Maybe.map .code
                        |> Expect.equal Nothing
            , test "insert example" <|
                \() ->
                    let
                        result =
                            KeysDict.promising
                                [ unique .lowercase, unique .uppercase ]
                                -- lowercase and uppercase are unique keys across each value
                                |> KeysDict.insert { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                                -- is inserted
                                |> KeysDict.insert { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                                -- is inserted, because rating is not a key
                                |> KeysDict.insert { lowercase = 'b', uppercase = 'C', rating = 0 }
                                -- ignored, the left value already exists
                                |> KeysDict.insert { lowercase = 'c', uppercase = 'A', rating = 0 }
                                -- ignored, the right value already exists
                                |> KeysDict.insert { lowercase = 'c', uppercase = 'C', rating = 0.6 }

                        -- is inserted
                    in
                    KeysDict.equal
                        result
                        (KeysDict.promising
                            [ unique .lowercase, unique .uppercase ]
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
                    |> KeysDict.remove .code 2
                    |> Expect.equal with2
        , test "insert |> remove char leaves it unchanged" <|
            \() ->
                with2
                    |> KeysDict.insert { code = 2, char = 'C' }
                    |> KeysDict.remove .char 'C'
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
                        KeysDict.promising
                            [ unique .number, unique .name ]
                            |> KeysDict.insert { number = 0, name = "zero" }
                            |> KeysDict.insert { number = 1, name = "one" }

                    mathSymbolNames =
                        digitNames
                            |> KeysDict.toList
                            |> List.map
                                (\{ number, name } ->
                                    { symbol = String.fromInt number, name = name }
                                )
                            |> (\list ->
                                    KeysDict.promising [ unique .symbol, unique .name ]
                                        |> KeysDict.insertAll list
                               )
                            |> KeysDict.insert { symbol = "+", name = "plus" }
                in
                Expect.true "mapped KeysDict equal to put up"
                    (KeysDict.equal
                        mathSymbolNames
                        (KeysDict.promising
                            [ unique .symbol, unique .name ]
                            |> KeysDict.insert { symbol = "0", name = "zero" }
                            |> KeysDict.insert { symbol = "1", name = "one" }
                            |> KeysDict.insert { symbol = "+", name = "plus" }
                        )
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

                serializeCharWithCodeKeysDict =
                    KeysDict.serialize serializeCharWithCode
                        [ unique .code, unique .char ]

                encodedDecoded =
                    Serialize.encodeToJson
                        serializeCharWithCodeKeysDict
                        with2
                        |> Serialize.decodeFromJson
                            serializeCharWithCodeKeysDict
            in
            case encodedDecoded of
                Ok decoded ->
                    Expect.true "encoded |> decoded equal to before"
                        (KeysDict.equal decoded with2)

                Err err ->
                    -- too lazy to case on import Serialize.Error(..)
                    Expect.fail (Debug.toString err)


readmeExamplesTest : Test
readmeExamplesTest =
    describe "readme examples work"
        [ test "braces" <|
            \() ->
                let
                    typeChar char =
                        brackets
                            |> KeysDict.at .open char
                            |> Maybe.map
                                (\{ closed } ->
                                    String.fromList [ char, closed ]
                                )
                            |> Maybe.withDefault
                                (brackets
                                    |> KeysDict.at .closed char
                                    |> Maybe.map
                                        (\{ open } ->
                                            String.fromList [ open, char ]
                                        )
                                    |> Maybe.withDefault
                                        (String.fromChar char)
                                )
                in
                Expect.equal
                    ([ '(', '}' ] |> List.map typeChar)
                    [ "()", "{}" ]
        , test "cased letters" <|
            \() ->
                let
                    lowerUppercaseLetters =
                        KeysDict.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> KeysDict.insertAll
                                [ { lowercase = 'a', uppercase = 'A' }
                                , { lowercase = 'b', uppercase = 'B' }
                                , { lowercase = 'c', uppercase = 'C' }
                                ]

                    upperCase char =
                        lowerUppercaseLetters
                            |> KeysDict.at .lowercase char
                            |> Maybe.map .uppercase
                in
                Expect.equal
                    ([ 'c', 'a', 'x' ] |> List.map upperCase)
                    [ Just 'C', Just 'A', Nothing ]
        , test "periodic table" <|
            \() ->
                let
                    elements =
                        KeysDict.promising
                            [ unique .atomicNumber, unique .symbol ]
                            |> KeysDict.insertAll
                                [ { symbol = "H", name = "Hydrogen", atomicNumber = 1 }
                                , { symbol = "He", name = "Helium", atomicNumber = 2 }
                                ]

                    atomicNumberOfElementWithName : String -> Maybe Int
                    atomicNumberOfElementWithName name =
                        elements
                            |> KeysDict.at .name name
                            |> Maybe.map .atomicNumber
                in
                [ atomicNumberOfElementWithName "Helium"
                , atomicNumberOfElementWithName "Hydrogen"
                ]
                    |> Expect.equal [ Just 2, Just 1 ]
        ]
