module MultiSetTest exposing (suite)

import Expect
import MultiSet exposing (MultiSet, unique)
import Serialize
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "MultiSet"
        [ uniquenessTest
        , scanTest
        , insertTest
        , removeTest
        , transformTest
        , readmeExamplesTest
        ]


uniquenessTest : Test
uniquenessTest =
    describe "MultiSet.Uniqueness"
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


with2 : MultiSet CharWithCode
with2 =
    MultiSet.promising
        [ unique .code, unique .char ]
        |> MultiSet.insertAll [ at0, at1 ]


type alias BracketMatch =
    { open : Char, closed : Char }


brackets : MultiSet BracketMatch
brackets =
    MultiSet.promising
        [ unique .open, unique .closed ]
        |> MultiSet.insertAll
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]


scanTest : Test
scanTest =
    describe "scan"
        [ describe "size"
            [ test "empty"
                (\() ->
                    MultiSet.promising
                        [ unique .code, unique .char ]
                        |> MultiSet.size
                        |> Expect.equal 0
                )
            , test "same size as a list of unique elements"
                (\() ->
                    List.range 0 41
                        |> List.map (\i -> ( i, i ))
                        |> List.foldl MultiSet.insert
                            (MultiSet.promising
                                [ unique Tuple.first, unique Tuple.second ]
                            )
                        |> MultiSet.size
                        |> Expect.equal 42
                )
            ]
        , describe "at"
            (let
                casedLetters =
                    MultiSet.promising
                        [ unique .lowercase, unique .uppercase ]
                        |> MultiSet.insertAll
                            [ { lowercase = 'a', uppercase = 'A' }
                            , { lowercase = 'b', uppercase = 'B' }
                            ]

                lowercase char =
                    casedLetters
                        |> MultiSet.at .uppercase char
                        |> Maybe.map .lowercase

                uppercase char =
                    casedLetters
                        |> MultiSet.at .lowercase char
                        |> Maybe.map .uppercase

                ratedOperators =
                    MultiSet.promising
                        [ unique .symbol, unique .name ]
                        |> MultiSet.insertAll
                            [ { rating = 0.5, symbol = "<", name = "lt" }
                            , { rating = 0.5, symbol = ">", name = "gt" }
                            ]
             in
             [ test "finds lowercase"
                (\() ->
                    List.map lowercase [ 'a', 'B' ]
                        |> Expect.equal [ Nothing, Just 'b' ]
                )
             , test "finds uppercase"
                (\() ->
                    List.map uppercase [ 'b', 'A' ]
                        |> Expect.equal [ Just 'B', Nothing ]
                )
             , test "finds most recently inserted non-unique"
                (\() ->
                    MultiSet.at .rating 0.5 ratedOperators
                        |> Expect.equal
                            ({ rating = 0.5, symbol = ">", name = "gt" }
                                |> Just
                            )
                )
             ]
            )
        , test "equal example works"
            (\() ->
                let
                    letterCodes =
                        MultiSet.promising
                            [ unique .letter, unique .code ]
                            |> MultiSet.insertAll
                                [ { letter = 'a', code = 97 }
                                , { letter = 'b', code = 98 }
                                ]

                    fancyCompetingLetterCodes =
                        MultiSet.promising
                            [ unique .code, unique .letter ]
                            |> MultiSet.insertAll
                                [ { code = 98, letter = 'b' }
                                , { code = 97, letter = 'a' }
                                ]
                in
                MultiSet.equal letterCodes
                    fancyCompetingLetterCodes
                    |> Expect.true "from reversed list equal to before"
            )
        , describe "toList examples work"
            [ test "isEmpty"
                (\() ->
                    let
                        isEmpty =
                            List.isEmpty << MultiSet.toList
                    in
                    Expect.true "isEmpty for filled False, ifEmpty True"
                        (isEmpty
                            (MultiSet.promising
                                [ unique .code, unique .char ]
                            )
                            && not (isEmpty with2)
                        )
                )
            , test "most recently inserted"
                (\() ->
                    let
                        mostRecentlyInserted =
                            List.head << MultiSet.toList
                    in
                    mostRecentlyInserted
                        (MultiSet.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> MultiSet.insertAll
                                [ { lowercase = 'a', uppercase = 'A' }
                                , { lowercase = 'b', uppercase = 'B' }
                                ]
                        )
                        |> Expect.equal
                            ({ lowercase = 'b', uppercase = 'B' }
                                |> Just
                            )
                )
            ]
        , test "any"
            (\() ->
                MultiSet.promising
                    [ unique .username, unique .email ]
                    |> MultiSet.insertAll
                        [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
                        , { username = "gria", priority = 3, email = "miggo@inlook.com" }
                        ]
                    |> MultiSet.any (\user -> user.priority > 4)
                    |> Expect.equal False
            )
        , test "all"
            (\() ->
                MultiSet.promising
                    [ unique .username, unique .email ]
                    |> MultiSet.insertAll
                        [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
                        , { username = "gria", priority = 3, email = "miggo@inlook.com" }
                        ]
                    |> MultiSet.all (\user -> user.priority < 4)
                    |> Expect.equal True
            )
        , let
            letters =
                MultiSet.promising
                    [ unique .lowercase, unique .uppercase ]
                    |> MultiSet.insertAll
                        [ { lowercase = 'a', uppercase = 'A' }
                        , { lowercase = 'b', uppercase = 'B' }
                        ]
          in
          describe "isUnique"
            [ test "not unique"
                (\() ->
                    Expect.all
                        [ MultiSet.isUnique
                            { lowercase = 'b', uppercase = 'C' }
                            -- the .lowercase already exists
                            >> Expect.equal False
                        , MultiSet.isUnique
                            { lowercase = 'c', uppercase = 'A' }
                            -- the .uppercase already exists
                            >> Expect.equal False
                        ]
                        letters
                )
            , test "unique"
                (\() ->
                    letters
                        |> MultiSet.isUnique
                            { lowercase = 'c', uppercase = 'C' }
                        |> Expect.equal True
                )
            ]
        ]


insertTest : Test
insertTest =
    describe "insert and insertAll"
        [ test "ignored for duplicates"
            (\() ->
                MultiSet.size
                    (with2
                        |> MultiSet.insert at0
                        |> MultiSet.insert at1
                    )
                    |> Expect.equal 2
            )
        , test "access code is Just letter of inserted pair"
            (\() ->
                MultiSet.promising [ unique .code, unique .char ]
                    |> MultiSet.insert at1
                    |> MultiSet.at .code at1.code
                    |> Maybe.map .char
                    |> Expect.equal (Just at1.char)
            )
        , test "code is Nothing if not of inserted pair"
            (\() ->
                MultiSet.promising [ unique .code, unique .char ]
                    |> MultiSet.insert at1
                    |> MultiSet.at .code at0.code
                    |> Maybe.map .char
                    |> Expect.equal Nothing
            )
        , test "char is Just left of inserted pair"
            (\() ->
                MultiSet.promising [ unique .code, unique .char ]
                    |> MultiSet.insert at1
                    |> MultiSet.at .char at1.char
                    |> Maybe.map .code
                    |> Expect.equal (Just at1.code)
            )
        , test "char is Nothing if not of inserted pair"
            (\() ->
                MultiSet.promising [ unique .code, unique .char ]
                    |> MultiSet.insert at1
                    |> MultiSet.at .char at0.char
                    |> Maybe.map .code
                    |> Expect.equal Nothing
            )
        , test "insert example"
            (\() ->
                let
                    result =
                        MultiSet.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> MultiSet.insertAll
                                [ { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                                , { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                                , { lowercase = 'b', uppercase = 'C', rating = 0 }
                                , { lowercase = 'c', uppercase = 'A', rating = 0 }
                                , { lowercase = 'c', uppercase = 'C', rating = 0.6 }
                                ]
                in
                MultiSet.equal result
                    (MultiSet.promising
                        [ unique .lowercase, unique .uppercase ]
                        |> MultiSet.insertAll
                            [ { lowercase = 'c', uppercase = 'C', rating = 0.6 }
                            , { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                            , { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                            ]
                    )
                    |> Expect.true "keys are unique, others aren't"
            )
        ]


removeTest : Test
removeTest =
    let
        openClosedBrackets =
            MultiSet.promising
                [ unique .open, unique .closed ]
                |> MultiSet.insert
                    { open = "(", closed = ")" }
    in
    describe "take elements out"
        [ describe "remove"
            [ test "insert |> remove code leaves it unchanged"
                (\() ->
                    with2
                        |> MultiSet.insert { code = 2, char = 'C' }
                        |> MultiSet.remove .code 2
                        |> Expect.equal with2
                )
            , test "insert |> remove char leaves it unchanged"
                (\() ->
                    with2
                        |> MultiSet.insert { code = 2, char = 'C' }
                        |> MultiSet.remove .char 'C'
                        |> Expect.equal with2
                )
            , test "nothing to remove"
                (\() ->
                    openClosedBrackets
                        |> MultiSet.remove .open ")"
                        --> no change, .open is never ")"
                        |> Expect.equal openClosedBrackets
                )
            , test "something to remove"
                (\() ->
                    openClosedBrackets
                        |> MultiSet.remove .closed ")"
                        |> MultiSet.toList
                        |> Expect.equalLists []
                )
            , test "non-unique aspect"
                (\() ->
                    MultiSet.promising
                        [ unique .open, unique .closed ]
                        |> MultiSet.insertAll
                            [ { open = "[", closed = "]", meaning = List }
                            , { open = "<", closed = ">", meaning = Custom }
                            , { open = "\\", closed = "/", meaning = Custom }
                            ]
                        |> MultiSet.remove .meaning Custom
                        |> MultiSet.toList
                        |> Expect.equalLists
                            [ { open = "[", closed = "]", meaning = List }
                            ]
                )
            ]
        , let
            operators =
                MultiSet.promising
                    [ unique .symbol, unique .name ]
                    |> MultiSet.insertAll
                        [ { symbol = ">", name = "gt" }
                        , { symbol = "<", name = "lt" }
                        , { symbol = "==", name = "eq" }
                        ]
          in
          test "when"
            (\() ->
                operators
                    |> MultiSet.when
                        (.symbol >> String.length >> (==) 1)
                    |> MultiSet.equal
                        (MultiSet.promising
                            [ unique .symbol, unique .name ]
                            |> MultiSet.insertAll
                                [ { symbol = ">", name = "gt" }
                                , { symbol = "<", name = "lt" }
                                ]
                        )
                    |> Expect.equal True
            )
        ]


type BracketMeaning
    = List
    | Custom


transformTest : Test
transformTest =
    describe "transform"
        [ test "fold works as in the example"
            (\() ->
                let
                    openingAndClosing =
                        brackets
                            |> MultiSet.fold
                                (\{ open, closed } ->
                                    (::) (String.fromList [ open, closed ])
                                )
                                []
                in
                Expect.equalLists
                    openingAndClosing
                    [ "()", "{}" ]
            )
        , serializeTest
        ]


serializeTest : Test
serializeTest =
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

        serializeCharWithCodeMultiSet =
            MultiSet.serialize serializeCharWithCode
                [ unique .code, unique .char ]
    in
    test "json encoded & decoded is the same"
        (\() ->
            let
                encodedDecoded =
                    Serialize.encodeToJson
                        serializeCharWithCodeMultiSet
                        with2
                        |> Serialize.decodeFromJson
                            serializeCharWithCodeMultiSet
            in
            case encodedDecoded of
                Ok decoded ->
                    Expect.true "encoded |> decoded equal to before"
                        (MultiSet.equal decoded with2)

                Err err ->
                    -- too lazy to case on import Serialize.Error(..)
                    Expect.fail (Debug.toString err)
        )


readmeExamplesTest : Test
readmeExamplesTest =
    describe "readme examples work"
        [ test "braces"
            (\() ->
                let
                    typeChar char =
                        brackets |> MultiSet.at .open char |> Maybe.map (\{ closed } -> String.fromList [ char, closed ]) |> Maybe.withDefault (brackets |> MultiSet.at .closed char |> Maybe.map (\{ open } -> String.fromList [ open, char ]) |> Maybe.withDefault (String.fromChar char))
                in
                Expect.equal ([ '(', '}' ] |> List.map typeChar)
                    [ "()", "{}" ]
            )
        , test "cased letters"
            (\() ->
                let
                    lowerUppercaseLetters =
                        MultiSet.promising [ unique .lowercase, unique .uppercase ]
                            |> MultiSet.insertAll
                                [ { lowercase = 'a', uppercase = 'A' }
                                , { lowercase = 'b', uppercase = 'B' }
                                , { lowercase = 'c', uppercase = 'C' }
                                ]

                    upperCase char =
                        lowerUppercaseLetters |> MultiSet.at .lowercase char |> Maybe.map .uppercase
                in
                Expect.equal ([ 'c', 'a', 'x' ] |> List.map upperCase)
                    [ Just 'C', Just 'A', Nothing ]
            )
        , test "periodic table"
            (\() ->
                let
                    elements =
                        MultiSet.promising [ unique .atomicNumber, unique .symbol ]
                            |> MultiSet.insertAll
                                [ { symbol = "H", name = "Hydrogen", atomicNumber = 1 }
                                , { symbol = "He", name = "Helium", atomicNumber = 2 }
                                ]

                    atomicNumberOfElementWithName : String -> Maybe Int
                    atomicNumberOfElementWithName name =
                        elements |> MultiSet.at .name name |> Maybe.map .atomicNumber
                in
                [ atomicNumberOfElementWithName "Helium"
                , atomicNumberOfElementWithName "Hydrogen"
                ]
                    |> Expect.equal [ Just 2, Just 1 ]
            )
        ]
