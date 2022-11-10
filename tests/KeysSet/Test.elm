module KeysSet.Test exposing (suite)

{-| Anyone have the motivation to make those fuzz tests?
-}

import Expect
import KeysSet exposing (KeysSet, unique)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "KeysSet"
        [ uniquenessTest
        , scanTest
        , insertTest
        , removeTest
        , transformTest
        , readmeExamplesTest
        ]


uniquenessTest : Test
uniquenessTest =
    describe "KeysSet.Uniqueness"
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


scanTest : Test
scanTest =
    describe "scan"
        [ describe "isEmpty"
            [ test "True for empty"
                (\() ->
                    Expect.onFail "isEmpty for filled False, ifEmpty True"
                        (KeysSet.isEmpty
                            (KeysSet.promising
                                [ unique .code, unique .char ]
                            )
                            |> Expect.equal True
                        )
                )
            , test "False for filled"
                (\() ->
                    KeysSet.promising
                        [ unique .code, unique .letter ]
                        |> KeysSet.insert { code = 98, letter = 'b' }
                        |> KeysSet.isEmpty
                        |> Expect.equal False
                )
            ]
        , describe "size"
            [ test "empty"
                (\() ->
                    KeysSet.promising
                        [ unique .code, unique .char ]
                        |> KeysSet.size
                        |> Expect.equal 0
                )
            , test "same size as a list of unique elements"
                (\() ->
                    List.range 0 41
                        |> List.map (\i -> ( i, i ))
                        |> List.foldl KeysSet.insert
                            (KeysSet.promising
                                [ unique Tuple.first, unique Tuple.second ]
                            )
                        |> KeysSet.size
                        |> Expect.equal 42
                )
            ]
        , describe "at"
            (let
                casedLetters =
                    KeysSet.promising
                        [ unique .lowercase, unique .uppercase ]
                        |> KeysSet.insertList
                            [ { lowercase = 'a', uppercase = 'A' }
                            , { lowercase = 'b', uppercase = 'B' }
                            ]

                lowercase char =
                    casedLetters
                        |> KeysSet.element ( .uppercase, char )
                        |> Maybe.map .lowercase

                uppercase char =
                    casedLetters
                        |> KeysSet.element ( .lowercase, char )
                        |> Maybe.map .uppercase

                ratedOperators =
                    KeysSet.promising
                        [ unique .symbol, unique .name ]
                        |> KeysSet.insertList
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
                    ratedOperators
                        |> KeysSet.element ( .rating, 0.5 )
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
                        KeysSet.promising
                            [ unique .letter, unique .code ]
                            |> KeysSet.insertList
                                [ { letter = 'a', code = 97 }
                                , { letter = 'b', code = 98 }
                                ]

                    fancyCompetingLetterCodes =
                        KeysSet.promising
                            [ unique .code, unique .letter ]
                            |> KeysSet.insert
                                { code = 98, letter = 'b' }
                            |> KeysSet.insert
                                { code = 97, letter = 'a' }
                in
                Expect.onFail "from reversed list equal to before"
                    (KeysSet.isEqualTo letterCodes
                        fancyCompetingLetterCodes
                        |> Expect.equal True
                    )
            )
        , describe "toList examples"
            [ test "after insertAll"
                (\() ->
                    KeysSet.promising
                        [ unique .open, unique .closed ]
                        |> KeysSet.insertList
                            [ { open = '(', closed = ')' }
                            , { open = '{', closed = '}' }
                            ]
                        |> KeysSet.toList
                        |> Expect.equalLists
                            [ { open = '{', closed = '}' }
                            , { open = '(', closed = ')' }
                            ]
                )
            , test "most recently inserted"
                (\() ->
                    let
                        mostRecentlyInserted =
                            KeysSet.toList >> List.head
                    in
                    mostRecentlyInserted
                        (KeysSet.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> KeysSet.insertList
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
                KeysSet.promising
                    [ unique .username, unique .email ]
                    |> KeysSet.insertList
                        [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
                        , { username = "gria", priority = 3, email = "miggo@inlook.com" }
                        ]
                    |> KeysSet.any (\user -> user.priority > 4)
                    |> Expect.equal False
            )
        , test "all"
            (\() ->
                KeysSet.promising
                    [ unique .username, unique .email ]
                    |> KeysSet.insertList
                        [ { username = "fred", priority = 1, email = "higgi@outlook.com" }
                        , { username = "gria", priority = 3, email = "miggo@inlook.com" }
                        ]
                    |> KeysSet.all (\user -> user.priority < 4)
                    |> Expect.equal True
            )
        , let
            letters =
                KeysSet.promising
                    [ unique .lowercase, unique .uppercase ]
                    |> KeysSet.insertList
                        [ { lowercase = 'a', uppercase = 'A' }
                        , { lowercase = 'b', uppercase = 'B' }
                        ]
          in
          describe "isUnique"
            [ describe "not unique"
                [ test "lowercase"
                    (\() ->
                        KeysSet.isUniqueIn letters
                            { lowercase = 'b', uppercase = 'C' }
                            -- the .lowercase already exists
                            |> Expect.equal False
                    )
                , test "uppercase"
                    (\() ->
                        KeysSet.isUniqueIn letters
                            { lowercase = 'c', uppercase = 'A' }
                            -- the .uppercase already exists
                            |> Expect.equal False
                    )
                ]
            , test "unique"
                (\() ->
                    KeysSet.isUniqueIn letters
                        { lowercase = 'c', uppercase = 'C' }
                        |> Expect.equal True
                )
            ]
        ]


insertTest : Test
insertTest =
    let
        element0 : CharWithCode
        element0 =
            { code = 0, char = 'A' }

        element1 : CharWithCode
        element1 =
            { code = 1, char = 'B' }

        with2 : KeysSet CharWithCode
        with2 =
            KeysSet.promising
                [ unique .code, unique .char ]
                |> KeysSet.insertList [ element0, element1 ]
    in
    describe "insert and insertAll"
        [ test "ignored for duplicates"
            (\() ->
                KeysSet.size
                    (with2
                        |> KeysSet.insert element0
                        |> KeysSet.insert element1
                    )
                    |> Expect.equal 2
            )
        , test "access code is Just letter of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert element1
                    |> KeysSet.element ( .code, element1.code )
                    |> Maybe.map .char
                    |> Expect.equal (Just element1.char)
            )
        , test "code is Nothing if not of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert element1
                    |> KeysSet.element ( .code, element0.code )
                    |> Maybe.map .char
                    |> Expect.equal Nothing
            )
        , test "char is Just left of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert element1
                    |> KeysSet.element ( .char, element1.char )
                    |> Maybe.map .code
                    |> Expect.equal (Just element1.code)
            )
        , test "char is Nothing if not of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert element1
                    |> KeysSet.element ( .char, element0.char )
                    |> Maybe.map .code
                    |> Expect.equal Nothing
            )
        , test "insert example"
            (\() ->
                let
                    result =
                        KeysSet.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> KeysSet.insertList
                                [ { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                                , { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                                , { lowercase = 'b', uppercase = 'C', rating = 0 }
                                , { lowercase = 'c', uppercase = 'A', rating = 0 }
                                , { lowercase = 'c', uppercase = 'C', rating = 0.6 }
                                ]
                in
                Expect.onFail "keys are unique, others aren't"
                    (KeysSet.isEqualTo result
                        (KeysSet.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> KeysSet.insertList
                                [ { lowercase = 'c', uppercase = 'C', rating = 0.6 }
                                , { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                                , { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                                ]
                        )
                        |> Expect.equal True
                    )
            )
        ]


removeTest : Test
removeTest =
    let
        openClosedBrackets =
            KeysSet.promising
                [ unique .open, unique .closed ]
                |> KeysSet.insert
                    { open = "(", closed = ")" }
    in
    describe "take elements out"
        [ describe "remove"
            [ let
                ab : KeysSet CharWithCode
                ab =
                    KeysSet.promising
                        [ unique .code, unique .char ]
                        |> KeysSet.insertList
                            [ { code = 0, char = 'A' }
                            , { code = 1, char = 'B' }
                            ]
              in
              test "insert |> remove code leaves it unchanged"
                (\() ->
                    ab
                        |> KeysSet.insert { code = 2, char = 'C' }
                        |> KeysSet.elementRemove ( .code, 2 )
                        |> Expect.equal ab
                )
            , test "nothing to remove"
                (\() ->
                    openClosedBrackets
                        |> KeysSet.elementRemove ( .open, ")" )
                        --> no change, .open is never ")"
                        |> Expect.equal openClosedBrackets
                )
            , test "something to remove"
                (\() ->
                    openClosedBrackets
                        |> KeysSet.elementRemove ( .closed, ")" )
                        |> KeysSet.toList
                        |> Expect.equalLists []
                )
            , test "non-unique aspect"
                (\() ->
                    KeysSet.promising
                        [ unique .open, unique .closed ]
                        |> KeysSet.insertList
                            [ { open = "[", closed = "]", meaning = List }
                            , { open = "<", closed = ">", meaning = Custom }
                            , { open = "\\", closed = "/", meaning = Custom }
                            ]
                        |> KeysSet.elementRemove ( .meaning, Custom )
                        |> KeysSet.toList
                        |> Expect.equalLists
                            [ { open = "[", closed = "]", meaning = List }
                            ]
                )
            ]
        , let
            operators =
                KeysSet.promising
                    [ unique .symbol, unique .name ]
                    |> KeysSet.insertList
                        [ { symbol = ">", name = "gt" }
                        , { symbol = "<", name = "lt" }
                        , { symbol = "==", name = "eq" }
                        ]
          in
          test "mapTry"
            (\() ->
                operators
                    |> KeysSet.mapTry
                        (\operator ->
                            case operator.symbol |> String.length of
                                1 ->
                                    operator.name |> Just

                                _ ->
                                    Nothing
                        )
                        [ unique identity ]
                    |> KeysSet.isEqualTo
                        (KeysSet.promising
                            [ unique identity ]
                            |> KeysSet.insertList
                                [ "gt", "lt" ]
                        )
                    |> Expect.equal True
            )
        ]


transformTest : Test
transformTest =
    describe "transform"
        [ test "fold example"
            (\() ->
                let
                    brackets : KeysSet { open : Char, closed : Char }
                    brackets =
                        KeysSet.promising
                            [ unique .open, unique .closed ]
                            |> KeysSet.insertList
                                [ { open = '(', closed = ')' }
                                , { open = '{', closed = '}' }
                                ]

                    openingAndClosing =
                        brackets
                            |> KeysSet.foldFrom []
                                (\{ open, closed } ->
                                    (::) (String.fromList [ open, closed ])
                                )
                in
                openingAndClosing
                    |> Expect.equalLists [ "()", "{}" ]
            )
        , test "toList example"
            (\() ->
                KeysSet.promising
                    [ unique .open, unique .closed ]
                    |> KeysSet.insertList
                        [ { open = '(', closed = ')' }
                        , { open = '{', closed = '}' }
                        ]
                    |> KeysSet.toList
                    |> Expect.equalLists
                        [ { open = '{', closed = '}' }
                        , { open = '(', closed = ')' }
                        ]
            )
        ]


readmeExamplesTest : Test
readmeExamplesTest =
    describe "readme examples"
        [ test "braces"
            (\() ->
                let
                    brackets : KeysSet { open : Char, closed : Char }
                    brackets =
                        KeysSet.promising
                            [ unique .open, unique .closed ]
                            |> KeysSet.insertList
                                [ { open = '(', closed = ')' }
                                , { open = '{', closed = '}' }
                                ]

                    typeChar char =
                        brackets
                            |> KeysSet.element ( .open, char )
                            |> Maybe.map
                                (\{ closed } ->
                                    String.fromList [ char, closed ]
                                )
                            |> Maybe.withDefault
                                (brackets
                                    |> KeysSet.element ( .closed, char )
                                    |> Maybe.map
                                        (\{ open } ->
                                            String.fromList [ open, char ]
                                        )
                                    |> Maybe.withDefault (String.fromChar char)
                                )
                in
                Expect.equal ([ '(', '}' ] |> List.map typeChar)
                    [ "()", "{}" ]
            )
        , test "cased letters"
            (\() ->
                let
                    lowerUppercaseLetters =
                        KeysSet.promising [ unique .lowercase, unique .uppercase ]
                            |> KeysSet.insertList
                                [ { lowercase = 'a', uppercase = 'A' }
                                , { lowercase = 'b', uppercase = 'B' }
                                , { lowercase = 'c', uppercase = 'C' }
                                ]

                    upperCase char =
                        lowerUppercaseLetters
                            |> KeysSet.element ( .lowercase, char )
                            |> Maybe.map .uppercase
                in
                Expect.equal ([ 'c', 'a', 'x' ] |> List.map upperCase)
                    [ Just 'C', Just 'A', Nothing ]
            )
        , test "periodic table"
            (\() ->
                let
                    elements =
                        KeysSet.promising [ unique .atomicNumber, unique .symbol ]
                            |> KeysSet.insertList
                                [ { symbol = "H", name = "Hydrogen", atomicNumber = 1 }
                                , { symbol = "He", name = "Helium", atomicNumber = 2 }
                                ]

                    atomicNumberOfElementWithName : String -> Maybe Int
                    atomicNumberOfElementWithName name =
                        elements
                            |> KeysSet.element ( .name, name )
                            |> Maybe.map .atomicNumber
                in
                [ atomicNumberOfElementWithName "Helium"
                , atomicNumberOfElementWithName "Hydrogen"
                ]
                    |> Expect.equal [ Just 2, Just 1 ]
            )
        ]


type alias CharWithCode =
    { char : Char, code : Int }


type BracketMeaning
    = List
    | Custom
