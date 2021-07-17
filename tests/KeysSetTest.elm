module KeysSetTest exposing (suite)

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


type alias CharWithCode =
    { char : Char, code : Int }


at0 : CharWithCode
at0 =
    { code = 0, char = 'A' }


at1 : CharWithCode
at1 =
    { code = 1, char = 'B' }


with2 : KeysSet CharWithCode
with2 =
    KeysSet.promising
        [ unique .code, unique .char ]
        |> KeysSet.insertAll [ at0, at1 ]


type alias BracketMatch =
    { open : Char, closed : Char }


brackets : KeysSet BracketMatch
brackets =
    KeysSet.promising
        [ unique .open, unique .closed ]
        |> KeysSet.insertAll
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            ]


scanTest : Test
scanTest =
    describe "scan"
        [ describe "size"
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
                        |> KeysSet.insertAll
                            [ { lowercase = 'a', uppercase = 'A' }
                            , { lowercase = 'b', uppercase = 'B' }
                            ]

                lowercase char =
                    casedLetters
                        |> KeysSet.at .uppercase char
                        |> Maybe.map .lowercase

                uppercase char =
                    casedLetters
                        |> KeysSet.at .lowercase char
                        |> Maybe.map .uppercase

                ratedOperators =
                    KeysSet.promising
                        [ unique .symbol, unique .name ]
                        |> KeysSet.insertAll
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
                    KeysSet.at .rating 0.5 ratedOperators
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
                            |> KeysSet.insertAll
                                [ { letter = 'a', code = 97 }
                                , { letter = 'b', code = 98 }
                                ]

                    fancyCompetingLetterCodes =
                        KeysSet.promising
                            [ unique .code, unique .letter ]
                            |> KeysSet.insertAll
                                [ { code = 98, letter = 'b' }
                                , { code = 97, letter = 'a' }
                                ]
                in
                KeysSet.equal letterCodes
                    fancyCompetingLetterCodes
                    |> Expect.true "from reversed list equal to before"
            )
        , describe "toList examples work"
            [ test "isEmpty"
                (\() ->
                    let
                        isEmpty =
                            List.isEmpty << KeysSet.toList
                    in
                    Expect.true "isEmpty for filled False, ifEmpty True"
                        (isEmpty
                            (KeysSet.promising
                                [ unique .code, unique .char ]
                            )
                            && not (isEmpty with2)
                        )
                )
            , test "most recently inserted"
                (\() ->
                    let
                        mostRecentlyInserted =
                            List.head << KeysSet.toList
                    in
                    mostRecentlyInserted
                        (KeysSet.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> KeysSet.insertAll
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
                    |> KeysSet.insertAll
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
                    |> KeysSet.insertAll
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
                    |> KeysSet.insertAll
                        [ { lowercase = 'a', uppercase = 'A' }
                        , { lowercase = 'b', uppercase = 'B' }
                        ]
          in
          describe "isUnique"
            [ test "not unique"
                (\() ->
                    Expect.all
                        [ KeysSet.isUnique
                            { lowercase = 'b', uppercase = 'C' }
                            -- the .lowercase already exists
                            >> Expect.equal False
                        , KeysSet.isUnique
                            { lowercase = 'c', uppercase = 'A' }
                            -- the .uppercase already exists
                            >> Expect.equal False
                        ]
                        letters
                )
            , test "unique"
                (\() ->
                    letters
                        |> KeysSet.isUnique
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
                KeysSet.size
                    (with2
                        |> KeysSet.insert at0
                        |> KeysSet.insert at1
                    )
                    |> Expect.equal 2
            )
        , test "access code is Just letter of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert at1
                    |> KeysSet.at .code at1.code
                    |> Maybe.map .char
                    |> Expect.equal (Just at1.char)
            )
        , test "code is Nothing if not of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert at1
                    |> KeysSet.at .code at0.code
                    |> Maybe.map .char
                    |> Expect.equal Nothing
            )
        , test "char is Just left of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert at1
                    |> KeysSet.at .char at1.char
                    |> Maybe.map .code
                    |> Expect.equal (Just at1.code)
            )
        , test "char is Nothing if not of inserted pair"
            (\() ->
                KeysSet.promising [ unique .code, unique .char ]
                    |> KeysSet.insert at1
                    |> KeysSet.at .char at0.char
                    |> Maybe.map .code
                    |> Expect.equal Nothing
            )
        , test "insert example"
            (\() ->
                let
                    result =
                        KeysSet.promising
                            [ unique .lowercase, unique .uppercase ]
                            |> KeysSet.insertAll
                                [ { lowercase = 'b', uppercase = 'B', rating = 0.5 }
                                , { lowercase = 'a', uppercase = 'A', rating = 0.5 }
                                , { lowercase = 'b', uppercase = 'C', rating = 0 }
                                , { lowercase = 'c', uppercase = 'A', rating = 0 }
                                , { lowercase = 'c', uppercase = 'C', rating = 0.6 }
                                ]
                in
                KeysSet.equal result
                    (KeysSet.promising
                        [ unique .lowercase, unique .uppercase ]
                        |> KeysSet.insertAll
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
            KeysSet.promising
                [ unique .open, unique .closed ]
                |> KeysSet.insert
                    { open = "(", closed = ")" }
    in
    describe "take elements out"
        [ describe "remove"
            [ test "insert |> remove code leaves it unchanged"
                (\() ->
                    with2
                        |> KeysSet.insert { code = 2, char = 'C' }
                        |> KeysSet.remove .code 2
                        |> Expect.equal with2
                )
            , test "insert |> remove char leaves it unchanged"
                (\() ->
                    with2
                        |> KeysSet.insert { code = 2, char = 'C' }
                        |> KeysSet.remove .char 'C'
                        |> Expect.equal with2
                )
            , test "nothing to remove"
                (\() ->
                    openClosedBrackets
                        |> KeysSet.remove .open ")"
                        --> no change, .open is never ")"
                        |> Expect.equal openClosedBrackets
                )
            , test "something to remove"
                (\() ->
                    openClosedBrackets
                        |> KeysSet.remove .closed ")"
                        |> KeysSet.toList
                        |> Expect.equalLists []
                )
            , test "non-unique aspect"
                (\() ->
                    KeysSet.promising
                        [ unique .open, unique .closed ]
                        |> KeysSet.insertAll
                            [ { open = "[", closed = "]", meaning = List }
                            , { open = "<", closed = ">", meaning = Custom }
                            , { open = "\\", closed = "/", meaning = Custom }
                            ]
                        |> KeysSet.remove .meaning Custom
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
                    |> KeysSet.insertAll
                        [ { symbol = ">", name = "gt" }
                        , { symbol = "<", name = "lt" }
                        , { symbol = "==", name = "eq" }
                        ]
          in
          test "when"
            (\() ->
                operators
                    |> KeysSet.when
                        (.symbol >> String.length >> (==) 1)
                    |> KeysSet.equal
                        (KeysSet.promising
                            [ unique .symbol, unique .name ]
                            |> KeysSet.insertAll
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
        [ test "fold example"
            (\() ->
                let
                    openingAndClosing =
                        brackets
                            |> KeysSet.fold
                                (\{ open, closed } ->
                                    (::) (String.fromList [ open, closed ])
                                )
                                []
                in
                Expect.equalLists
                    openingAndClosing
                    [ "()", "{}" ]
            )
        , test "toList example"
            (\() ->
                KeysSet.promising
                    [ unique .open, unique .closed ]
                    |> KeysSet.insertAll
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
                    typeChar char =
                        brackets |> KeysSet.at .open char |> Maybe.map (\{ closed } -> String.fromList [ char, closed ]) |> Maybe.withDefault (brackets |> KeysSet.at .closed char |> Maybe.map (\{ open } -> String.fromList [ open, char ]) |> Maybe.withDefault (String.fromChar char))
                in
                Expect.equal ([ '(', '}' ] |> List.map typeChar)
                    [ "()", "{}" ]
            )
        , test "cased letters"
            (\() ->
                let
                    lowerUppercaseLetters =
                        KeysSet.promising [ unique .lowercase, unique .uppercase ]
                            |> KeysSet.insertAll
                                [ { lowercase = 'a', uppercase = 'A' }
                                , { lowercase = 'b', uppercase = 'B' }
                                , { lowercase = 'c', uppercase = 'C' }
                                ]

                    upperCase char =
                        lowerUppercaseLetters |> KeysSet.at .lowercase char |> Maybe.map .uppercase
                in
                Expect.equal ([ 'c', 'a', 'x' ] |> List.map upperCase)
                    [ Just 'C', Just 'A', Nothing ]
            )
        , test "periodic table"
            (\() ->
                let
                    elements =
                        KeysSet.promising [ unique .atomicNumber, unique .symbol ]
                            |> KeysSet.insertAll
                                [ { symbol = "H", name = "Hydrogen", atomicNumber = 1 }
                                , { symbol = "He", name = "Helium", atomicNumber = 2 }
                                ]

                    atomicNumberOfElementWithName : String -> Maybe Int
                    atomicNumberOfElementWithName name =
                        elements |> KeysSet.at .name name |> Maybe.map .atomicNumber
                in
                [ atomicNumberOfElementWithName "Helium"
                , atomicNumberOfElementWithName "Hydrogen"
                ]
                    |> Expect.equal [ Just 2, Just 1 ]
            )
        ]
