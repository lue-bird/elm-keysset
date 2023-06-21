module Main exposing (main)

import Bracket exposing (Bracket)
import Browser
import Element as Ui
import Element.Background as UiBackground
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Emptiable exposing (Emptiable, filled)
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onInput)
import Keys exposing (key)
import KeysSet exposing (KeysSet)
import LetterInfo exposing (LetterInfo)
import N exposing (N2, N3)
import Possibly exposing (Possibly)


type alias State =
    { textInLetterInfo : String
    , letterInfo : Emptiable LetterInfo Possibly
    , textInOpenCloseBrackets : String
    }


main : Program () State Event
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = reactTo
        }


initialModel : State
initialModel =
    { textInLetterInfo = "a"
    , letterInfo = aLetterInfo |> filled
    , textInOpenCloseBrackets = "just type "
    }


type Event
    = InputInCharacterInfo String
    | TypeInOpenCloseBrackets String


reactTo : Event -> (State -> State)
reactTo msg model =
    case msg of
        InputInCharacterInfo text ->
            case String.toInt text of
                Just int ->
                    { model
                        | textInLetterInfo = text
                        , letterInfo = casedLetterInAlphabet int
                    }

                Nothing ->
                    case text |> String.toList |> List.reverse of
                        last :: _ ->
                            { model
                                | textInLetterInfo = last |> String.fromChar
                                , letterInfo =
                                    casedLetterByLowercase last
                                        |> Emptiable.map Emptiable.filled
                                        |> Emptiable.fillElseOnEmpty (\_ -> casedLetterByUppercase last)
                            }

                        [] ->
                            { model | textInLetterInfo = "" }

        TypeInOpenCloseBrackets text ->
            let
                textMatchLength : Int
                textMatchLength =
                    matchLength ( text, model.textInOpenCloseBrackets )
            in
            { model
                | textInOpenCloseBrackets =
                    if
                        (text |> String.length)
                            > (model.textInOpenCloseBrackets |> String.length)
                    then
                        case text |> String.dropLeft textMatchLength |> String.uncons of
                            Nothing ->
                                text

                            Just ( new, afterNew ) ->
                                let
                                    beforeNew =
                                        text |> String.left textMatchLength
                                in
                                case brackets |> KeysSet.element (key .open Bracket.keys) new of
                                    Emptiable.Filled { closed } ->
                                        beforeNew ++ String.fromList [ new, closed ] ++ afterNew

                                    Emptiable.Empty _ ->
                                        case brackets |> KeysSet.element (key .closed Bracket.keys) new of
                                            Emptiable.Filled { open } ->
                                                beforeNew ++ String.fromList [ open, new ] ++ afterNew

                                            Emptiable.Empty _ ->
                                                text

                    else
                        case model.textInOpenCloseBrackets |> String.dropLeft textMatchLength |> String.uncons of
                            Nothing ->
                                text

                            Just ( removed, afterRemoved ) ->
                                let
                                    beforeRemoved : String
                                    beforeRemoved =
                                        model.textInOpenCloseBrackets |> String.left textMatchLength
                                in
                                case brackets |> KeysSet.element (key .open Bracket.keys) removed of
                                    Emptiable.Filled bracket ->
                                        beforeRemoved
                                            ++ (afterRemoved |> removeFirstIndex (bracket.closed |> String.fromChar))

                                    Emptiable.Empty _ ->
                                        case brackets |> KeysSet.element (key .closed Bracket.keys) removed of
                                            Emptiable.Filled bracket ->
                                                (beforeRemoved |> removeLastIndex (bracket.open |> String.fromChar))
                                                    ++ afterRemoved

                                            Emptiable.Empty _ ->
                                                text
            }


removeFirstIndex : String -> String -> String
removeFirstIndex match =
    \string ->
        case string |> String.indexes match of
            [] ->
                string

            matchingIndex :: _ ->
                string |> removeIndex matchingIndex


removeIndex : Int -> String -> String
removeIndex index =
    \string ->
        (string |> String.left index)
            ++ (string |> String.dropLeft (index + 1))


removeLastIndex : String -> String -> String
removeLastIndex match =
    \string ->
        case string |> lastIndex match of
            Nothing ->
                string

            Just matchingIndex ->
                string |> removeIndex matchingIndex


lastIndex : String -> String -> Maybe Int
lastIndex match =
    \string ->
        string |> String.indexes match |> List.reverse |> List.head


matchLength : ( String, String ) -> Int
matchLength ( a, b ) =
    case ( a |> String.uncons, b |> String.uncons ) of
        ( _, Nothing ) ->
            0

        ( Nothing, _ ) ->
            0

        ( Just ( aHead, aTail ), Just ( bHead, bTail ) ) ->
            if aHead /= bHead then
                0

            else
                -- aHead matches bHead
                1 + matchLength ( aTail, bTail )


view : State -> Html Event
view { textInOpenCloseBrackets, letterInfo, textInLetterInfo } =
    [ [ Ui.text "KeysSet"
            |> Ui.el
                [ UiFont.family [ UiFont.monospace ]
                , UiFont.size 38
                ]
      , Ui.text "some examples"
      ]
        |> Ui.column
            [ Ui.padding 12
            , Ui.padding 27
            ]
    , [ viewCharacterInfo letterInfo textInLetterInfo
      , viewOpenCloseBrackets textInOpenCloseBrackets
      ]
        |> Ui.column [ Ui.moveRight 20, Ui.spacing 34 ]
    ]
        |> Ui.column
            [ Ui.paddingXY 56 20
            , Ui.spacing 20
            ]
        |> Ui.layoutWith
            { options =
                [ Ui.focusStyle
                    { borderColor = Just (Ui.rgb 0 1 0)
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            [ UiBackground.color (Ui.rgb 0 0 0)
            , UiFont.color (Ui.rgb 1 1 0.5)
            ]


viewCharacterInfo : Emptiable LetterInfo Possibly -> String -> Ui.Element Event
viewCharacterInfo letterInfo content =
    Ui.column [ Ui.spacing 10 ]
        [ Ui.text "Information about your letter"
            |> Ui.el [ UiFont.size 24 ]
        , Ui.column [ Ui.spacing 6, Ui.paddingXY 20 4 ]
            [ Ui.text "type the number in the alphabet or the lowercase / uppercase letter."
            , viewTextInput { onInput = InputInCharacterInfo, value = content }
            , case letterInfo of
                Emptiable.Filled { inAlphabet, lowercase, uppercase } ->
                    Ui.column [ UiFont.family [ UiFont.monospace ], Ui.paddingXY 20 4 ]
                        [ Ui.text ("# " ++ String.fromInt inAlphabet ++ " in the alphabet")
                        , Ui.text ("▼ " ++ String.fromChar lowercase ++ " lowercase")
                        , Ui.text ("▲ " ++ String.fromChar uppercase ++ " uppercase")
                        ]

                Emptiable.Empty _ ->
                    Ui.text "try a different one."
            ]
        ]


aLetterInfo : LetterInfo
aLetterInfo =
    { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }


letterInfos : Emptiable (KeysSet LetterInfo LetterInfo.Keys N3) Possibly
letterInfos =
    KeysSet.fromList LetterInfo.keys
        [ aLetterInfo
        , { inAlphabet = 1, lowercase = 'b', uppercase = 'B' }
        , { inAlphabet = 2, lowercase = 'c', uppercase = 'C' }
        , { inAlphabet = 3, lowercase = 'd', uppercase = 'D' }
        , { inAlphabet = 4, lowercase = 'e', uppercase = 'E' }
        , { inAlphabet = 5, lowercase = 'f', uppercase = 'F' }
        , { inAlphabet = 6, lowercase = 'g', uppercase = 'G' }
        , { inAlphabet = 7, lowercase = 'h', uppercase = 'H' }
        , { inAlphabet = 8, lowercase = 'i', uppercase = 'I' }
        , { inAlphabet = 9, lowercase = 'j', uppercase = 'J' }
        , { inAlphabet = 10, lowercase = 'k', uppercase = 'K' }
        , { inAlphabet = 23, lowercase = 'x', uppercase = 'X' }
        , { inAlphabet = 24, lowercase = 'y', uppercase = 'Y' }
        , { inAlphabet = 25, lowercase = 'z', uppercase = 'Z' }
        ]


casedLetterByLowercase : Char -> Emptiable LetterInfo Possibly
casedLetterByLowercase char =
    letterInfos |> KeysSet.element (key .lowercase LetterInfo.keys) char


casedLetterByUppercase : Char -> Emptiable LetterInfo Possibly
casedLetterByUppercase char =
    letterInfos |> KeysSet.element (key .uppercase LetterInfo.keys) char


casedLetterInAlphabet : Int -> Emptiable LetterInfo Possibly
casedLetterInAlphabet inAlphabet =
    letterInfos |> KeysSet.element (key .inAlphabet LetterInfo.keys) inAlphabet


brackets : Emptiable (KeysSet Bracket Bracket.Keys N2) Possibly
brackets =
    KeysSet.fromList Bracket.keys
        [ { open = '(', closed = ')' }
        , { open = '[', closed = ']' }
        , { open = '{', closed = '}' }
        ]


viewOpenCloseBrackets : String -> Ui.Element Event
viewOpenCloseBrackets labelText =
    Ui.column [ Ui.spacing 10 ]
        [ Ui.text "Auto-open and -close brackets"
            |> Ui.el [ UiFont.size 24 ]
        , Ui.column
            [ Ui.spacing 6
            , Ui.paddingXY 20 4
            ]
            [ Ui.text "type text with some [, ], {, }, (, )"
            , viewTextInput
                { value = labelText
                , onInput = TypeInOpenCloseBrackets
                }
                |> Ui.el [ UiFont.family [ UiFont.monospace ] ]
            ]
        ]


viewTextInput :
    { rec | value : String, onInput : String -> msg }
    -> Ui.Element msg
viewTextInput { value, onInput } =
    UiInput.text
        [ UiBorder.solid
        , UiBorder.color (Ui.rgb 1 1 0)
        , UiBorder.widthEach { edges | bottom = 3 }
        , UiBackground.color (Ui.rgba 0 0 0 0)
        ]
        { onChange = onInput
        , text = value
        , placeholder = Nothing
        , label = UiInput.labelHidden "text input"
        }


edges : { right : number, top : number, left : number, bottom : number }
edges =
    { right = 0, top = 0, left = 0, bottom = 0 }
