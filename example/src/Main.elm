module Main exposing (main)

import Browser
import Element as Ui
import Element.Border as UiBorder
import Element.Font as UiFont
import Element.Input as UiInput
import Html exposing (Html, text)
import Html.Attributes
import Html.Events exposing (onInput)
import KeysSet exposing (KeysSet, unique)


type alias Model =
    { textInLetterInfo : String
    , letterInfo : Maybe LetterInfo
    , textInOpenCloseBrackets : String
    }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }


initialModel : Model
initialModel =
    { textInLetterInfo = "a"
    , letterInfo = Just aLetterInfo
    , textInOpenCloseBrackets = "just type "
    }


type Msg
    = InputInCharacterInfo String
    | TypeInOpenCloseBrackets String


update : Msg -> Model -> Model
update msg model =
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
                                        |> Maybe.map Just
                                        |> Maybe.withDefault (casedLetterByUppercase last)
                            }

                        [] ->
                            model

        TypeInOpenCloseBrackets text ->
            { model
                | textInOpenCloseBrackets =
                    case text |> String.toList |> List.reverse of
                        last :: rest ->
                            let
                                before =
                                    List.reverse rest |> String.fromList
                            in
                            if
                                String.length text
                                    > String.length (.textInOpenCloseBrackets model)
                            then
                                case KeysSet.at .open last brackets of
                                    Just { closed } ->
                                        text ++ String.fromChar closed

                                    Nothing ->
                                        case KeysSet.at .closed last brackets of
                                            Just { open } ->
                                                before ++ String.fromList [ open, last ]

                                            Nothing ->
                                                text

                            else
                                case KeysSet.at .open last brackets of
                                    Just _ ->
                                        before

                                    Nothing ->
                                        case KeysSet.at .closed last brackets of
                                            Just _ ->
                                                before

                                            Nothing ->
                                                text

                        [] ->
                            text
            }


view : Model -> Html Msg
view { textInOpenCloseBrackets, letterInfo, textInLetterInfo } =
    Ui.column [ Ui.paddingXY 16 6, Ui.spacing 20 ]
        [ Ui.column [ Ui.padding 12 ]
            [ Ui.text "KeysSet"
                |> Ui.el
                    [ UiFont.family [ UiFont.typeface "Fira Code" ]
                    , UiFont.size 32
                    ]
            , Ui.text "some examples"
            ]
        , Ui.column [ Ui.moveRight 20, Ui.spacing 34 ]
            [ viewCharacterInfo letterInfo textInLetterInfo
            , viewOpenCloseBrackets textInOpenCloseBrackets
            ]
        ]
        |> Ui.layout []


viewCharacterInfo : Maybe LetterInfo -> String -> Ui.Element Msg
viewCharacterInfo letterInfo content =
    Ui.column [ Ui.spacing 10 ]
        [ Ui.text "Information about your letter."
            |> Ui.el [ UiFont.size 22 ]
        , Ui.column [ Ui.spacing 6 ]
            [ Ui.text "type the number in the alphabet or the lowercase / uppercase letter."
            , viewTextInput { onInput = InputInCharacterInfo, value = content }
            , case letterInfo of
                Just { inAlphabet, lowercase, uppercase } ->
                    Ui.column [ UiFont.family [ UiFont.typeface "Fira Code" ] ]
                        [ Ui.text ("# " ++ String.fromInt inAlphabet ++ " in the alphabet")
                        , Ui.text ("▼ " ++ String.fromChar lowercase ++ " lowercase")
                        , Ui.text ("▲ " ++ String.fromChar uppercase ++ " uppercase")
                        ]

                Nothing ->
                    Ui.text "try another."
            ]
        ]


type alias LetterInfo =
    { lowercase : Char
    , uppercase : Char
    , inAlphabet : Int
    }


aLetterInfo : LetterInfo
aLetterInfo =
    { inAlphabet = 0, lowercase = 'a', uppercase = 'A' }


letterInfos : KeysSet LetterInfo
letterInfos =
    KeysSet.promising
        [ unique .lowercase
        , unique .uppercase
        , unique .inAlphabet
        ]
        |> KeysSet.insert aLetterInfo
        |> KeysSet.insertAll
            [ { inAlphabet = 1, lowercase = 'b', uppercase = 'B' }
            , { inAlphabet = 2, lowercase = 'c', uppercase = 'C' }
            , { inAlphabet = 5, lowercase = 'f', uppercase = 'F' }
            , { inAlphabet = 10, lowercase = 'k', uppercase = 'K' }
            , { inAlphabet = 25, lowercase = 'z', uppercase = 'Z' }
            ]


casedLetterByLowercase : Char -> Maybe LetterInfo
casedLetterByLowercase char =
    KeysSet.at .lowercase char letterInfos


casedLetterByUppercase : Char -> Maybe LetterInfo
casedLetterByUppercase char =
    KeysSet.at .uppercase char letterInfos


casedLetterInAlphabet : Int -> Maybe LetterInfo
casedLetterInAlphabet inAlphabet =
    KeysSet.at .inAlphabet inAlphabet letterInfos


viewOpenCloseBrackets : String -> Ui.Element Msg
viewOpenCloseBrackets labelText =
    Ui.column [ Ui.spacing 10 ]
        [ Ui.text "Auto-open and -close brackets"
            |> Ui.el [ UiFont.size 22 ]
        , Ui.column
            [ Ui.spacing 6
            , UiFont.family [ UiFont.typeface "Fira Code" ]
            ]
            [ Ui.text "[] {} (): type an open or closed bracket (but don't move the cursor)"
            , viewTextInput
                { value = labelText
                , onInput = TypeInOpenCloseBrackets
                }
            ]
        ]


type alias OpenClosedBracket =
    { open : Char
    , closed : Char
    }


brackets : KeysSet OpenClosedBracket
brackets =
    KeysSet.promising [ unique .open, unique .closed ]
        |> KeysSet.insertAll
            [ { open = '(', closed = ')' }
            , { open = '{', closed = '}' }
            , { open = '[', closed = ']' }
            ]


viewTextInput :
    { rec | value : String, onInput : String -> msg }
    -> Ui.Element msg
viewTextInput { value, onInput } =
    UiInput.text
        [ UiBorder.solid
        , UiBorder.color (Ui.rgb 0 1 1)
        , UiBorder.width 1
        , UiBorder.widthEach { edges | bottom = 5 }
        ]
        { onChange = onInput
        , text = value
        , placeholder = Nothing
        , label = UiInput.labelHidden "text input"
        }


edges =
    { right = 0, top = 0, left = 0, bottom = 0 }
