-- https://pianomanfrazier.com/post/elm-calculator-book/03-styling/
module Main exposing (main, view)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)

type alias Model = 
    {}

initialModel : Model
initialModel =
    {}

type Msg
    = NoOp

type Size
    = Single
    | Double
    | Triple

type Color
    = Yellow
    | Gray
    | White

update : Msg -> Model -> Model
update msg model =
    case msg of
       NoOp ->
        model

sizeToString : Size -> String
sizeToString size =
    case size of
        Single -> "single"
        Double -> "double"
        Triple -> "triple"

colorToString: Color -> String
colorToString color =
    case color of
        Yellow -> "bg-yellow"
        Gray -> "bg-gray"
        White -> "bg-white"

cell : Size -> Color -> String -> Html Msg
cell size color content =
    button
            [ 
                class <|
                String.join " " <|
                [ 
                    "cell",
                    sizeToString size,
                    colorToString color
                ]
            ]
            [ text content ]

inputBox : Float -> Html Msg
inputBox num =
    div
        [ class "input-box" ]
        [ text <| String.fromFloat num ]

section : Html Msg
section =
    div
        [ class "section" ]
        [
            cell Single Gray "←",
            cell Single Gray "C",
            cell Single Gray "CE",
            cell Single Yellow "÷",
            cell Single White "7",
            cell Single White "8",
            cell Single White "9",
            cell Single Yellow "×",
            cell Single White "4",
            cell Single White "5",
            cell Single White "6",
            cell Single Yellow "-",
            cell Single White "1",
            cell Single White "2",
            cell Single White "3",
            cell Single Yellow "+",
            cell Single White "0",
            cell Single White ".",
            cell Double Yellow "Enter"
        ]

view : Model -> Html Msg
view model =
    div
        []
        [ 
            h1 [class "h1"] [ text "RPN Calculator" ],
            div
                [ class "calculator" ]
                [ 
                    inputBox 78.9,
                    section
                ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
    {
        init = initialModel,
        view = view,
        update = update
    }