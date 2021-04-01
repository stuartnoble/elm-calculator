-- https://pianomanfrazier.com/post/elm-calculator-book/04-basic-operations/
module Main exposing (main, view)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type alias Model = 
    {
        stack: List Float,
        currentNum : Float
    }

initialModel : Model
initialModel =
    {
        stack = [],
        currentNum = 0
    }

type Operator
    = Add
    | Subtract
    | Multiply
    | Divide

type Msg
    = Back
    | Clear
    | ClearAll
    | Enter
    | InputNumber Float
    | InputOperator Operator

type Size
    = Single
    | Double
    | Triple

type Color
    = Yellow
    | Gray
    | White

operatorFunction : Operator -> (Float -> Float -> Float)
operatorFunction operator =
    case operator of
        Add -> (+)
        Subtract -> (-)
        Multiply -> (*)
        Divide -> (/)

update : Msg -> Model -> Model
update msg model =
    case msg of
        Back -> { model | currentNum = model.currentNum / 10 |> floor |> toFloat }
        Clear -> { model | currentNum = 0 }
        ClearAll -> { model | currentNum = 0, stack = [] }
        Enter -> 
            { 
                model |
                    stack = model.currentNum :: model.stack,
                    currentNum = 0
            }
        InputNumber num -> { model | currentNum = (model.currentNum * 10) + num }
        InputOperator operator ->
            case model.stack of
                -- stack is empty
                [] ->
                    -- return itself
                    model  
                -- pop the list
                x :: xs ->
                    let 
                        -- get function to use
                        operation = operatorFunction operator
                        -- calculate
                        newNum = operation model.currentNum x
                    in
                        -- update model
                        {
                            model |
                                stack = xs,
                                currentNum = newNum
                        }

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

cell : Html.Attribute Msg -> Size -> Color -> String -> Html Msg
cell attr size color content =
    button
            [ 
                class <|
                String.join " " <|
                [ 
                    "cell",
                    sizeToString size,
                    colorToString color
                ],
                attr
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
            cell (onClick(Back)) Single Gray "←",
            cell (onClick (Clear)) Single Gray "C",
            cell (onClick (Clear)) Single Gray "CE",
            cell (onClick (InputOperator Divide)) Single Yellow "÷",
            cell (onClick (InputNumber 7)) Single White "7",
            cell (onClick (InputNumber 8)) Single White "8",
            cell (onClick (InputNumber 9)) Single White "9",
            cell (onClick (InputOperator Multiply)) Single Yellow "×",
            cell (onClick (InputNumber 4)) Single White "4",
            cell (onClick (InputNumber 5)) Single White "5",
            cell (onClick (InputNumber 6)) Single White "6",
            cell (onClick (InputOperator Subtract)) Single Yellow "-",
            cell (onClick (InputNumber 1)) Single White "1",
            cell (onClick (InputNumber 2)) Single White "2",
            cell (onClick (InputNumber 3)) Single White "3",
            cell (onClick(InputOperator Add)) Single Yellow "+",
            cell (onClick (InputNumber 0)) Single White "0",
            --cell Single White ".",
            cell (onClick (Enter)) Double Yellow "Enter"
        ]

view : Model -> Html Msg
view model =
    div
        []
        [ 
            h1 [class "h1"] [ text "RPN Calculator" ],
            div
                [ class "calculator" ]
                (
                    List.map
                        inputBox
                        (List.reverse model.stack)
                    ++ 
                    [
                        inputBox model.currentNum,
                        section
                    ]
                )
        ]

main : Program () Model Msg
main =
    Browser.sandbox
    {
        init = initialModel,
        view = view,
        update = update
    }