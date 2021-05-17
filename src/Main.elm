-- https://pianomanfrazier.com/post/elm-calculator-book/05-decimals/
module Main exposing (main, view)

import Browser
import Html exposing (Html, button, div, h1, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

type alias Model = 
    {
        stack: List Float,
        currentNum: String,
        error: Maybe String
    }

initialModel : Model
initialModel =
    {
        stack = [],
        currentNum = "0",
        error = Nothing
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
    | SetDecimal
    | Enter
    | SetSign
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
        Back -> 
            let
                newNum = String.dropRight 1 model.currentNum
            in
                { 
                    model |
                        currentNum =
                            if String.isEmpty newNum then
                                "0"
                            else
                                newNum
                }

        Clear -> { model | currentNum = "0" }
        ClearAll -> { model | currentNum = "0", stack = [] }
        SetDecimal ->
            if String.contains "." model.currentNum then
                model

            else
                { model | currentNum = model.currentNum ++ "." }
        Enter -> 
            let
                maybeNumber = String.toFloat model.currentNum 
            in
            case maybeNumber of
                Nothing->
                    { model | error = Just "PARSE ERR" }
                Just num ->
                    { model | stack = num :: model.stack, currentNum = "0" }
        SetSign ->
            -- Make sure that zero can't be negated
            if model.currentNum == "0" then
                model

            -- Switch the sign: remove the negative sign if it exists
            else if String.startsWith "-" model.currentNum then
                { model | currentNum = String.dropLeft 1 model.currentNum}

            -- or add if it doesn't
            else 
                { model | currentNum = "-" ++ model.currentNum }
        InputNumber num ->
            if model.currentNum == "0" then
                { model | currentNum = String.fromFloat num }

            else
                { model | currentNum = model.currentNum ++ String.fromFloat num }
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
                        maybeNumber =
                            String.toFloat model.currentNum
                    in
                    case maybeNumber of
                        Nothing ->
                            { model | error = Just "PARSE ERR" }

                        Just num ->
                            let
                                newNum = operation num x
                            in
                                -- update model
                                { 
                                    model |
                                        stack = xs,
                                        currentNum = String.fromFloat newNum
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

stackBox : Float -> Html Msg
stackBox num =
    div
        [ class "input-box" ]
        [ text <| String.fromFloat num ]


inputBox : Html Msg -> Html Msg
inputBox num =
    div
        [ class "input-box" ]
        [ num ]

section : Html Msg
section =
    div
        [ class "section" ]
        [
            cell (onClick <|Back) Single Gray "←",
            cell (onClick <| Clear) Single Gray "C",
            cell (onClick <| Clear) Single Gray "CE",
            cell (onClick <| InputOperator Divide) Single Yellow "÷",
            cell (onClick <| InputNumber 7) Single White "7",
            cell (onClick <| InputNumber 8) Single White "8",
            cell (onClick <| InputNumber 9) Single White "9",
            cell (onClick <| InputOperator Multiply) Single Yellow "×",
            cell (onClick <| InputNumber 4) Single White "4",
            cell (onClick <| InputNumber 5) Single White "5",
            cell (onClick <| InputNumber 6) Single White "6",
            cell (onClick <| InputOperator Subtract) Single Yellow "-",
            cell (onClick <| InputNumber 1) Single White "1",
            cell (onClick <| InputNumber 2) Single White "2",
            cell (onClick <| InputNumber 3) Single White "3",
            cell (onClick <|InputOperator Add) Single Yellow "+",
            cell (onClick <| InputNumber 0) Single White "0",
            cell (onClick <| SetDecimal) Single White ".",
            cell (onClick <| SetSign) Single Yellow "+/-",
            cell (onClick <| Enter) Single Yellow "Enter"
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
                        stackBox
                        (List.reverse model.stack)
                    ++ 
                    [
                        inputBox <|
                        case model.error of
                            Nothing -> inputBox (text model.currentNum)
                            Just err -> inputBox (span [ class "error" ] [ text err ]),
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