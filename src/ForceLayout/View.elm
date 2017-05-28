module ForceLayout.View exposing (view)

import ForceLayout.Types exposing (..)
import Html exposing (div, text, Html, hr, button, input, label, fieldset, legend)
import Html.Attributes exposing (type_, value, min, max, step, href)
import Html.Attributes as HA
import Html.Events exposing (onInput, onClick, onMouseUp)
import TypedSvg.Core exposing (Svg)
import TypedSvg as Svg
import TypedSvg.Attributes exposing (viewBox, stroke)
import TypedSvg.Attributes.InPx as SA
import Graph as G
import Color
import Draggable


view : Model -> Html Msg
view { graph, layoutSettings, example } =
    div []
        [ description
        , controls layoutSettings example
        , viewGraph graph
        ]


controls : LayoutSettings -> PredefinedExample -> Html Msg
controls layoutSettings example =
    div [ HA.style [ ( "float", "left" ) ] ]
        [ layoutSettingsControls layoutSettings
        , exampleControls example
        , randomizeButton
        ]


randomizeButton : Html Msg
randomizeButton =
    div [ HA.style [ ( "padding", "10px" ) ] ] [ button [ onClick Randomize ] [ text "Randomize" ] ]


layoutSettingsControls : LayoutSettings -> Html Msg
layoutSettingsControls settings =
    div []
        [ rangeSlider "Charge" "determines repulsive force between nodes. The higher the charge,the more spaced-out the graph will be." SetCharge 1000 50000 100 settings.charge
        , rangeSlider "Stiffness" "determines attractive force between connected nodes. The higher the stiffness, the more tight the graph will be. You can imagine edges being like rubber bands pulling nodes together." SetStiffness 0.1 1 0.1 settings.stiffness
        , rangeSlider "Speed" "determines how fast the animation runs. Lowering it to 0 freezes the animation." SetTimeDiff 0.0 0.3 0.001 settings.timeDiff
        ]


rangeSlider : String -> String -> (String -> Msg) -> Float -> Float -> Float -> Float -> Html Msg
rangeSlider sliderLabel tooltip tagger minVal maxVal stepVal val =
    div []
        [ label
            [ HA.style [ ( "display", "inline-block" ), ( "width", "60px" ) ] ]
            [ text sliderLabel ]

        --TODO use tooltip
        , input
            [ type_ "range"
            , HA.min <| toString minVal
            , HA.max <| toString maxVal
            , step <| toString stepVal
            , value <| toString val
            , onInput tagger
            ]
            []
        , text <| toString val
        ]


exampleControls : PredefinedExample -> Html Msg
exampleControls example =
    fieldset [ HA.style [ ( "display", "inline-block" ) ] ]
        [ legend [] [ text "Examples" ]
        , radio example "tetrahedron" Tetrahedron
        , radio example "cube" Cube
        , radio example "circle" (Circle 5)
        , radio example "hypercube" Hypercube
        ]


radio : PredefinedExample -> String -> PredefinedExample -> Html Msg
radio currExample value exampleSelectableByRadio =
    div []
        [ input
            [ type_ "radio"
            , HA.name "font-size"
            , onClick <| SelectExample exampleSelectableByRadio
            , HA.checked (currExample == exampleSelectableByRadio)
            ]
            []
        , text value
        ]


viewGraph : LayoutGraph -> Html Msg
viewGraph graph =
    Svg.svg [ SA.width canvasWidth, SA.height canvasHeight, viewBox 0 0 canvasWidth canvasHeight ] <|
        viewEdges graph
            ++ viewNodes (G.nodes graph)


viewEdges : LayoutGraph -> List (Svg msg)
viewEdges graph =
    List.map (\edge -> viewEdge graph edge) <| G.edges graph


viewNodes : List PositionedNode -> List (Svg Msg)
viewNodes nodes =
    List.map viewNode nodes


viewEdge : LayoutGraph -> G.Edge () -> Svg msg
viewEdge gr ({ from, to } as edge) =
    let
        (Point2D fromX fromY) =
            getCoords gr from

        (Point2D toX toY) =
            getCoords gr to
    in
        Svg.line [ SA.x1 fromX, SA.y1 fromY, SA.x2 toX, SA.y2 toY, SA.strokeWidth 1, stroke Color.black ] []


viewNode : PositionedNode -> Svg Msg
viewNode { id, label } =
    let
        (Point2D x y) =
            label
    in
        Svg.circle
            [ SA.cx x
            , SA.cy y
            , SA.r 10
            , Draggable.mouseTrigger id DragMsg
            , onMouseUp NodeDragEnd
            ]
            []


description : Html a
description =
    div []
        [ Html.h3 [] [ text "Force directed graph layout in Elm" ]
        , div []
            [ text "You can drag and drop nodes and tweak layout algorithm's parameters using the sliders below. Source code is available "
            , Html.a [ href "https://github.com/jhrcek/force-layout" ] [ text "here" ]
            ]
        ]
