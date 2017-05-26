module ForceLayout.View exposing (view)

import ForceLayout.Types exposing (..)
import Html exposing (div, text, Html, hr, button, input, label)
import Html.Attributes exposing (type_, value, min, max, step)
import Html.Attributes as HA
import Html.Events exposing (onInput)
import TypedSvg.Core exposing (Svg)
import TypedSvg as Svg
import TypedSvg.Attributes exposing (viewBox, stroke)
import TypedSvg.Attributes.InPx as A
import Graph as G
import Color


view : Model -> Html Msg
view { graph, layoutSettings } =
    div []
        [ layoutSettingsControls layoutSettings
        , viewGraph graph
        ]


layoutSettingsControls : LayoutSettings -> Html Msg
layoutSettingsControls settings =
    div [ HA.style [ ( "padding", "5px" ) ] ]
        [ rangeSlider "charge" SetCharge 1000 100000 10 settings.charge
        , rangeSlider "stiffness" SetStiffness 0.1 1 0.1 settings.stiffness
        , rangeSlider "timeDiff" SetTimeDiff 0.01 0.5 0.01 settings.timeDiff
        ]


rangeSlider : String -> (String -> Msg) -> Float -> Float -> Float -> Float -> Html Msg
rangeSlider lbl tagger minVal maxVal stepVal val =
    div []
        [ label []
            [ text lbl
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
        ]


viewGraph : LayoutGraph -> Html msg
viewGraph graph =
    Svg.svg [ A.width canvasWidth, A.height canvasHeight, viewBox 0 0 canvasWidth canvasHeight ] <|
        viewEdges graph
            ++ viewNodes (G.nodes graph)


viewEdges : LayoutGraph -> List (Svg msg)
viewEdges graph =
    List.map (\edge -> viewEdge graph edge) <| G.edges graph


viewNodes : List PositionedNode -> List (Svg msg)
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
        Svg.line [ A.x1 fromX, A.y1 fromY, A.x2 toX, A.y2 toY, A.strokeWidth 1, stroke Color.black ] []


viewNode : PositionedNode -> Svg msg
viewNode { label } =
    let
        (Point2D x y) =
            label
    in
        Svg.circle [ A.cx x, A.cy y, A.r 10 ] []
