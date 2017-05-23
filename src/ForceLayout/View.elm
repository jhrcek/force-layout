module ForceLayout.View exposing (view)

import ForceLayout.Types exposing (..)
import Html exposing (div, text, Html, hr, button)
import TypedSvg.Core exposing (Svg)
import TypedSvg as Svg
import TypedSvg.Attributes exposing (viewBox, stroke)
import TypedSvg.Attributes.InPx as A
import Graph as G
import Color


view : Model -> Html Msg
view graph =
    div []
        [ viewGraph graph
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
