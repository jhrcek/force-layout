module ForceLayout.Types
    exposing
        ( Model
        , Msg(..)
        , LayoutGraph
        , Point2D(Point2D)
        , PositionedNode
        , canvasWidth
        , canvasHeight
        , getCoords
        , ctxToPoint
        )

import Graph exposing (Edge, Graph, Node, NodeId, NodeContext)
import Graph as G


type alias Model =
    LayoutGraph


type Msg
    = InitRandomPositions (List Point2D)
    | AnimationTick


type alias LayoutGraph =
    Graph Point2D ()


type Point2D
    = Point2D Float Float


type alias PositionedNode =
    Node Point2D


getCoords : LayoutGraph -> NodeId -> Point2D
getCoords graph nid =
    case G.get nid graph of
        Nothing ->
            Debug.crash <| "Node " ++ toString nid ++ " was not in the graph " ++ toString graph

        Just ctx ->
            ctxToPoint ctx


ctxToPoint : NodeContext Point2D () -> Point2D
ctxToPoint { node } =
    let
        { label } =
            node
    in
        label


canvasWidth : Float
canvasWidth =
    1280


canvasHeight : Float
canvasHeight =
    720
