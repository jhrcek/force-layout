module ForceLayout.Types
    exposing
        ( Model
        , Msg(..)
        , LayoutGraph
        , Point2D(Point2D)
        , PositionedNode
        , LayoutSettings
        , canvasWidth
        , canvasHeight
        , getCoords
        , ctxToPoint
        , defaultLayoutSettings
        )

import Graph exposing (Edge, Graph, Node, NodeId, NodeContext)
import Graph as G


type alias Model =
    { graph : LayoutGraph
    , layoutSettings : LayoutSettings
    }


type Msg
    = InitRandomPositions (List Point2D)
    | AnimationTick
    | SetCharge String
    | SetStiffness String
    | SetTimeDiff String


type alias LayoutGraph =
    Graph Point2D ()


type Point2D
    = Point2D Float Float


type alias PositionedNode =
    Node Point2D


type alias LayoutSettings =
    { charge : Float
    , stiffness : Float
    , timeDiff : Float
    }


defaultLayoutSettings : LayoutSettings
defaultLayoutSettings =
    { charge = 10000, stiffness = 0.5, timeDiff = 0.01 }


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
