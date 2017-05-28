module ForceLayout.Types
    exposing
        ( Model
        , Msg(..)
        , LayoutGraph
        , Point2D(Point2D)
        , PositionedNode
        , LayoutSettings
        , PredefinedExample(..)
        , canvasWidth
        , canvasHeight
        , getCoords
        , ctxToPoint
        , mapNodeLabelInCtx
        , mapNodeInContext
        , defaultLayoutSettings
        )

import Graph exposing (Edge, Graph, Node, NodeId, NodeContext)
import Graph as G
import Draggable


type alias Model =
    { graph : LayoutGraph
    , draggedNode : Maybe NodeId
    , drag : Draggable.State NodeId
    , layoutSettings : LayoutSettings
    , example : PredefinedExample
    }


type Msg
    = InitRandomPositions (List Point2D)
    | AnimationTick
      -- Settings changes
    | SetCharge String
    | SetStiffness String
    | SetTimeDiff String
    | SelectExample PredefinedExample
    | Randomize
      -- Drag and drop
    | NodeDraggedBy Draggable.Delta
    | NodeDragStart NodeId
    | NodeDragEnd
    | DragMsg (Draggable.Msg NodeId)


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


type PredefinedExample
    = Tetrahedron
    | Cube
    | Circle Int
    | Hypercube


defaultLayoutSettings : LayoutSettings
defaultLayoutSettings =
    { charge = 10000, stiffness = 0.5, timeDiff = 0.02 }


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
    960



-- Stuff missing in Graph library


mapNodeLabelInCtx : (a -> b) -> NodeContext a e -> NodeContext b e
mapNodeLabelInCtx f ctx =
    { ctx | node = mapLabelInNode f ctx.node }


mapLabelInNode : (a -> b) -> Node a -> Node b
mapLabelInNode f node =
    { node | label = f node.label }


mapNodeInContext : (Node a -> Node b) -> NodeContext a e -> NodeContext b e
mapNodeInContext f ctx =
    { ctx | node = f ctx.node }
