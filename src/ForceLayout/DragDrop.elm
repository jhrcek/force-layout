module ForceLayout.DragDrop exposing (..)

import Draggable exposing (Delta)
import Draggable.Events exposing (onDragEnd, onDragBy, onDragStart)
import ForceLayout.Types exposing (..)
import Graph as G
import Maybe


dragConfig : Draggable.Config G.NodeId Msg
dragConfig =
    Draggable.customConfig
        [ onDragBy NodeDraggedBy
        , onDragStart NodeDragStart
        , onDragEnd NodeDragEnd
        ]


dragNode : Model -> Delta -> Model
dragNode model delta =
    case model.draggedNode of
        Nothing ->
            model

        Just draggedNodeId ->
            let
                newGraph =
                    G.update draggedNodeId (updatePositionInContext delta) model.graph
            in
                { model | graph = newGraph }


updatePositionInContext : Delta -> Maybe (G.NodeContext Point2D e) -> Maybe (G.NodeContext Point2D e)
updatePositionInContext delta =
    Maybe.map (mapNodeCtx (updatePosition delta))


updatePosition : Delta -> Point2D -> Point2D
updatePosition ( dx, dy ) (Point2D x y) =
    Point2D (x + dx) (y + dy)


startDragging : G.NodeId -> Model -> Model
startDragging id model =
    { model | draggedNode = Just id }


stopDragging : Model -> Model
stopDragging model =
    { model | draggedNode = Nothing }
