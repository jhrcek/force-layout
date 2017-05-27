module ForceLayout.State exposing (init, update, subscriptions)

import ForceLayout.DragDrop exposing (..)
import ForceLayout.Physics exposing (..)
import ForceLayout.Types exposing (..)
import ForceLayout.SampleGraphs exposing (..)
import Draggable
import Graph exposing (NodeId)
import Graph as G
import Random
import Time exposing (Time, millisecond)


init : ( Model, Cmd Msg )
init =
    ( { graph = G.empty
      , draggedNode = Nothing
      , layoutSettings = defaultLayoutSettings
      , drag = Draggable.init
      , example = Cube
      }
    , newGraphInitCommand Cube
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ graph, layoutSettings, example } as model) =
    case msg of
        InitRandomPositions randomPoints ->
            ( { model | graph = makeGraph randomPoints <| getEdges example }, Cmd.none )

        AnimationTick ->
            let
                newGraph =
                    updatePositions layoutSettings graph
            in
                ( { model | graph = newGraph }, Cmd.none )

        SetCharge newCharge ->
            let
                newSettings =
                    { layoutSettings | charge = parseFloat newCharge }
            in
                ( { model | layoutSettings = newSettings }, Cmd.none )

        SetStiffness newStiffness ->
            let
                newSettings =
                    { layoutSettings | stiffness = parseFloat newStiffness }
            in
                ( { model | layoutSettings = newSettings }, Cmd.none )

        SetTimeDiff newDiff ->
            let
                newSettings =
                    { layoutSettings | timeDiff = parseFloat newDiff }
            in
                ( { model | layoutSettings = newSettings }, Cmd.none )

        Randomize ->
            ( model, newGraphInitCommand example )

        SelectExample ex ->
            ( { model | example = ex }, newGraphInitCommand ex )

        NodeDragStart nid ->
            ( startDragging nid model, Cmd.none )

        NodeDraggedBy delta ->
            ( dragNode model delta, Cmd.none )

        NodeDragEnd ->
            ( stopDragging model, Cmd.none )

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


newGraphInitCommand : PredefinedExample -> Cmd Msg
newGraphInitCommand example =
    Random.generate InitRandomPositions <| getNodeIdGenerator example


parseFloat : String -> Float
parseFloat =
    Result.withDefault 0 << String.toFloat


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Sub.batch
        [ Time.every (25 * millisecond) (\_ -> AnimationTick)
        , Draggable.subscriptions DragMsg drag
        ]
