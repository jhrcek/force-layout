module ForceLayout.State exposing (init, update, subscriptions)

import ForceLayout.Physics exposing (..)
import ForceLayout.Types exposing (..)
import ForceLayout.SampleGraphs exposing (..)
import Random
import Graph exposing (NodeId)
import Graph as G
import Time exposing (Time, millisecond)


init : ( Model, Cmd Msg )
init =
    let
        randomPointsGenerationCommand =
            Random.generate InitRandomPositions <| randomPointsGenerator hypercube
    in
        ( { graph = G.empty, layoutSettings = defaultLayoutSettings }, randomPointsGenerationCommand )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ graph, layoutSettings } as model) =
    case msg of
        InitRandomPositions randomPoints ->
            ( { model | graph = makeGraph randomPoints hypercube }, Cmd.none )

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


parseFloat : String -> Float
parseFloat =
    Result.withDefault 0 << String.toFloat


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (25 * millisecond) (\_ -> AnimationTick)
