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
            Random.generate InitRandomPositions <| randomPointsGenerator cube
    in
        ( G.empty, randomPointsGenerationCommand )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg graph =
    case msg of
        InitRandomPositions randomPoints ->
            ( makeGraph randomPoints cube, Cmd.none )

        AnimationTick ->
            let
                newGraph =
                    updatePositions 0.01 graph
            in
                ( newGraph, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (25 * millisecond) (\_ -> AnimationTick)
