module ForceLayout.State exposing (init, update, subscriptions)

import ForceLayout.Physics exposing (..)
import ForceLayout.Types exposing (..)
import Random
import Graph exposing (NodeId)
import Graph as G
import Time exposing (Time, millisecond)


init : ( Model, Cmd Msg )
init =
    ( G.empty, Random.generate InitRandomPositions pointsGen )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg graph =
    case msg of
        InitRandomPositions rndPos ->
            ( initGraph rndPos, Cmd.none )

        AnimationTick ->
            let
                newGraph =
                    updatePositions 0.01 graph
            in
                ( newGraph, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (25 * millisecond) (\_ -> AnimationTick)


initGraph : List Point2D -> LayoutGraph
initGraph positions =
    G.fromNodeLabelsAndEdgePairs positions sampleEdges


pointGen : Random.Generator Point2D
pointGen =
    let
        xgen =
            Random.float 0 canvasWidth

        ygen =
            Random.float 0 canvasHeight
    in
        Random.map2 Point2D xgen ygen


pointsGen : Random.Generator (List Point2D)
pointsGen =
    Random.list 8 pointGen


sampleEdges : List ( NodeId, NodeId )
sampleEdges =
    [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 0 ), ( 4, 5 ), ( 5, 6 ), ( 6, 7 ), ( 7, 4 ), ( 0, 4 ), ( 1, 5 ), ( 2, 6 ), ( 3, 7 ) ]
