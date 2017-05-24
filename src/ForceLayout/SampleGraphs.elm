module ForceLayout.SampleGraphs exposing (..)

import ForceLayout.Types exposing (..)
import Graph exposing (NodeId, fromNodeLabelsAndEdgePairs)
import Random exposing (Generator)


type alias Edges =
    List ( NodeId, NodeId )


type PredefinedExample
    = Tetrahedron
    | Cube
    | Circle Int


makeGraph : List Point2D -> Edges -> LayoutGraph
makeGraph nodePositions edges =
    fromNodeLabelsAndEdgePairs nodePositions edges


pointGen : Generator Point2D
pointGen =
    let
        xgen =
            Random.float 0 canvasWidth

        ygen =
            Random.float 0 canvasHeight
    in
        Random.map2 Point2D xgen ygen


randomPointsGenerator : Edges -> Generator (List Point2D)
randomPointsGenerator edges =
    let
        maximalId =
            List.concatMap (\( a, b ) -> [ a, b ]) edges |> List.maximum |> Maybe.withDefault -1
    in
        -- nodes in the graph will be all numbers from (0 to maximalId), so generate position for each of them
        Random.list (maximalId + 1) pointGen



-- Sample graphs


cube : Edges
cube =
    [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 0 ), ( 4, 5 ), ( 5, 6 ), ( 6, 7 ), ( 7, 4 ), ( 0, 4 ), ( 1, 5 ), ( 2, 6 ), ( 3, 7 ) ]


circle : Int -> Edges
circle nodeCount =
    List.range 1 nodeCount |> List.map (\n -> ( n - 1, n )) |> \pairs -> ( nodeCount, 0 ) :: pairs


tetrahedron : Edges
tetrahedron =
    [ ( 0, 1 ), ( 1, 2 ), ( 2, 0 ), ( 0, 3 ), ( 1, 3 ), ( 2, 3 ) ]
