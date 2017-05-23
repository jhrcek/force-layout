module ForceLayout.Physics exposing (..)

import ForceLayout.Types exposing (..)
import Math.Vector2 as V
import Graph as G


updatePositions : Float -> LayoutGraph -> LayoutGraph
updatePositions deltaTime graph =
    G.mapContexts
        (\ctx ->
            let
                { node } =
                    ctx
            in
                { ctx | node = updatePosition deltaTime node graph }
        )
        graph


updatePosition :
    Float -- Time since the last update
    -> PositionedNode -- Vertex we are analysing
    -> LayoutGraph
    -> PositionedNode -- New position
updatePosition deltaTime node graph =
    let
        { id, label } =
            node

        nodeCoords =
            label

        -- Gets a velocity by multiplying the time by the force (we take the mass to be 1).
        getVel force otherNode =
            V.scale deltaTime <| force nodeCoords otherNode

        -- Sum all the pushing and pulling.  All vertices push, the connected vertices pull.
        pushVector =
            G.fold
                (\ctx acc ->
                    let
                        otherNode =
                            ctxToPoint ctx
                    in
                        V.add (getVel pushForce otherNode) acc
                )
                zeroV2
                graph

        pullVector =
            neighborPoints graph id
                |> List.foldr (\otherNode acc -> V.add (getVel pullForce otherNode) acc) zeroV2

        (Point2D x y) =
            nodeCoords

        newPosition =
            Point2D (x + V.getX pushVector + V.getX pullVector) (y + V.getY pushVector + V.getY pullVector)
    in
        { node | label = newPosition }


neighborPoints : LayoutGraph -> G.NodeId -> List Point2D
neighborPoints graph nid =
    G.get nid graph
        |> Maybe.map (\ctx -> G.alongIncomingEdges ctx ++ G.alongOutgoingEdges ctx |> List.map (getCoords graph))
        |> Maybe.withDefault []


charge : Float
charge =
    10000


p2v : Point2D -> V.Vec2
p2v (Point2D x y) =
    V.vec2 x y


pushForce :
    Point2D -- Vertex we're calculating the force for
    -> Point2D -- Vertex pushing the other away
    -> V.Vec2
pushForce p1 p2 =
    let
        v1 =
            p2v p1

        v2 =
            p2v p2

        diff =
            V.sub v1 v2

        l =
            V.length diff
    in
        if l > 0 then
            V.scale (charge / l) (V.normalize diff)
        else
            {- vertex doesn't affects itself -}
            zeroV2


stiffness : Float
stiffness =
    0.5


pullForce : Point2D -> Point2D -> V.Vec2
pullForce p1 p2 =
    let
        v1 =
            p2v p1

        v2 =
            p2v p2
    in
        V.scale stiffness <| V.sub v2 v1


zeroV2 : V.Vec2
zeroV2 =
    V.vec2 0 0
