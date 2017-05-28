module ForceLayout.Physics exposing (updatePositions)

import ForceLayout.Types exposing (..)
import Math.Vector2 as V
import Graph as G


updatePositions : LayoutSettings -> LayoutGraph -> LayoutGraph
updatePositions settings graph =
    G.mapContexts (mapNodeInContext (updatePosition settings graph)) graph


updatePosition :
    LayoutSettings
    -> LayoutGraph
    -> PositionedNode -- Vertex we are analysing
    -> PositionedNode -- New position
updatePosition { charge, stiffness, timeDiff } graph node =
    let
        nodeId =
            node.id

        nodePosition =
            node.label

        (Point2D x y) =
            nodePosition

        -- Gets a velocity by multiplying the time by the force (we take the mass to be 1).
        getVel force otherNodePosition =
            V.scale timeDiff <| force nodePosition otherNodePosition

        -- Sum of repulsive forces from All nodes
        pushVector =
            G.fold
                (\ctx acc ->
                    let
                        otherNodePosition =
                            ctxToPoint ctx
                    in
                        V.add (getVel (pushForce charge) otherNodePosition) acc
                )
                zeroV2
                graph

        -- Sum attractive forces from Connected node
        pullVector =
            neighborPoints graph nodeId
                |> List.foldr (\otherNode acc -> V.add (getVel (pullForce stiffness) otherNode) acc) zeroV2

        newPosition =
            Point2D (x + V.getX pushVector + V.getX pullVector) (y + V.getY pushVector + V.getY pullVector)
    in
        { node | label = newPosition }


neighborPoints : LayoutGraph -> G.NodeId -> List Point2D
neighborPoints graph nid =
    G.get nid graph
        |> Maybe.map (\ctx -> G.alongIncomingEdges ctx ++ G.alongOutgoingEdges ctx |> List.map (getCoords graph))
        |> Maybe.withDefault []


p2v : Point2D -> V.Vec2
p2v (Point2D x y) =
    V.vec2 x y


pushForce :
    Float
    -> Point2D -- Vertex we're calculating the force for
    -> Point2D -- Vertex pushing the other away
    -> V.Vec2
pushForce charge p1 p2 =
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


pullForce : Float -> Point2D -> Point2D -> V.Vec2
pullForce stiffness p1 p2 =
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
