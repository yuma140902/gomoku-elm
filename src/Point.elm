module Point exposing (..)

import Direction exposing (Direction)


type alias Point =
    { x : Int, y : Int }


neighbor : Point -> Direction -> Int -> Point
neighbor { x, y } { diffX, diffY } distance =
    Point (x + diffX * distance) (y + diffY * distance)


listAlongDirection : Int -> Point -> Direction -> List Point
listAlongDirection num begin dir =
    let
        n =
            neighbor begin dir
    in
    List.map (\i -> n i) (List.range 0 (num - 1))
