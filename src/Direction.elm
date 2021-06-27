module Direction exposing (..)


type alias Direction =
    { diffX : Int, diffY : Int }


negate : Direction -> Direction
negate { diffX, diffY } =
    Direction -diffX -diffY
