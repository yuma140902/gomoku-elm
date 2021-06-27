module DirectionTest exposing (..)

import Direction exposing (Direction)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Direction module"
        [ describe "negateDirection"
            [ test "positive" <|
                \_ ->
                    Direction.negate (Direction 1 2) |> Expect.equal (Direction -1 -2)
            , test "negative" <|
                \_ ->
                    Direction.negate (Direction -4 -3) |> Expect.equal (Direction 4 3)
            , test "positive and negative" <|
                \_ ->
                    Direction.negate (Direction -6 7) |> Expect.equal (Direction 6 -7)
            , test "zero to zero" <|
                \_ ->
                    Direction.negate (Direction 0 0) |> Expect.equal (Direction 0 0)
            ]
        ]
