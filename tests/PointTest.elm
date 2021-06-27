module PointTest exposing (..)

import Direction exposing (Direction)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Point exposing (Point)
import Test exposing (..)


suite : Test
suite =
    describe "Point module"
        [ describe "neighbor"
            [ test "a" <|
                \_ ->
                    Point.neighbor (Point 0 0) (Direction 1 -1) 3 |> Expect.equal (Point 3 -3)
            , fuzz
                (tuple
                    ( tuple ( int, int ) |> map (\( x, y ) -> Point x y)
                    , tuple ( int, int ) |> map (\( i, j ) -> Direction i j)
                    )
                )
                "origin"
              <|
                \( p, dir ) ->
                    Point.neighbor p dir 0 |> Expect.equal p
            , fuzz
                (tuple
                    ( tuple ( int, int )
                        |> map (\( x, y ) -> Point x y)
                    , int
                    )
                )
                "dir zero"
              <|
                \( p, distance ) ->
                    Point.neighbor p (Direction 0 0) distance |> Expect.equal p
            ]
        , describe "listAlongDirection"
            [ test "a" <|
                \_ ->
                    Point.listAlongDirection 4 (Point 0 0) (Direction 1 -1)
                        |> Expect.equalLists [ Point 0 0, Point 1 -1, Point 2 -2, Point 3 -3 ]
            , test "length zero" <|
                \_ ->
                    Point.listAlongDirection 0 (Point 0 0) (Direction 1 1)
                        |> Expect.equalLists []
            , test "dir zero" <|
                \_ ->
                    Point.listAlongDirection 3 (Point 42 3) (Direction 0 0)
                        |> Expect.equalLists [ Point 42 3, Point 42 3, Point 42 3 ]
            ]
        ]
