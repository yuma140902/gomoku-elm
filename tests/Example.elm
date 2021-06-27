module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra
import Main exposing (Direction)
import Svg.Attributes exposing (in_)
import Test exposing (..)


suite : Test
suite =
    describe "Main module"
        [ describe "nth"
            [ test "0th" <|
                \_ ->
                    Main.nth 0 [ 1, 2, 3 ] |> Expect.equal (Just 1)
            , test "3rd" <|
                \_ ->
                    Main.nth 2 [ 1, 2, 3 ] |> Expect.equal (Just 3)
            , test "out of range" <|
                \_ ->
                    Main.nth 99 [ 1, 2, 3 ] |> Expect.equal Nothing
            , test "out of range negative" <|
                \_ ->
                    Main.nth -1 [ 1, 2, 3 ] |> Expect.equal Nothing
            ]
        , describe "unwrap"
            [ test "just just a" <|
                \_ ->
                    Main.unwrap (Just (Just 1)) |> Expect.equal (Just 1)
            , test "just nothing" <|
                \_ ->
                    Main.unwrap (Just Nothing) |> Expect.equal Nothing
            , test "nothing" <|
                \_ ->
                    Main.unwrap Nothing |> Expect.equal Nothing
            ]
        , describe "flatten"
            [ test "list of lists" <|
                \_ ->
                    Main.flatten [ [ 1, 2 ], [ 3, 4, 5 ] ] |> Expect.equalLists [ 1, 2, 3, 4, 5 ]
            , test "list of lists of lists" <|
                \_ ->
                    Main.flatten [ [ [ 1, 2 ], [ 3, 4 ] ], [ [ 5, 6 ], [ 7, 8 ] ] ]
                        |> Expect.equalLists [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ] ]
            ]
        , describe "serializeIdx"
            [ todo "serializeIdx should not be used" ]
        , describe "getCellAt"
            [ test "first row" <|
                \_ ->
                    let
                        cells =
                            List.Extra.setAt 3 (Just Main.Black) (List.repeat (Main.rows * Main.columns) Nothing)
                    in
                    Main.getCellAt 0 3 cells |> Expect.equal (Just Main.Black)
            , test "reference to nothing" <|
                \_ ->
                    let
                        cells =
                            List.Extra.setAt 3 (Just Main.Black) (List.repeat (Main.rows * Main.columns) Nothing)
                    in
                    Main.getCellAt 4 4 cells |> Expect.equal Nothing
            , test "out of range" <|
                \_ ->
                    let
                        cells =
                            List.repeat (Main.rows * Main.columns) (Just Main.Black)
                    in
                    Main.getCellAt 999 999 cells |> Expect.equal Nothing
            , test "out of range negative" <|
                \_ ->
                    let
                        cells =
                            List.repeat (Main.rows * Main.columns) (Just Main.Black)
                    in
                    Main.getCellAt -1 -1 cells |> Expect.equal Nothing
            ]
        , describe "toggleTurn"
            [ test "black turns white" <|
                \_ ->
                    Main.toggleTurn Main.Black |> Expect.equal Main.White
            , test "white turns black" <|
                \_ ->
                    Main.toggleTurn Main.White |> Expect.equal Main.Black
            ]
        , describe "serializePoint"
            [ todo "serializePoint should change its arguments" ]
        , describe "negateDirection"
            [ test "positive" <|
                \_ ->
                    Main.negateDirection (Main.Direction 1 2) |> Expect.equal (Main.Direction -1 -2)
            , test "negative" <|
                \_ ->
                    Main.negateDirection (Main.Direction -4 -3) |> Expect.equal (Main.Direction 4 3)
            , test "positive and negative" <|
                \_ ->
                    Main.negateDirection (Main.Direction -6 7) |> Expect.equal (Main.Direction 6 -7)
            , test "zero to zero" <|
                \_ ->
                    Main.negateDirection (Main.Direction 0 0) |> Expect.equal (Main.Direction 0 0)
            ]
        , describe "neighborPoint"
            [ test "neighbor" <|
                \_ ->
                    Main.neighborPoint (Main.Point 0 0) (Main.Direction 1 -1) 3 |> Expect.equal (Main.Point 3 -3)
            , fuzz
                (tuple
                    ( tuple ( int, int ) |> map (\( x, y ) -> Main.Point x y)
                    , tuple ( int, int ) |> map (\( i, j ) -> Main.Direction i j)
                    )
                )
                "origin"
              <|
                \( p, dir ) ->
                    Main.neighborPoint p dir 0 |> Expect.equal p
            , fuzz
                (tuple
                    ( tuple ( int, int )
                        |> map (\( x, y ) -> Main.Point x y)
                    , int
                    )
                )
                "dir zero"
              <|
                \( p, distance ) ->
                    Main.neighborPoint p (Direction 0 0) distance |> Expect.equal p
            ]
        , describe "pointsAlongDirection"
            [ test "a" <|
                \_ ->
                    Main.pointsAlongDirection 4 (Main.Point 0 0) (Main.Direction 1 -1)
                        |> Expect.equalLists [ Main.Point 0 0, Main.Point 1 -1, Main.Point 2 -2, Main.Point 3 -3 ]
            , test "length zero" <|
                \_ ->
                    Main.pointsAlongDirection 0 (Main.Point 0 0) (Main.Direction 1 1)
                        |> Expect.equalLists []
            , test "dir zero" <|
                \_ ->
                    Main.pointsAlongDirection 3 (Main.Point 42 3) (Main.Direction 0 0)
                        |> Expect.equalLists [ Main.Point 42 3, Main.Point 42 3, Main.Point 42 3 ]
            ]
        ]
