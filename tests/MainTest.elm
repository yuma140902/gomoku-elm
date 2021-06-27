module MainTest exposing (..)

import Array
import Direction exposing (Direction)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Main exposing (Player(..), Stone(..))
import Point exposing (Point)
import Test exposing (..)


suite : Test
suite =
    describe "Main module"
        [ describe "unwrapStone"
            [ test "just exist a" <|
                \_ ->
                    Main.unwrapStone (Just (Exist Black)) |> Expect.equal (Exist Black)
            , test "just none" <|
                \_ ->
                    Main.unwrapStone (Just None) |> Expect.equal None
            , test "nothing" <|
                \_ ->
                    Main.unwrapStone Nothing |> Expect.equal None
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
        , describe "toStone"
            [ test "just a" <|
                \_ ->
                    Main.toStone (Just Black) |> Expect.equal (Exist Black)
            , test "nothing" <|
                \_ ->
                    Main.toStone Nothing |> Expect.equal None
            ]
        , describe "serializeIdx"
            [ todo "serializeIdx should not be used" ]
        , describe "getCellAt"
            [ test "first row" <|
                \_ ->
                    let
                        cells =
                            Array.initialize (Main.rows * Main.columns) (always None) |> Array.set 3 (Exist Black)
                    in
                    Main.getCellAtPoint (Point 0 3) cells |> Expect.equal (Exist Black)
            , test "reference to nothing" <|
                \_ ->
                    let
                        cells =
                            Array.initialize (Main.rows * Main.columns) (always None) |> Array.set 3 (Exist Black)
                    in
                    Main.getCellAtPoint (Point 4 4) cells |> Expect.equal None
            , test "out of range" <|
                \_ ->
                    let
                        cells =
                            Array.initialize (Main.rows * Main.columns) (always (Exist Black))
                    in
                    Main.getCellAtPoint (Point 999 999) cells |> Expect.equal None
            , test "out of range negative" <|
                \_ ->
                    let
                        cells =
                            Array.initialize (Main.rows * Main.columns) (always (Exist Black))
                    in
                    Main.getCellAtPoint (Point -1 -1) cells |> Expect.equal None
            ]
        , describe "toggleTurn"
            [ test "black turns white" <|
                \_ ->
                    Main.toggleTurn Black |> Expect.equal White
            , test "white turns black" <|
                \_ ->
                    Main.toggleTurn White |> Expect.equal Black
            ]
        , describe "serializePoint"
            [ todo "serializePoint should change its arguments" ]
        ]
