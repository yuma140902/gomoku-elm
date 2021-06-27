module MainTest exposing (..)

import Direction exposing (Direction)
import Expect exposing (Expectation)
import Fuzz exposing (..)
import List.Extra
import Main
import Point exposing (Point)
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
                    Main.getCellAt (Point 0 3) cells |> Expect.equal (Just Main.Black)
            , test "reference to nothing" <|
                \_ ->
                    let
                        cells =
                            List.Extra.setAt 3 (Just Main.Black) (List.repeat (Main.rows * Main.columns) Nothing)
                    in
                    Main.getCellAt (Point 4 4) cells |> Expect.equal Nothing
            , test "out of range" <|
                \_ ->
                    let
                        cells =
                            List.repeat (Main.rows * Main.columns) (Just Main.Black)
                    in
                    Main.getCellAt (Point 999 999) cells |> Expect.equal Nothing
            , test "out of range negative" <|
                \_ ->
                    let
                        cells =
                            List.repeat (Main.rows * Main.columns) (Just Main.Black)
                    in
                    Main.getCellAt (Point -1 -1) cells |> Expect.equal Nothing
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
        ]
