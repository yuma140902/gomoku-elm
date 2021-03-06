module Main exposing (..)

import Array exposing (Array)
import Browser
import Direction exposing (Direction)
import Html exposing (Html, a, button, div, h1, li, ol, p, section, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Point exposing (Point)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox)



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- UTILITY


unwrapStone : Maybe Stone -> Stone
unwrapStone mm =
    case mm of
        Just m ->
            m

        Nothing ->
            None


toStone : Maybe Player -> Stone
toStone mp =
    case mp of
        Just p ->
            Exist p

        Nothing ->
            None


flatten : List (List a) -> List a
flatten xss =
    List.foldr List.append [] xss


serializeIdx : Int -> Int -> Int
serializeIdx x y =
    x * columns + y


getCellAtPoint : Point -> Board -> Stone
getCellAtPoint p cells =
    unwrapStone (Array.get (serializePoint p) cells)


getCellAt : Int -> Board -> Stone
getCellAt idx cells =
    unwrapStone (Array.get idx cells)


toggleTurn : Player -> Player
toggleTurn player =
    case player of
        Black ->
            White

        White ->
            Black


isPlayerWinner : Point -> Player -> Board -> Bool
isPlayerWinner p player cells =
    flatten
        (directions
            |> List.map
                (\dir ->
                    Point.listAlongDirection 5 p (Direction.negate dir)
                        |> List.map
                            (\{ x, y } -> isPlayerWinnerAlongDirection x y dir player cells)
                )
        )
        |> List.foldr (||)
            False


serializePoint : Point -> Int
serializePoint { x, y } =
    x * columns + y


directions : List Direction
directions =
    [ Direction 1 0
    , Direction 0 1
    , Direction 1 1
    , Direction 1 -1
    ]


isPlayerWinnerAlongDirection : Int -> Int -> Direction -> Player -> Board -> Bool
isPlayerWinnerAlongDirection i j dir player cells =
    Point.listAlongDirection 5 (Point i j) dir
        |> List.map
            (\p -> serializePoint p)
        |> List.foldr
            (\idx a -> a && getCellAt idx cells == Exist player)
            True



-- MODEL


type Player
    = Black
    | White


type Stone
    = Exist Player
    | None


type alias Board =
    Array Stone


type alias Model =
    { cells : Board
    , turn : Player
    , isFinished : Bool
    , winner : Maybe Player
    }


rows : Int
rows =
    10


columns : Int
columns =
    10


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { cells = Array.initialize (rows * columns) (always None)
    , turn = White
    , isFinished = False
    , winner = Nothing
    }



-- UPDATE


type Msg
    = Reset
    | Place Point


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialModel, Cmd.none )

        Place p ->
            if model.isFinished || getCellAtPoint p model.cells /= None then
                ( model, Cmd.none )

            else
                let
                    newCells =
                        Array.set (serializePoint p) (Exist model.turn) model.cells
                in
                if not (isPlayerWinner p model.turn newCells) then
                    ( { model
                        | cells = newCells
                        , turn = toggleTurn model.turn
                      }
                    , Cmd.none
                    )

                else
                    ( { model
                        | cells = newCells
                        , isFinished = True
                        , winner = Just model.turn
                      }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "App" ]
        [ h1 [] [ text "????????????" ]
        , div [ class "container" ] [ board model, sidebar ]
        ]


board : Model -> Html Msg
board model =
    div [ id "game", class "item" ]
        [ statusbox model
        , div [ id "goban" ] (goban model.cells)
        , button [ onClick Reset ] [ text "RESTART" ]
        ]


statusbox : Model -> Html Msg
statusbox model =
    if not model.isFinished then
        p [] [ text "??????", div [ class "inline-stone" ] [ stoneSvg (Exist model.turn) ], text "???????????????" ]

    else
        p []
            [ div [ class "inline-stone" ] [ stoneSvg (toStone model.winner) ], text "??????????????????" ]


goban : Board -> List (Html Msg)
goban cells =
    List.range 0 (rows - 1)
        |> List.map
            (\i ->
                div [ class "board-row" ]
                    (List.range 0 (columns - 1)
                        |> List.map (\j -> renderCell (Point i j) cells)
                    )
            )


renderCell : Point -> Board -> Html Msg
renderCell p cells =
    div [ class "cell-wrapper" ]
        [ div [ class "cell", onClick (Place p) ]
            [ stoneSvg (getCellAtPoint p cells) ]
        ]


stoneSvg : Stone -> Html Msg
stoneSvg player =
    case player of
        Exist Black ->
            svg [ viewBox "0 0 100 100" ]
                [ circle [ cx "50", cy "50", r "30", fill "black", stroke "black", strokeWidth "5" ] [] ]

        Exist White ->
            svg [ viewBox "0 0 100 100" ]
                [ circle [ cx "50", cy "50", r "30", fill "white", stroke "black", strokeWidth "5" ] [] ]

        None ->
            span [] []


sidebar : Html Msg
sidebar =
    div [ id "side", class "item" ]
        [ section []
            [ h1 [] [ text "?????????" ] ]
        , ol []
            [ li [] [ text "??????????????????????????????" ]
            , li [] [ text "??????5??????????????????????????????????????????????????????????????????" ]
            ]
        ]
