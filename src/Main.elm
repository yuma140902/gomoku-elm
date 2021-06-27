module Main exposing (..)

import Browser
import Direction exposing (Direction)
import Html exposing (Html, a, button, div, h1, li, ol, p, section, span, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import List.Extra
import Point exposing (Point)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox)



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- UTILITY


nth : Int -> List a -> Maybe a
nth =
    List.Extra.getAt


unwrap : Maybe (Maybe a) -> Maybe a
unwrap mm =
    case mm of
        Just m ->
            m

        Nothing ->
            Nothing


flatten : List (List a) -> List a
flatten xss =
    List.foldr List.append [] xss


serializeIdx : Int -> Int -> Int
serializeIdx x y =
    x * columns + y


getCellAt : Point -> Board -> Maybe Player
getCellAt p cells =
    unwrap (nth (serializePoint p) cells)


toggleTurn : Player -> Player
toggleTurn player =
    case player of
        Black ->
            White

        White ->
            Black


judgeWinner : Point -> Player -> Board -> Maybe Player
judgeWinner p turn cells =
    if
        List.foldr (||)
            False
            (flatten
                (List.map
                    (\dir ->
                        List.map
                            (\{ x, y } -> isPlayerWinnerAlongDirection x y dir turn cells)
                            (Point.listAlongDirection 5 p (Direction.negate dir))
                    )
                    directions
                )
            )
    then
        Just turn

    else
        Nothing


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
    List.foldr
        (\idx a -> a && (unwrap (nth idx cells) == Just player))
        True
        (List.map
            (\p -> serializePoint p)
            (Point.listAlongDirection 5 (Point i j) dir)
        )



-- MODEL


type Player
    = Black
    | White


type alias Board =
    List (Maybe Player)


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
    { cells = List.repeat (rows * columns) Nothing
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
            if model.isFinished || getCellAt p model.cells /= Nothing then
                ( model, Cmd.none )

            else
                let
                    newCells =
                        List.Extra.setAt (serializePoint p) (Just model.turn) model.cells
                in
                case judgeWinner p model.turn newCells of
                    Nothing ->
                        ( { model
                            | cells = newCells
                            , turn = toggleTurn model.turn
                          }
                        , Cmd.none
                        )

                    Just player ->
                        ( { model
                            | cells = newCells
                            , isFinished = True
                            , winner = Just player
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
        [ h1 [] [ text "五目並べ" ]
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
        p [] [ text "次は", div [ class "inline-stone" ] [ cellSvg (Just model.turn) ], text "の手番です" ]

    else
        p [] [ div [ class "inline-stone" ] [ cellSvg model.winner ], text "が勝ちました" ]


goban : Board -> List (Html Msg)
goban cells =
    List.map
        (\i ->
            div [ class "board-row" ] (List.map (\j -> renderCell (Point i j) cells) (List.range 0 (columns - 1)))
        )
        (List.range 0 (rows - 1))


renderCell : Point -> Board -> Html Msg
renderCell p cells =
    div [ class "cell-wrapper" ]
        [ div [ class "cell", onClick (Place p) ]
            [ cellSvg (getCellAt p cells) ]
        ]


cellSvg : Maybe Player -> Html Msg
cellSvg player =
    case player of
        Just Black ->
            svg [ viewBox "0 0 100 100" ]
                [ circle [ cx "50", cy "50", r "30", fill "black", stroke "black", strokeWidth "5" ] [] ]

        Just White ->
            svg [ viewBox "0 0 100 100" ]
                [ circle [ cx "50", cy "50", r "30", fill "white", stroke "black", strokeWidth "5" ] [] ]

        Nothing ->
            span [] []


sidebar : Html Msg
sidebar =
    div [ id "side", class "item" ]
        [ section []
            [ h1 [] [ text "遊び方" ] ]
        , ol []
            [ li [] [ text "白黒を交互に打ちます" ]
            , li [] [ text "先に5つ縦横斜めのどこかに連続して並べたら勝利です" ]
            ]
        ]
