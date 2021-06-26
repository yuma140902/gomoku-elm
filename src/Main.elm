module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, li, ol, section, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, r, viewBox)



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- UTILITY


nth : Int -> List a -> Maybe a
nth idx xs =
    List.head (List.drop idx xs)


unwrap : Maybe (Maybe a) -> Maybe a
unwrap mm =
    case mm of
        Just Nothing ->
            Nothing

        Nothing ->
            Nothing

        Just (Just a) ->
            Just a



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
    | Place Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( initialModel, Cmd.none )

        Place i j ->
            --TODO
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
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
    div [] []


goban : Board -> List (Html Msg)
goban cells =
    List.map
        (\i ->
            div [ class "board-row" ] (List.map (\j -> renderCell i j cells) (List.range 0 columns))
        )
        (List.range 0 rows)


renderCell : Int -> Int -> Board -> Html Msg
renderCell i j cells =
    div [ class "cell-wrapper" ]
        [ div [ class "cell", onClick (Place i j) ]
            [ case unwrap (nth (serializeIdx i j) cells) of
                Just Black ->
                    svg [ viewBox "0 0 100 100" ] [ circle [ cx "50", cy "50", r "30", fill "black" ] [] ]

                Just White ->
                    svg [ viewBox "0 0 100 100" ] [ circle [ cx "50", cy "50", r "30", fill "white" ] [] ]

                Nothing ->
                    svg [] []
            ]
        ]


serializeIdx : Int -> Int -> Int
serializeIdx x y =
    x * rows + y


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
