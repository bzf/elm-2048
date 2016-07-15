module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Action exposing (Action)
import Dict
import Model exposing (Model)
import Grid exposing (Point)


type alias Cell =
    ( Point, { value : Int } )


view : Model -> Html Action
view model =
    div []
        [ h1 [] [ text "elm-2048" ]
        , button [ class "btn", onClick Action.ResetGame ] [ text "Reset" ]
        , drawModel model
        ]


drawModel : Model -> Html Action
drawModel model =
    div []
        [ drawGame model
        , maybeDrawGameOverOverlay model
        ]


maybeDrawGameOverOverlay : Model -> Html Action
maybeDrawGameOverOverlay model =
    case model.gameOver of
        True ->
            div [ class "game--over" ]
                [ h1 [] [ text "Game over!" ]
                , button [ onClick Action.ResetGame, class "btn" ] [ text "Play again" ]
                ]

        False ->
            div [] []


drawGame : Model -> Html Action
drawGame model =
    let
        cells =
            Dict.toList model.grid

        filterRow index =
            List.filter (\( ( x, y ), _ ) -> y == index)
    in
        div [ class "game" ]
            [ drawRow (filterRow 0 cells) 0
            , drawRow (filterRow 1 cells) 1
            , drawRow (filterRow 2 cells) 2
            , drawRow (filterRow 3 cells) 3
            ]


drawRow : List Cell -> Int -> Html Action
drawRow row rowIndex =
    let
        findCell row index =
            row
                |> List.filter (\( point, _ ) -> fst point == index)
                |> List.head
    in
        div [ class "game__row" ]
            [ drawCell <| findCell row 0
            , drawCell <| findCell row 1
            , drawCell <| findCell row 2
            , drawCell <| findCell row 3
            ]


drawCell : Maybe Cell -> Html Action
drawCell maybeCell =
    case maybeCell of
        Just cell ->
            let
                value =
                    snd cell
                        |> (\x -> x.value)
                        |> toString
            in
                div [ class <| "game__cell game__cell--" ++ value ]
                    [ text value ]

        Nothing ->
            div [ class "game__cell game__cell--empty" ] []
