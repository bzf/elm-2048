module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Action exposing (Action)
import Dict
import Model exposing (Model)


type alias Cell =
    ( Model.Point, Int )


view : Model -> Html Action
view model =
    div []
        [ h1 [] [ text "elm-2048" ]
        , drawModel <| Dict.toList model.grid
        ]


drawModel : List Cell -> Html Action
drawModel cells =
    let
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
                    toString <| snd cell
            in
                div [ class <| "game__cell game__cell--" ++ value ]
                    [ text value ]

        Nothing ->
            div [ class "game__cell game__cell--empty" ] []
