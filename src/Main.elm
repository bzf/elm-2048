module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Time exposing (second)
import Keyboard exposing (KeyCode)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Action exposing (Action(..))
import Game


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    Game.Game


type alias Cell =
    ( Game.Point, Int )


init : ( Model, Cmd Action )
init =
    ( Game.new, Cmd.none )


view : Model -> Html Action
view model =
    div []
        [ h1 [] [ text "elm-2048" ]
        , drawGame <| Dict.toList model.grid
        ]


drawGame : List Cell -> Html Action
drawGame cells =
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


update : Action -> Model -> ( Model, Cmd Action )
update message model =
    case message of
        OnMove direction ->
            Game.update model direction

        AddCell randomIndex ->
            ( Game.addCell randomIndex model, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ Time.every second (always Tick)
        , Keyboard.downs (onKeyDown False model)
        ]


onKeyDown : Bool -> Model -> KeyCode -> Action
onKeyDown on _ keycode =
    case keycode of
        37 ->
            OnMove Left

        72 ->
            OnMove Left

        39 ->
            OnMove Right

        76 ->
            OnMove Right

        75 ->
            OnMove Up

        38 ->
            OnMove Up

        74 ->
            OnMove Down

        40 ->
            OnMove Down

        _ ->
            Noop
