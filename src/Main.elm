module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Keyboard exposing (KeyCode)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Action exposing (Action(..))
import Model exposing (Model)
import View
import Grid exposing (Grid, Point)


main =
    Html.program
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Action )
init =
    Model.triggerAddCell Model.new


update : Action -> Model -> ( Model, Cmd Action )
update message model =
    case message of
        OnMove direction ->
            Model.update model direction

        AddCell randomIndex ->
            let
                model' =
                    Grid.availableCells model.grid
                        |> List.drop randomIndex
                        |> List.head
                        |> addPoint model

                gameOver' =
                    Grid.isGameOver model'.grid
            in
                ( { model' | gameOver = gameOver' }, Cmd.none )

        ResetGame ->
            init

        Noop ->
            ( model, Cmd.none )


addPoint : Model -> Maybe Point -> Model
addPoint model point =
    case point of
        Just point' ->
            { model | grid = Grid.addPointToGrid model.grid point' }

        Nothing ->
            model


subscriptions : Model -> Sub Action
subscriptions model =
    Sub.batch
        [ Keyboard.downs (onKeyDown False model)
        ]


onKeyDown : Bool -> Model -> KeyCode -> Action
onKeyDown on _ keycode =
    case keycode of
        37 ->
            OnMove Left

        39 ->
            OnMove Right

        38 ->
            OnMove Up

        40 ->
            OnMove Down

        82 ->
            ResetGame

        _ ->
            Noop
