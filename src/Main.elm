module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Time exposing (second)

main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Configuration =
    Int


type alias Model =
    Int


init : Configuration -> ( Model, Cmd Msg )
init configuration =
    ( configuration, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text <| "The counter is " ++ (toString model) ]


type Msg
    = Tick


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick ->
            ( model + 1, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second (always Tick)
