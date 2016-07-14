module Model exposing (Model, new, update, triggerAddCell)

import Dict exposing (Dict)
import List.Extra
import Random
import Action exposing (Action(..))
import Direction exposing (Direction(..))
import Grid exposing (Grid, Point)


type alias Model =
    { grid : Grid
    , lastMove : Maybe Direction
    }


new : Model
new =
    { grid = Grid.new
    , lastMove = Nothing
    }


update : Model -> Direction -> ( Model, Cmd Action )
update model direction =
    let
        grid' =
            model.grid
                |> Grid.handleMove direction
                |> Grid.moveEverything direction

        model' =
            { model | lastMove = Just direction, grid = grid' }
    in
        triggerAddCell model'


triggerAddCell : Model -> ( Model, Cmd Action )
triggerAddCell model =
    ( model
    , Random.generate AddCell (Random.int 0 (List.length (Grid.availableCells model.grid) - 1))
    )
