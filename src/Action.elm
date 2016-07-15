module Action exposing (..)

import Direction exposing (Direction)


type Action
    = OnMove Direction
    | ResetGame
    | AddCell (Int)
    | Noop
