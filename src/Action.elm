module Action exposing (..)

import Direction exposing (Direction)


type Action
    = OnMove Direction
    | AddCell (Int)
    | Noop
