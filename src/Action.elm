module Action exposing (..)

import Direction exposing (Direction)


type Action
    = Tick
    | OnMove Direction
    | AddCell (Int)
    | Noop
