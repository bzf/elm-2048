module Grid exposing (..)

import Dict exposing (Dict)
import List.Extra
import Direction exposing (Direction(..))


type alias Point =
    -- The tuple consists of `(x, y)` values
    ( Int, Int )


type alias Grid =
    Dict Point Int


new : Grid
new =
    Dict.empty


moveEverything : Direction -> Grid -> Grid
moveEverything direction grid =
    case direction of
        Left ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( i, (snd (fst cell)) ), (snd cell) )
                        )
            in
                rows grid
                    |> List.map moveRow
                    |> List.concat
                    |> Dict.fromList

        Right ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( 3 - i, (snd (fst cell)) ), (snd cell) )
                        )
            in
                rows grid
                    |> List.map List.reverse
                    |> List.map moveRow
                    |> List.map List.reverse
                    |> List.concat
                    |> Dict.fromList

        Up ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( (fst (fst cell)), i ), (snd cell) )
                        )
            in
                columns grid
                    |> List.map moveRow
                    |> List.concat
                    |> Dict.fromList

        Down ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( (fst (fst cell)), 3 - i ), (snd cell) )
                        )
            in
                columns grid
                    |> List.map List.reverse
                    |> List.map moveRow
                    |> List.map List.reverse
                    |> List.concat
                    |> Dict.fromList


handleMove : Direction -> Grid -> Grid
handleMove direction grid =
    case direction of
        Left ->
            handleLeft grid

        Right ->
            handleRight grid

        Up ->
            handleUp grid

        Down ->
            handleDown grid


handleLeft : Grid -> Grid
handleLeft grid =
    rows grid
        |> List.map mergeCells
        |> List.concat
        |> Dict.fromList


handleRight : Grid -> Grid
handleRight grid =
    rows grid
        |> List.map List.reverse
        |> List.map mergeCells
        |> List.concat
        |> Dict.fromList


handleUp : Grid -> Grid
handleUp grid =
    columns grid
        |> List.map mergeCellsVertical
        |> List.concat
        |> Dict.fromList


handleDown : Grid -> Grid
handleDown grid =
    columns grid
        |> List.map List.reverse
        |> List.map mergeCellsVertical
        |> List.concat
        |> Dict.fromList


availableCells : Grid -> List Point
availableCells grid =
    List.Extra.zip (List.repeat 4 [0..3] |> List.concat)
        (List.map (List.repeat 4) [0..3] |> List.concat)
        |> List.filter (\point -> not <| Dict.member point grid)


columns : Grid -> List (List ( Point, Int ))
columns grid =
    grid
        |> Dict.toList
        |> List.sortBy (\cell -> fst (fst cell))
        |> List.Extra.groupWhile (\c1 c2 -> (fst (fst c1)) == (fst (fst c2)))
        |> List.map (List.sortBy (\cell -> snd (fst cell)))


rows : Grid -> List (List ( Point, Int ))
rows grid =
    grid
        |> Dict.toList
        |> List.sortBy (\cell -> snd (fst cell))
        |> List.Extra.groupWhile (\c1 c2 -> (snd (fst c1)) == (snd (fst c2)))
        |> List.map (List.sortBy (\cell -> fst (fst cell)))


mergeCellsVertical : List ( Point, Int ) -> List ( Point, Int )
mergeCellsVertical list =
    case list of
        [] ->
            []

        v :: [] ->
            [ v ]

        x :: y :: rest ->
            if (snd x) == (snd y) then
                ( (fst x), (snd x) * 2 ) :: (mergeCellsVertical rest)
            else
                List.append [ x ] (mergeCellsVertical (y :: rest))


mergeCells : List ( Point, Int ) -> List ( Point, Int )
mergeCells list =
    case list of
        [] ->
            []

        v :: [] ->
            [ v ]

        x :: y :: rest ->
            if (snd x) == (snd y) then
                ( (fst x), (snd x) * 2 ) :: (mergeCells rest)
            else
                List.append [ x ] (mergeCells (y :: rest))


addPointToGrid : Grid -> Point -> Grid
addPointToGrid grid point =
    Dict.insert point 2 grid
