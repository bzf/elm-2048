module Model exposing (Model, Point, new, update, addCell, availableCells)

import Dict exposing (Dict)
import List.Extra
import Random
import Action exposing (Action(..))
import Direction exposing (Direction(..))


type alias Point =
    -- The tuple consists of `(x, y)` values
    ( Int, Int )


type alias Model =
    { grid : Dict Point Int
    , lastMove : Maybe Direction
    }


new : Model
new =
    { grid = newGrid
    , lastMove = Nothing
    }


newGrid : Dict Point Int
newGrid =
    -- TODO: Randomize the starting points
    Dict.empty
        |> Dict.insert ( 0, 0 ) 2
        |> Dict.insert ( 2, 0 ) 2


update : Model -> Direction -> ( Model, Cmd Action )
update model direction =
    let
        model' =
            model
                |> setLastMove direction
                |> handleMove direction
    in
        triggerAddCell model'


triggerAddCell : Model -> ( Model, Cmd Action )
triggerAddCell game =
    ( game
    , Random.generate AddCell (Random.int 0 (List.length (availableCells game) - 1))
    )


setLastMove : Direction -> Model -> Model
setLastMove direction model =
    { model | lastMove = Just direction }


handleMove : Direction -> Model -> Model
handleMove direction model =
    case direction of
        Left ->
            handleLeft model
                |> moveEverything Left

        Right ->
            handleRight model
                |> moveEverything Right

        Up ->
            handleUp model
                |> moveEverything Up

        Down ->
            handleDown model
                |> moveEverything Down


moveEverything : Direction -> Model -> Model
moveEverything direction game =
    case direction of
        Left ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( i, (snd (fst cell)) ), (snd cell) )
                        )

                grid' =
                    rows game
                        |> List.map moveRow
                        |> List.concat
                        |> Dict.fromList
            in
                { game | grid = grid' }

        Right ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( 3 - i, (snd (fst cell)) ), (snd cell) )
                        )

                grid' =
                    rows game
                        |> List.map List.reverse
                        |> List.map moveRow
                        |> List.map List.reverse
                        |> List.concat
                        |> Dict.fromList
            in
                { game | grid = grid' }

        Up ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( (fst (fst cell)), i ), (snd cell) )
                        )

                grid' =
                    columns game
                        |> List.map moveRow
                        |> List.concat
                        |> Dict.fromList
            in
                { game | grid = grid' }

        Down ->
            let
                moveRow =
                    List.indexedMap
                        (\i cell ->
                            ( ( (fst (fst cell)), 3 - i ), (snd cell) )
                        )

                grid' =
                    columns game
                        |> List.map List.reverse
                        |> List.map moveRow
                        |> List.map List.reverse
                        |> List.concat
                        |> Dict.fromList
            in
                { game | grid = grid' }


handleLeft : Model -> Model
handleLeft game =
    let
        currentRows =
            rows game

        grid' =
            List.map mergeCells currentRows
                |> List.concat
                |> Dict.fromList
    in
        { game | grid = grid' }


handleRight : Model -> Model
handleRight game =
    let
        currentRows =
            rows game

        grid' =
            List.map mergeCells currentRows
                |> List.concat
                |> Dict.fromList
    in
        { game | grid = grid' }


handleUp : Model -> Model
handleUp game =
    let
        currentColumns =
            columns game

        grid' =
            List.map mergeCellsVertical currentColumns
                |> List.concat
                |> Dict.fromList
    in
        { game | grid = grid' }


handleDown : Model -> Model
handleDown game =
    let
        currentColumns =
            columns game

        grid' =
            List.map mergeCellsVertical currentColumns
                |> List.concat
                |> Dict.fromList
    in
        { game | grid = grid' }


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


columns : Model -> List (List ( Point, Int ))
columns game =
    game.grid
        |> Dict.toList
        |> List.sortBy (\cell -> fst (fst cell))
        |> List.Extra.groupWhile (\c1 c2 -> (fst (fst c1)) == (fst (fst c2)))
        |> List.map (List.sortBy (\cell -> fst (fst cell)))


rows : Model -> List (List ( Point, Int ))
rows game =
    game.grid
        |> Dict.toList
        |> List.sortBy (\cell -> snd (fst cell))
        |> List.Extra.groupWhile (\c1 c2 -> (snd (fst c1)) == (snd (fst c2)))
        |> List.map (List.sortBy (\cell -> fst (fst cell)))


addCell : Int -> Model -> Model
addCell randomIndex game =
    let
        point =
            availableCells game
                |> List.drop (randomIndex - 1)
                |> List.head
                |> Maybe.withDefault ( 0, 0 )

        grid' =
            Dict.insert point 2 game.grid
    in
        { game | grid = grid' }


availableCells : Model -> List Point
availableCells game =
    List.Extra.zip
        (List.repeat 4 [0..3] |> List.concat)
        (List.map (List.repeat 4) [0..3] |> List.concat)
        |> List.filter (\point -> not <| Dict.member point game.grid)
