module GridTest exposing (..)

import ElmTestBDDStyle exposing (..)
import ElmTest exposing (assert)
import Grid
import Dict


buildCell : ( Int, Int ) -> Int -> ( ( Int, Int ), { value : Int } )
buildCell point x =
    ( point, { value = x } )


tests : Test
tests =
    describe "GridTest"
        [ let
            grid =
                Grid.new

            availableCells =
                Grid.availableCells grid

            doesNotContainPoint =
                (\point -> not <| Dict.member point grid)
          in
            describe "Grid.availableCells"
                [ it "returns all cells that are not already occupied"
                    (expect (List.all doesNotContainPoint availableCells) toBeTruthy)
                ]
        , describe "Grid.isGameOver"
            [ let
                grid =
                    Dict.fromList [ ( ( 0, 0 ), { value = 2 } ), ( ( 0, 1 ), { value = 4 } ), ( ( 0, 2 ), { value = 32 } ), ( ( 0, 3 ), { value = 4 } ), ( ( 1, 0 ), { value = 8 } ), ( ( 1, 1 ), { value = 128 } ), ( ( 1, 2 ), { value = 4 } ), ( ( 1, 3 ), { value = 8 } ), ( ( 2, 0 ), { value = 4 } ), ( ( 2, 1 ), { value = 64 } ), ( ( 2, 2 ), { value = 8 } ), ( ( 2, 3 ), { value = 32 } ), ( ( 3, 0 ), { value = 2 } ), ( ( 3, 1 ), { value = 8 } ), ( ( 3, 2 ), { value = 256 } ), ( ( 3, 3 ), { value = 2 } ) ]
              in
                it "returns True when there are no possible moves left"
                    <| expect (Grid.isGameOver grid) toBe True
            , let
                grid =
                    Dict.fromList [ ( ( 0, 0 ), { value = 2 } ), ( ( 0, 1 ), { value = 4 } ), ( ( 0, 2 ), { value = 32 } ), ( ( 0, 3 ), { value = 4 } ), ( ( 1, 0 ), { value = 8 } ), ( ( 1, 1 ), { value = 128 } ), ( ( 1, 2 ), { value = 4 } ), ( ( 1, 3 ), { value = 8 } ), ( ( 2, 0 ), { value = 4 } ), ( ( 2, 1 ), { value = 64 } ), ( ( 2, 2 ), { value = 8 } ), ( ( 2, 3 ), { value = 32 } ), ( ( 3, 0 ), { value = 2 } ), ( ( 3, 1 ), { value = 8 } ), ( ( 3, 2 ), { value = 2 } ), ( ( 3, 3 ), { value = 2 } ) ]
              in
                it "returns False when there are possible moves left"
                    <| expect (Grid.isGameOver grid) toBe False
            , let
                grid =
                    Dict.fromList [ ( ( 0, 0 ), { value = 128 } ), ( ( 0, 1 ), { value = 64 } ), ( ( 0, 2 ), { value = 32 } ), ( ( 0, 3 ), { value = 16 } ), ( ( 1, 0 ), { value = 64 } ), ( ( 1, 1 ), { value = 32 } ), ( ( 1, 2 ), { value = 16 } ), ( ( 1, 3 ), { value = 8 } ), ( ( 2, 0 ), { value = 32 } ), ( ( 2, 1 ), { value = 16 } ), ( ( 2, 2 ), { value = 8 } ), ( ( 2, 3 ), { value = 2 } ), ( ( 3, 0 ), { value = 16 } ), ( ( 3, 1 ), { value = 8 } ), ( ( 3, 2 ), { value = 4 } ), ( ( 3, 3 ), { value = 2 } ) ]
              in
                it "should return False for this grid"
                    <| expect (Grid.isGameOver grid) toBe False
            , let
                grid =
                    Dict.fromList [ ( ( 0, 0 ), { value = 128 } ), ( ( 0, 1 ), { value = 64 } ), ( ( 0, 2 ), { value = 32 } ), ( ( 0, 3 ), { value = 16 } ), ( ( 1, 0 ), { value = 64 } ), ( ( 1, 1 ), { value = 32 } ), ( ( 1, 2 ), { value = 16 } ), ( ( 1, 3 ), { value = 8 } ), ( ( 2, 0 ), { value = 32 } ), ( ( 2, 1 ), { value = 16 } ), ( ( 2, 2 ), { value = 8 } ), ( ( 2, 3 ), { value = 4 } ), ( ( 3, 0 ), { value = 16 } ), ( ( 3, 1 ), { value = 8 } ), ( ( 3, 2 ), { value = 4 } ), ( ( 3, 3 ), { value = 2 } ) ]
              in
                it "should return True for this grid"
                    <| expect (Grid.isGameOver grid) toBe True
            ]
        ]
