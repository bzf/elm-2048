module GridTest exposing (..)

import ElmTestBDDStyle exposing (..)
import ElmTest exposing (assert)
import Grid
import Dict


tests : Test
tests =
    describe "GridTest"
        [ describe "Grid.columns"
            [ it "returns the columns sorted by their row value" <|
                expect (Grid.columns (Dict.fromList [ ( ( 0, 0 ), 2 ), ( ( 0, 1 ), 2 ), ( ( 0, 2 ), 4 ), ( ( 0, 3 ), 16 ), ( ( 1, 0 ), 2 ), ( ( 1, 2 ), 4 ), ( ( 1, 3 ), 16 ), ( ( 2, 2 ), 8 ), ( ( 2, 3 ), 16 ), ( ( 3, 2 ), 2 ), ( ( 3, 3 ), 8 ) ]))
                    toBe
                    [ [ ( ( 0, 0 ), 2 ), ( ( 0, 1 ), 2 ), ( ( 0, 2 ), 4 ), ( ( 0, 3 ), 16 ) ]
                    , [ ( ( 1, 0 ), 2 ), ( ( 1, 2 ), 4 ), ( ( 1, 3 ), 16 ) ]
                    , [ ( ( 2, 2 ), 8 ), ( ( 2, 3 ), 16 ) ]
                    , [ ( ( 3, 2 ), 2 ), ( ( 3, 3 ), 8 ) ]
                    ]
            ]
        , let
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
        ]
