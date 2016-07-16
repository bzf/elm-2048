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
        ]
