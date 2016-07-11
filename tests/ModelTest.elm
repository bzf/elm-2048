module ModelTest exposing (..)

import ElmTestBDDStyle exposing (..)
import ElmTest exposing (assert)
import Model
import Dict


tests : Test
tests =
    describe "ModelTest"
        [ describe
            "Model.new"
            [ it "returns a game with two 2s on the grid"
                (Model.new
                    |> (\model -> expect (Dict.size model.grid) toBe 2)
                )
            ]
        , describe
            "Model.addCell"
            [ it "adds the cell to the internal grid"
                (Model.new
                    |> Model.addCell 0
                    |> (\model -> expect (Dict.size model.grid) toBe 3)
                )
            ]
        , let
            model =
                Model.new

            availableCells =
                Model.availableCells model

            containsPoint =
                (\point -> not <| Dict.member point model.grid)
          in
            describe "Model.availableCells"
                [ it "returns all cells that are not already occupied"
                    (expect (List.all containsPoint availableCells) toBeTruthy)
                ]
        ]
