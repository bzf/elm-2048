module Main exposing (..)

import ElmTest exposing (..)
import ModelTest exposing (..)
import GridTest exposing (..)


tests : Test
tests =
    suite "Test suite"
        [ ModelTest.tests
        , GridTest.tests
        ]


main : Program Never
main =
    runSuite tests
