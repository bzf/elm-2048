module Main exposing (..)

import ElmTest exposing (..)
import ModelTest exposing (..)


tests : Test
tests =
    ModelTest.tests


main : Program Never
main =
    runSuite tests
