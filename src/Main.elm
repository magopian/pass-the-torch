module Main exposing (..)

import Html
import Model exposing (init, update, Model, Msg)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
