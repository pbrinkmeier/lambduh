module Main exposing (main)

import Browser
import Model
import View

init : () -> (Model.Model, Cmd Model.Msg)
init () = (Model.initialModel, Cmd.none)


main = Browser.element
    { init = init
    , subscriptions = \_ -> Sub.none
    , update = \msg model -> (Model.update msg model, Cmd.none)
    , view = View.view
    }
