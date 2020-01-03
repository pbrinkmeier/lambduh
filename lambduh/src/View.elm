module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LambdaParsers
import Model exposing (Msg(..))
import Parser

view model =
    div []
        [ input [ onInput SetSource ] []
        , p [] [ text model.source ]
        , p [] [ text <| Debug.toString <| Parser.run LambdaParsers.term model.source ]
        ]
