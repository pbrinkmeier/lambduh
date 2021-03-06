module View exposing (view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LambdaParsers
import Msg exposing (Msg(..))
import Parser
import Widget

view model =
    div [ class "ll" ]
        [ div [ class "ll-topbar" ]
            [ div [ class "container" ]
                [ h1 [ class "ll-topbar-title" ] [ text "lambduh" ]
                ]
            ]
        , div [ class "container ll-init" ]
            [ input [ onInput SetTermSource, value model.termSource, class "ll-init-input" ] []
            , button [ onClick TryAddTerm, class "ll-init-addterm btn" ] [ text "+ term" ]
            ]
        , div [ class "container ll-widgets" ] <| viewWidgets model
        ]

viewWidgets model =
    let
        viewWidget widget =
            div [ class "ll-widget" ] <| concatMaybes
                [ Just <| div [ class "ll-widget-title" ] (Widget.viewTitle widget)
                , div [ class "ll-widget-controls" ] |> notEmpty (Widget.viewControls widget)
                , Just <| div [ class "ll-widget-content" ] (Widget.viewContent widget)
                , div [ class "ll-widget-actions" ] |> notEmpty (Widget.viewActions widget)
                ]
         
        notEmpty list f =
            case list of
                [] -> Nothing
                _ -> Just <| f list

        concatMaybes = List.filterMap (\x -> x)
    in
    List.map viewWidget <| Dict.values model.widgets
