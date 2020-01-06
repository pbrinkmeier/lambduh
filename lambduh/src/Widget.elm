module Widget exposing (viewTitle, viewContent, Widget, initTerm, initTree, initConstraints, viewControls)

import Html exposing (Html, span, text, div, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Lambda
import LambdaTypes
import Msg exposing (Msg(..))

type alias Widget =
    { id : Int
    , inner : InnerWidget
    }

type InnerWidget
    = TermWidget Lambda.Term
    | TreeWidget LambdaTypes.Tree
    | ConstraintsWidget LambdaTypes.Constraints

title : InnerWidget -> String
title widget =
    case widget of
        TermWidget _ -> "Term"
        TreeWidget _ -> "Tree"
        ConstraintsWidget _ -> "Constraints"

possibleActions : Widget -> List (String, Msg)
possibleActions widget =
    let
        pas =
            case widget.inner of
                TermWidget term ->
                    [ ("+ eval", AddEvaluationWidget term)
                    , ("+ tree", AddTreeWidget term)
                    ]
                TreeWidget tree ->
                    [ ("+ constraints", AddConstraintsWidget tree)
                    ]
                ConstraintsWidget constraints ->
                    [ -- TODO
                    ]
    in
    List.map (\(l, f) -> (l, f widget.id)) pas

viewTitle : Widget -> List (Html Msg)
viewTitle widget =
    [ div [ class "ll-widget-title-text" ]
        [ span [ class "ll-widget-title-id" ] [ text <| zeroPad 2 <| String.fromInt widget.id ]
        , span [] [ text " :: " ]
        , span [ class "ll-widget-title-name" ] [ text <| title widget.inner ]
        ]
    , div [ class "ll-widget-title-remove", onClick <| RemoveWidget widget.id ]
        [ text "remove"
        ]
    ]

zeroPad i str =
    if String.length str < i then
        zeroPad (i - 1) ("0" ++ str)
    else
        str

viewContent : Widget -> List (Html a)
viewContent widget =
    case widget.inner of
        TermWidget term -> [ Lambda.viewTerm term ]
        TreeWidget tree -> [ LambdaTypes.viewTree tree ]
        ConstraintsWidget constraints -> [ LambdaTypes.viewConstraints constraints ]


-- This ugly stuff may be used in the future for FRP-kind stuff
-- Or maybe I'll delete it, idk
initTerm term id =
    Widget id <| TermWidget term

initTree tree id =
    Widget id <| TreeWidget tree

initConstraints constraints id =
    Widget id <| ConstraintsWidget constraints

viewControls : Widget -> List (Html Msg)
viewControls =
    let
        viewControl (label, msg) =
            div [ class "ll-widget-control" ]
                [ button [ class "ll-widget-control-button", onClick msg ] [ text label ]
                ]
    in
    List.map viewControl << possibleActions
