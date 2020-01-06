module Widget exposing
    ( viewTitle
    , viewContent
    , Widget
    , initTerm
    , initTree
    , initConstraints
    , initUnification
    , viewControls
    , viewActions
    , setStepInUnificationWidget
    )

import Dict
import Html exposing (Html, span, text, div, button, pre)
import Html.Attributes exposing (class, disabled)
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
    | UnificationWidget Int LambdaTypes.History

title : InnerWidget -> String
title widget =
    case widget of
        TermWidget _ -> "Term"
        TreeWidget _ -> "Tree"
        ConstraintsWidget _ -> "Constraints"
        UnificationWidget _ _ -> "Unification"

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
                    [ ("+ unify", AddUnificationWidget constraints)
                    ]
                UnificationWidget _ _ ->
                    [
                    ]
    in
    List.map (\(l, f) -> (l, f widget.id)) pas

controls : Widget -> List (Html Msg)
controls widget =
    let
        buttonControl enabled label msg =
            button [ class "ll-widget-control-button", disabled <| not enabled, onClick msg ] [ text label ]

        hasKey key dict =
            case Dict.get key dict of
                Nothing -> False
                Just _ -> True
    in
        case widget.inner of
            UnificationWidget i history ->
                [ buttonControl (hasKey (i - 1) history) "←" (SetStep (i - 1) widget.id)
                , buttonControl (hasKey (i + 1) history) "→" (SetStep (i + 1) widget.id)
                ]
            _ ->
                []

setStepInUnificationWidget step widget =
    case widget.inner of
        UnificationWidget _ history ->
            { widget | inner = UnificationWidget step history }
        _ -> widget

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
    let
        columnContents : List (List (Html a))
        columnContents =
            case widget.inner of
                TermWidget term -> [ [ Lambda.viewTerm term ] ]
                TreeWidget tree -> [ [ LambdaTypes.viewTree tree ] ]
                ConstraintsWidget constraints -> [ [ LambdaTypes.viewConstraints constraints ] ]
                UnificationWidget current history ->
                    case Dict.get current history of
                        Nothing -> [ [ text "Nothing here :(" ] ]
                        Just historyEntry -> LambdaTypes.viewHistoryEntry historyEntry

        viewColumn : List (Html a) -> Html a
        viewColumn elements =
            div [ class "ll-widget-content-column" ] <|
                List.map (\e -> div [ class "ll-widget-content-wrapper" ] [e]) elements
    in
        List.map viewColumn columnContents

-- This ugly stuff may be used in the future for FRP-kind stuff
-- Or maybe I'll delete it, idk
initTerm term id =
    Widget id <| TermWidget term

initTree tree id =
    Widget id <| TreeWidget tree

initConstraints constraints id =
    Widget id <| ConstraintsWidget constraints

initUnification history id =
    Widget id <| UnificationWidget 0 history

viewControls : Widget -> List (Html Msg)
viewControls = controls

viewActions : Widget -> List (Html Msg)
viewActions =
    let
        viewAction (label, msg) =
            div [ class "ll-widget-action" ]
                [ button [ class "ll-widget-action-button", onClick msg ] [ text label ]
                ]
    in
    List.map viewAction << possibleActions
