module Lambda exposing (Term(..), viewTerm)

import Html exposing (Html, text, span)
import Html.Attributes exposing (class)

type Term
    = Var String
    | Abs String Term
    | App Term Term
    | Let String Term Term

viewTerm : Term -> Html a
viewTerm =
    let
        viewTermList term =
            case term of
                Var varName ->
                    [ span [ class "lambda-id" ] [ text varName ]
                    ]
                Abs param body ->
                    [ text "λ"
                    , span [ class "lambda-id" ] [ text param ]
                    , text "."
                    ] ++ viewTermList body
                App f x ->
                    let
                        lhs =
                            case f of
                                Abs _ _ -> parenthesize <| viewTermList f
                                Let _ _ _ -> parenthesize <| viewTermList f
                                _       -> viewTermList f
                        rhs =
                            case x of
                                App _ _ -> parenthesize <| viewTermList x
                                -- TODO: these parens are not always necessary
                                -- e.g. necessary: (g (\x.x)) f = g (\x.x) f
                                -- not necessary: g \x.x 
                                Abs _ _ -> parenthesize <| viewTermList x
                                Let _ _ _ -> parenthesize <| viewTermList x
                                _ ->       viewTermList x
                        
                        parenthesize h =
                            [ text "(" ] ++ h ++ [ text ")" ]
                    in
                        lhs ++ [ text " " ] ++ rhs
                Let name value inTerm ->
                    [ text "let ", span [ class "lambda-id" ] [ text name ], text " = " ] ++ viewTermList value ++ [ text " in " ] ++ viewTermList inTerm
    in
    span [ class "lambda" ] << viewTermList
