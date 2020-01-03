module LambdaParsers exposing (term)

import Char
import Lambda
import Parser exposing ((|=), (|.))
import Parser as P
import Set

term : P.Parser Lambda.Term
term =
    let
        terminal =
            P.oneOf
                [ abs
                , var
                , parenthesized
                ]

        abs =
            P.succeed Lambda.Abs
                |. P.symbol "\\"
                |= identifier
                |. P.symbol "."
                |= P.lazy (\_ -> term)

        var =
            P.map Lambda.Var identifier

        parenthesized =
            P.succeed (\x -> x)
                |. P.symbol "("
                |= P.lazy (\_ -> term)
                |. P.symbol ")"

        rest : Lambda.Term -> P.Parser Lambda.Term
        rest t =
            P.oneOf
                [ P.succeed (Lambda.App t)
                    |. P.symbol " "
                    |= terminal
                    |> P.andThen rest
                , P.succeed t
                ]

        identifier =
            P.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                -- TODO: add let and in
                , reserved = Set.empty
                }
    in
        terminal |> P.andThen rest
