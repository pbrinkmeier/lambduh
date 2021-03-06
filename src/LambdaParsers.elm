module LambdaParsers exposing (term)

import Char
import Lambda
import Parser exposing ((|=), (|.))
import Parser as P
import Set

{-
A parser for lambda-terms implemented using the elm/parser parser-combinator.
-}
term : P.Parser Lambda.Term
term =
    let
        -- "terminal" includes anything but function application
        -- function application needs to be handled with care because of its
        -- left-associativity
        -- TODO: add const terms
        terminal =
            P.oneOf
                [ abs
                , var
                , parenthesized
                , letTerm
                ]

        -- abstraction terms start with a backslash or a lambda, followed by
        -- an identifier ("parameter"), a dot, and a lambda-term ("body")
        abs =
            P.succeed Lambda.Abs
                |. P.oneOf [ P.symbol "\\", P.symbol "λ" ]
                |= identifier
                |. P.symbol "."
                |= P.lazy (\_ -> term)

        -- variables are just identifiers
        var =
            P.map Lambda.Var identifier

        -- literally just a lambda-term enclosed in parentheses
        parenthesized =
            P.succeed (\x -> x)
                |. P.symbol "("
                |= P.lazy (\_ -> term)
                |. P.symbol ")"

        letTerm =
            P.succeed Lambda.Let
                |. P.keyword "let"
                |. P.symbol " "
                |= identifier
                |. P.symbol " "
                |. P.symbol "="
                |. P.symbol " "
                |= P.lazy (\_ -> term)
                |. P.symbol " "
                |. P.keyword "in"
                |. P.symbol " "
                |= P.lazy (\_ -> term)

        -- parses a trailing function application
        -- if the source doesn't start with a space, return the term
        -- otherwise consume the space and *the next terminal*
        -- this kind of greedy behavior is needed to ensure the correct tree structure
        -- function application is left-associative, which means:
        -- f x x x = ((f x) x) x
        rest : Lambda.Term -> P.Parser Lambda.Term
        rest t =
            P.oneOf
                [ P.succeed (Lambda.App t)
                    -- even if the space was consumed, we can still try the next case
                    -- (if no terminal could be parsed)
                    -- this is the case for e.g. let x = y in x
                    -- in is a keyword ~> it can't be a variable
                    |. P.backtrackable (P.symbol " ")
                    |= terminal
                    |> P.andThen rest
                , P.succeed t
                ]

        -- these will usually just be a single character, but we'll allow longer
        -- identifiers just in case
        identifier =
            P.variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.fromList [ "let", "in" ]
                }
    in
        terminal |> P.andThen rest
