module LambdaTypes exposing
    ( Tree
    , generateTree
    , viewTree
    , Constraints
    , extractConstraints
    , viewConstraints
    )

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Lambda

type alias Tree =
    { context : TypeContext
    , term : Lambda.Term
    , termType : Type
    , inner : TreeNode
    }

type TreeNode
    = VarNode String Type
    | AbsNode String Tree
    | AppNode Tree Tree

type alias TypeContext = List (String, Type)

-- Types can be of different forms
type Type
    -- Numbered type variables are generated in type trees
    -- They are displayed as `\alpha_{i}`
    = NumberedTypeVariable Int
    -- Named variables are used to denote variables that belong to free variables in a term
    -- They are displayed as `\tau_{x}`, read: "Type of `x`"
    | NamedTypeVariable String
    -- TODO: const

viewType t =
    case t of
        NumberedTypeVariable i ->
            span [ class "type-var" ]
                [ text "α"
                , span [ class "type-var-id" ] [ text <| String.fromInt i ]
                ]
        NamedTypeVariable name ->
            span [ class "type-var" ]
                [ text "τ"
                , span [ class "type-var-id" ] [ text name ]
                ]

-- Look for the rightmost appearance of identifier "needle" and return its type
ctxLookup needle =
    let
        folder (varName, varType) acc =
            if varName == needle then
                Just varType
            else
                acc
    in
    List.foldl folder Nothing

generateTree =
    let
        -- currVarId: the variable number this term is supposed to be assigned
        -- nextVarId: the variable number to start indexing subtrees off
        -- this crime against humanity is needed to achieve "natural" numbering in App
        -- **m is not necessarily n + 1**
        -- returns next available variable in tuple (also for App, this is horrible)
        generateTreeN : TypeContext -> Int -> Int -> Lambda.Term -> (Tree, Int)
        generateTreeN ctx currVarId nextVarId term =
            let
                currVar = NumberedTypeVariable currVarId
                makeTree = Tree ctx term currVar
            in
            case term of
                Lambda.Var varName ->
                    let
                        t =
                            case ctxLookup varName ctx of
                                -- If the variable is free, give it a named type variable
                                Nothing -> NamedTypeVariable varName
                                -- Otherwise, use the one in the given context
                                Just x  -> x
                    in
                    (makeTree <| VarNode varName t, nextVarId)
                Lambda.Abs param body ->
                    let
                        -- Insert a new entry for "param" into the context
                        newCtx = ctx ++ [(param, NumberedTypeVariable nextVarId)]
                        (bodyTree, restVar) = generateTreeN newCtx (nextVarId + 1) (nextVarId + 2) body
                    in
                        (makeTree <| AbsNode param bodyTree, restVar)
                Lambda.App f x ->
                    let
                        (fTree, sndNextVarId) = generateTreeN ctx nextVarId (nextVarId + 2) f
                        (xTree, restVarId) = generateTreeN ctx (nextVarId + 1) sndNextVarId x
                    in
                        (makeTree <| AppNode fTree xTree, restVarId)
    in
        Tuple.first << generateTreeN [] 1 2

viewTree : Tree -> Html a
viewTree { context, term, termType, inner } =
    let
        viewContext =
            let
                viewAss (var, t) =
                    [ span [ class "lambda-id" ] [ text var ]
                    , text " : "
                    , viewType t
                    ]
            in
            List.concat << List.intersperse ([text ", "]) << List.map viewAss

        viewRuleReqs node =
            case node of
                VarNode varName lookupResult ->
                    -- todo
                    List.singleton <| span []
                        <| List.concat
                        <|
                            [ [ text "(" ]
                            , viewContext context
                            , [ text ")("
                              , span [ class "lambda-id" ] [ text varName ]
                              , text ") = "
                              , viewType lookupResult
                              ]
                            ]
                AbsNode _ body ->
                    [ viewTree body ]
                AppNode lhs rhs ->
                    [ viewTree lhs, viewTree rhs ]
    in
        div [ class "rule" ]
            [ div [ class "rule-contents" ]
                [ div [ class "rule-reqs" ] <| List.map (div [ class "rule-req" ] << List.singleton) <| viewRuleReqs inner
                , div [ class "rule-conc" ]
                    [ span [] <| viewContext context
                    , text " ⊢ "
                    , Lambda.viewTerm term
                    , text " : "
                    , viewType termType
                    ]
                ]
            , div [ class "rule-name" ] [ text <| ruleName inner ]
            ]

ruleName node =
    case node of
        VarNode _ _ -> "Var"
        AbsNode _ _ -> "Abs"
        AppNode _ _ -> "App"

---- constraints stuff ----

type alias Constraints = List Constraint

type alias Constraint =
    { lhs : Type
    , rhs : Type
    }

extractConstraints : Tree -> Constraints
extractConstraints _ = Debug.todo "Nothing here :("

viewConstraints : Constraints -> Html a
viewConstraints _ = Debug.todo "I N V I S I B L E"
