module LambdaTypes exposing
    ( Tree
    , Type(..)
    , fv
    , TypeVariable
    , generateTree
    , viewTree
    , Constraints
    , Constraint
    , extractConstraints
    , viewConstraints
    )

import Html exposing (Html, div, span, text, table, tr, td)
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
    -- Contains the parameters name and it's type in the new context (redundantly stored in the Tree)
    | AbsNode String Type Tree
    | AppNode Tree Tree

type alias TypeContext = List (String, Type)

-- Types can be either variables, concrete types (e.g. int) or functions
type Type
    = TypeVar TypeVariable
    | FunctionType Type Type
    -- | ConcreteType String

type TypeVariable
    -- Numbered type variables are generated in type trees
    -- They are displayed as `\alpha_{i}`
    = Numbered Int
    -- Named variables are used to denote variables that belong to free variables in a term
    -- They are displayed as `\tau_{x}`, read: "Type of `x`"
    | Named String
    -- Type of a function that maps the left type to the right one

-- May contain duplicates (not relevant)
-- Reason: `Set` needs its elements to be comparable
fv : Type -> List TypeVariable
fv t =
    case t of
        TypeVar tv ->
            [ tv ]
        FunctionType a b ->
            (fv a) ++ (fv b)

viewType t =
    case t of
        TypeVar tv ->
            case tv of
                Numbered i ->
                    span [ class "type-var" ]
                        [ text "α"
                        , span [ class "type-var-id" ] [ text <| String.fromInt i ]
                        ]
                Named name ->
                    span [ class "type-var" ]
                        [ text "τ"
                        , span [ class "type-var-id" ] [ text name ]
                        ]
        FunctionType f x ->
            let
                fHtml =
                    case f of
                        FunctionType _ _ -> parenthesize <| [ viewType f ]
                        _                -> [ viewType f ]
                xHtml = [ viewType x ]
                parenthesize h =
                    [ text "(" ] ++ h ++ [ text ")" ]
            in
                span [ class "type-func" ]
                    <| fHtml ++ [ text " → " ] ++ xHtml

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
                currVar = TypeVar <| Numbered currVarId
                makeTree = Tree ctx term currVar
            in
            case term of
                Lambda.Var varName ->
                    let
                        t =
                            case ctxLookup varName ctx of
                                -- If the variable is free, give it a named type variable
                                Nothing -> TypeVar <| Named varName
                                -- Otherwise, use the one in the given context
                                Just x  -> x
                    in
                    (makeTree <| VarNode varName t, nextVarId)
                Lambda.Abs param body ->
                    let
                        -- Insert a new entry for "param" into the context
                        paramType = TypeVar <| Numbered nextVarId
                        newCtx = ctx ++ [(param, paramType)]
                        (bodyTree, restVar) = generateTreeN newCtx (nextVarId + 1) (nextVarId + 2) body
                    in
                        -- Cache the parameters type in the node for easier retrieval
                        (makeTree <| AbsNode param paramType bodyTree, restVar)
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
                    -- TODO: make ugly code at least a bit nicer
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
                AbsNode _ _ body ->
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
        AbsNode _ _ _ -> "Abs"
        AppNode _ _ -> "App"

---- constraints stuff ----

type alias Constraints = List Constraint

type alias Constraint =
    { lhs : Type
    , rhs : Type
    }

extractConstraints : Tree -> Constraints
extractConstraints { termType, inner } =
    case inner of
        VarNode _ typeInContext ->
            [ Constraint typeInContext termType ]
        AbsNode param paramType body ->
            [ Constraint termType <| FunctionType paramType body.termType ] ++ extractConstraints body
        AppNode fTree xTree ->
            [ Constraint fTree.termType <| FunctionType xTree.termType termType ] ++ extractConstraints fTree ++ extractConstraints xTree

viewConstraints : Constraints -> Html a
viewConstraints constraints =
    let
        viewConstraintCells { lhs, rhs } =
            [ td [ class "cr -lhs" ] [ viewType lhs ]
            , td [ class "cr -eq" ] [ text "=" ]
            , td [ class "cr -rhs" ] [ viewType rhs ]
            ]

        viewConstraint leadingSymbol constraint =
            tr [ class "constraint " ] <|
            [ td [ class "cr -begin" ] [ text leadingSymbol ] ] ++ viewConstraintCells constraint

        lastRow =
            tr [ class "constraint" ]
                [ td [ class "cr -begin" ] [ text "}" ]
                , td [ class "cr -lhs" ] []
                , td [ class "cr -eq" ] []
                , td [ class "cr -rhs" ] []
                ]
    in
        case constraints of
            -- Shouldn't be possible (it is of course if someone were to use this function manually)
            [] -> Debug.todo "You should not have done that!"
            (first :: rest) ->
                table [ class "constraints" ] <|
                    [ viewConstraint "{" first ] ++ (List.map (viewConstraint ",") rest) ++ [ lastRow ]
