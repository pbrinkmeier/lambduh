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
    , viewType
    , unifyStepByStep
    , History
    , viewHistoryEntry
    )

import Html exposing (Html, div, span, text, table, tr, td)
import Html.Attributes exposing (class, classList)
import Lambda
import Dict exposing (Dict)
import Set

type alias Tree =
    { context : TypeContext
    , term : Lambda.Term
    , termType : Type
    , inner : TreeNode
    }

type TreeNode
    = VarNode String Type
    -- Contains the parameters name and its type in the new context (redundantly stored in the Tree)
    | AbsNode String Type Tree
    | AppNode Tree Tree
    -- Caches C_let constraint list
    | LetNode (List Constraint) Tree Tree

type alias TypeContext = List (String, TypeSchema)

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
fv : TypeSchema -> List TypeVariable
fv (Quantified bound t) =
    case t of
        TypeVar tv ->
            if not <| List.member tv bound then
                [ tv ]
            else
                []
        FunctionType a b ->
            (fv <| Quantified bound a) ++ (fv <| Quantified bound b)

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
                        (t, restVarId) =
                            case ctxLookup varName ctx of
                                -- If the variable is free, give it a named type variable
                                Nothing -> (TypeVar <| Named varName, nextVarId)
                                -- Otherwise, instantiate the one in the given context
                                -- That is, create new variables for every bound variable in the type schema
                                Just x  -> instantiate nextVarId x
                    in
                    (makeTree <| VarNode varName t, restVarId)
                Lambda.Abs param body ->
                    let
                        -- Insert a new entry for "param" into the context
                        paramType = TypeVar <| Numbered nextVarId
                        newCtx = ctx ++ [(param, Quantified [] paramType)]
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
                Lambda.Let name value inTerm ->
                    let
                        (valueTree, sndNextVarId) = generateTreeN ctx nextVarId (nextVarId + 2) value
                        letConstraints = extractConstraints valueTree
                        -- todo: handle errors
                        letUnifier = Result.withDefault [] <| unify letConstraints
                        newContext = applyUnifierToContext letUnifier ctx
                        -- Bind all variables that are not in the new context
                        valueType = bindFreeVariables newContext <| applyUnifierToType letUnifier (TypeVar <| Numbered nextVarId)

                        newContextWithBinding = newContext ++ [ (name, valueType) ]
                        (inTermTree, restVarId) = generateTreeN newContextWithBinding (nextVarId + 1) (sndNextVarId) inTerm
                    in
                        -- TODO:
                        -- letConstraints has already been simplified by unification
                        -- use letUnifier in place of letConstraints
                        (makeTree <| LetNode letConstraints valueTree inTermTree, restVarId)
    in
        Tuple.first << generateTreeN [] 1 2

type TypeSchema = Quantified (List TypeVariable) Type

bindFreeVariables : TypeContext -> Type -> TypeSchema
bindFreeVariables ctx inType =
    let
        boundVars =
            -- is this correct? this marks any variable that ever appears in the context as bound
            -- also: can't use a set here because "mimimi cOmParAblE"
            Debug.log "bound"
            <| List.concat
            <| List.map (fv << Tuple.second) ctx

        distinct l =
            case l of
                [] -> []
                (first :: rest) ->
                    if List.member first rest then
                        distinct rest
                    else
                        first :: distinct rest
        
        freeVars t =
            case t of
                FunctionType f x ->
                    freeVars f ++ freeVars x
                TypeVar tv ->
                    if not <| List.member tv boundVars then
                        [ tv ]
                    else
                        []
    in
        Quantified (distinct <| freeVars inType) inType

instantiate : Int -> TypeSchema -> (Type, Int)
instantiate nextVarId (Quantified bound t) =
    let
        restVarId = nextVarId + List.length bound
        unifier = List.indexedMap (\i tv -> Substitution tv (TypeVar <| Numbered <| nextVarId + i)) bound
    in
        (applyUnifierToType unifier t, restVarId)

viewTree : Tree -> Html a
viewTree { context, term, termType, inner } =
    let
        viewContext =
            let
                viewAss (var, t) =
                    [ span [ class "lambda-id" ] [ text var ]
                    , text " : "
                    ] ++ viewTypeSchema t

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
                              , text ") ⪰ "
                              , viewType lookupResult
                              ]
                            ]
                AbsNode _ _ body ->
                    [ viewTree body ]
                AppNode lhs rhs ->
                    [ viewTree lhs, viewTree rhs ]
                LetNode _ value inTerm ->
                    [ viewTree value, viewTree inTerm ]
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

viewTypeSchema (Quantified bound t) =
    let
        viewQuantor typeVar =
            [ text "∀"
            , viewType <| TypeVar typeVar
            , text "."
            ]
    in
    List.concat <| List.map viewQuantor bound ++ [ [ viewType t ] ]

ruleName node =
    case node of
        VarNode _ _ -> "Var"
        AbsNode _ _ _ -> "Abs"
        AppNode _ _ -> "App"
        LetNode _ _ _ -> "Let"

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
        LetNode cLet _ inTermTree ->
            [ Constraint termType inTermTree.termType ] ++ cLet ++ extractConstraints inTermTree

viewConstraints : Constraints -> Html a
viewConstraints constraints =
    let
        viewConstraint leadingSymbol { lhs, rhs } =
            tr [ class "constraint " ]
                [ td [ class "constraint-lead" ] [ text leadingSymbol ]
                , td [ class "constraint-lhs" ] [ viewType lhs ]
                , td [ class "constraint-eq" ] [ text "=" ]
                , td [ class "constraint-rhs" ] [ viewType rhs ]
                ]

        emptyRow symbol =
            tr [ class "constraint" ]
                [ td [ class "constraint-lead" ] [ text symbol ]
                , td [ class "constraint-lhs" ] []
                , td [ class "constraint-eq" ] []
                , td [ class "constraint-rhs" ] []
                ]
    in
        case constraints of
            [] -> table [ class "constraints" ] [ emptyRow "{", emptyRow "}" ]
            (first :: rest) ->
                table [ class "constraints" ] <|
                    [ viewConstraint "{" first ] ++ (List.map (viewConstraint ",") rest) ++ [ emptyRow "}" ]

---- unification stuff ----

type alias History = Dict Int HistoryEntry

type alias HistoryEntry =
    -- TODO: refactor this into its own sum type
    -- This is an kinda okay type because
    -- * Only one substitution is ever generated by a single unification step (?)
    -- * New substitutions are always added at the "beginning" of the unifier
    Result (Type, Type, List Constraint)
        { constraints : List Constraint
        , newSubstitution : Maybe Substitution
        , substitutions : List Substitution
        }

type alias Substitution =
    { replace : TypeVariable
    , by : Type
    }

unifyStepByStep : List Constraint -> History
unifyStepByStep =
    let
        unifyStepByStepAcc : List Substitution -> List Constraint -> List HistoryEntry
        unifyStepByStepAcc subs constraints =
            case constraints of
                [] ->
                    [ Ok { constraints = [], newSubstitution = Nothing, substitutions = subs } ]
                nonEmpty ->
                    case unifyStep constraints of
                        Err (lhs, rhs) ->
                            [ Err (lhs, rhs, constraints) ]
                        Ok (remainingConstraints, newSubst) ->
                            [ Ok { constraints = constraints, newSubstitution = newSubst, substitutions = subs } ] ++ unifyStepByStepAcc (maybeToList newSubst ++ subs) remainingConstraints
                        

        maybeToList m =
            case m of
                Just x -> [x]
                Nothing -> []
    in
        Dict.fromList << List.indexedMap Tuple.pair << unifyStepByStepAcc []

applySubstitution : Substitution -> TypeSchema -> TypeSchema
applySubstitution s (Quantified bound t) =
    let
        { replace, by } = s

    in
    case t of
        TypeVar tv ->
            if tv == replace && (not <| List.member tv bound) then
                Quantified bound by
            else
                Quantified bound t
        FunctionType f x ->
            Quantified bound <| FunctionType
                (getT <| applySubstitution s <| Quantified bound f)
                (getT <| applySubstitution s <| Quantified bound x)

getT (Quantified _ t2) = t2

applyUnifierToTypeSchema : List Substitution -> TypeSchema -> TypeSchema
applyUnifierToTypeSchema u ts =
    List.foldr applySubstitution ts u

applyUnifierToType : List Substitution -> Type -> Type
applyUnifierToType u t =
    getT <| applyUnifierToTypeSchema u <| Quantified [] t

applyUnifierToContext : List Substitution -> TypeContext -> TypeContext
applyUnifierToContext u =
    List.map (Tuple.mapSecond <| applyUnifierToTypeSchema u)

applyUnifierToConstraints : List Substitution -> List Constraint -> List Constraint
applyUnifierToConstraints u =
    List.map (\{ lhs, rhs } -> { lhs = applyUnifierToType u lhs, rhs = applyUnifierToType u rhs })
        
unify : List Constraint -> Result (Type, Type) (List Substitution)
unify =
    let
        unifyAcc subs constraints =
            case unifyStep constraints of
                Err e -> Err e
                Ok ([], newSub) -> Ok <| maybeToList newSub ++ subs
                Ok (rest, newSub) ->
                    unifyAcc (maybeToList newSub ++ subs) rest

        maybeToList m =
            case m of
                Just x -> [x]
                Nothing -> []

    in
        unifyAcc []

unifyStep : List Constraint -> Result (Type, Type) (List Constraint, Maybe Substitution)
unifyStep constraints =
    let
        -- stupid hack that is necessary because Elms pattern matching lacks guards
        -- TODO: forum/reddit, clean up this mess
        unifyLeftRightRest : Type -> Type -> List Constraint -> Result (Type, Type) (List Constraint, Maybe Substitution)
        unifyLeftRightRest lhs rhs rest =
            let
                lhsEqualsRhs =
                    if lhs == rhs then
                        Just (rest, Nothing)
                    else
                        Nothing

                varAndOccursCheck a b =
                    case a of
                        TypeVar tv -> 
                            if not <| List.member tv <| fv <| Quantified [] b then
                                Just (applyUnifierToConstraints [ Substitution tv b ] rest, Just <| Substitution tv b)
                            else
                                Nothing
                        _ ->
                            Nothing

                -- make function of a and b for "laziness" ~> better perf?
                functionTypes =
                    case (lhs, rhs) of
                        (FunctionType f1 x1, FunctionType f2 x2) ->
                            Just (rest ++ [ Constraint f1 f2, Constraint x1 x2 ], Nothing)
                        _ ->
                            Nothing
                
                -- TODO: doc
                orMaybe : Maybe a -> Maybe a -> Maybe a
                orMaybe m1 m2 =
                    -- TODO: Is `if` needed here for laziness?
                    -- Is `case` sufficient?
                    if m2 /= Nothing then
                        m2
                    else
                        m1
            in
            lhsEqualsRhs
            |> orMaybe (varAndOccursCheck lhs rhs)
            |> orMaybe (varAndOccursCheck rhs lhs)
            |> orMaybe functionTypes
            |> Result.fromMaybe (lhs, rhs)
    in
    case constraints of
        [] -> Ok ([], Nothing)
        ({ lhs, rhs } :: rest) -> unifyLeftRightRest lhs rhs rest

viewHistoryEntry : HistoryEntry -> List (List (Html a))
viewHistoryEntry entry =
    let
        constraints =
            case entry of
                Ok e -> e.constraints
                Err (_, _, c) -> c

        viewSubstitutionsOrErrorMsg =
            case entry of
                Err (lhs, rhs, _) ->
                    div []
                        [ viewType lhs
                        , text " is not unifiable with "
                        , viewType rhs
                        ]
                Ok { substitutions, newSubstitution } ->
                    table [ class "substitutions" ] <|
                        case (newSubstitution, substitutions) of
                            (Just first, rest) ->
                                [ viewSubstitution "[" True first ] ++ List.map (viewSubstitution "," False) rest ++ [ emptyRow "]" ]
                            (Nothing, first :: rest) ->
                                [ viewSubstitution "[" False first ] ++ List.map (viewSubstitution "," False) rest ++ [ emptyRow "]" ]
                            (Nothing, []) ->
                                -- shouldn't ever need to be display, first constraint is never "func = func"
                                [ emptyRow "[" ] ++ [ emptyRow "]" ]
         
        viewSubstitution symbol isNew { replace, by } =
            tr [ classList [ ("substitution", True), ("-new", isNew) ] ]
                [ td [ class "substitution-lead" ] [ text symbol ]
                , td [ class "substitution-lhs" ] [ viewType <| TypeVar replace ]
                , td [ class "substitution-arrow" ] [ text "⇨" ]
                , td [ class "substitution-rhs" ] [ viewType by ]
                ]
        
        emptyRow symbol =
            tr [ class "substitution" ]
                [ td [ class "substitution-lead" ] [ text symbol ]
                , td [ class "substitution-lhs" ] []
                , td [ class "substitution-arrow" ] []
                , td [ class "substitution-rhs" ] []
                ]

        viewSubstitutedType tv =
            case entry of
                Ok { substitutions } ->
                    [ div []
                        [ viewType tv
                        , text " = "
                        , viewType <| applyUnifierToType substitutions tv
                        ]
                    ]
                Err _ ->
                    []

    in
        [ [ viewConstraints constraints ]
        , [ viewSubstitutionsOrErrorMsg
          , div [] <| viewSubstitutedType (TypeVar <| Numbered 1)
          ]
        ]
