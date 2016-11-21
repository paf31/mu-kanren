module Main where

import Prelude
import Kanren.Var as Var
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Array (fromFoldable, replicate)
import Data.Either (Either(..))
import Data.List (List(..), intercalate, length, singleton, sortBy, zip, (..), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Nullable (toMaybe)
import Data.String (fromCharArray, joinWith)
import Data.Tuple (Tuple(..), fst)
import Kanren.Eval (builtIn, replaceAll)
import Kanren.Goal (Define, Goal(..))
import Kanren.Obj (Obj(..))
import Kanren.Parser (parseDefines, parseGoal)
import Kanren.State (State(..), pushGoal, pushUnsolvedGoals, stateGoal, stateHistory, stateStack, stateSubst, stateVar, unwindStack)
import Kanren.Subst (walk)
import Kanren.Term (Term(..))
import Kanren.Unify (unify)
import Partial.Unsafe (unsafePartial)
import React (ReactElement)
import Unsafe.Coerce (unsafeCoerce)

type AppState =
  { error       :: Maybe String
  , editing     :: Boolean
  , code        :: String
  , defnsCode   :: String
  , state       :: State
  , definitions :: List Define
  }

initialState :: AppState
initialState =
  { error:   Nothing
  , editing: true
  , code: joinWith "\n"
            [ "(fresh xs ys"
            , "  (appendo xs ys (a (b (c nil))))"
            , ")"
            ]
  , defnsCode: joinWith "\n"
            [ "(define appendo xs ys zs"
            , "  (disj (conj (= xs nil)"
            , "          (= ys zs)"
            , "        )"
            , "        (fresh x xs' res"
            , "          (conj (= xs (x xs'))"
            , "                (appendo xs' ys res)"
            , "                (= zs (x res))"
            , "          )"
            , "        )"
            , "  )"
            , ")"
            ]
  , state: State Done Nil Var.zero Nil Nil
  , definitions: Nil
  }

data Action
  = UpdateCode String
  | UpdateDefinitions String
  | ToggleEditing
  | GoToState State

performAction :: forall eff props. T.PerformAction eff AppState props Action
performAction action _ _ = void $ T.modifyState \currentState ->
  case action of
    UpdateCode s        -> currentState { code = s }
    UpdateDefinitions s -> currentState { defnsCode = s }
    ToggleEditing
      | currentState.editing -> run currentState
      | otherwise -> currentState { editing = true }
    GoToState state     -> currentState { state = state }

run :: AppState -> AppState
run state =
  case Tuple <$> parseGoal state.code <*> parseDefines state.defnsCode of
    Left err ->
      state { error = Just err }
    Right (Tuple goal defines) ->
      state { error       = Nothing
            , editing     = false
            , state       = State goal Nil Var.zero Nil Nil
            , definitions = defines
            }

render :: forall a. T.Render AppState a Action
render send _ state _ =
    [ RD.div [ RP.className "row" ]
             [ RD.div [ RP.className "col-xs-12" ]
                      [ RD.h2' [ RD.text "mu-kanren" ]
                      , RD.p' [ RD.text "A step-by-step evaluator for a dialect of "
                              , RD.a [ RP.href "https://github.com/jasonhemann/microKanren" ]
                                     [ RD.text "microKanren" ]
                              ]
                      ]
             ]
    , RD.div [ RP.className "row" ]
             [ RD.div [ RP.className "col-xs-12" ] [ navBar ] ]
    , RD.div [ RP.className "row" ]
             [ leftColumn
             , rightColumn
             ]
    , RD.div [ RP.className "row" ]
             [ RD.div [ RP.className "col-xs-12" ]
                      [ RD.small' [ RD.text "Made by "
                                  , RD.a [ RP.href "http://twitter.com/paf31" ]
                                         [ RD.text "@paf31" ]
                                  , RD.text " using "
                                  , RD.a [ RP.href "http://purescript.org" ]
                                         [ RD.text "PureScript" ]
                                  , RD.text "."
                                  ]
                     ]
             ]
    ]
  where
    rightColumn :: ReactElement
    rightColumn | state.editing = RD.div' []
                | otherwise =
      RD.div [ RP.className "col-xs-6" ]
             [ substPane
             , remainingGoalPane
             , stackPane
             ]

    substPane :: ReactElement
    substPane =
        RD.div [ RP.className "panel panel-default"]
               [ RD.div [ RP.className "panel-heading"]
                        [ RD.text "Substitution" ]
               , RD.table [ RP.className "table table-condensed"]
                          [ RD.thead' [ RD.tr' [ RD.th' [ RD.text "Unknown" ]
                                               , RD.th' [ RD.text "Term" ]
                                               ]
                                      ]
                          , RD.tbody' (map toRow (fromFoldable (sortBy (comparing fst) (stateSubst state.state))))
                          ]
               ]
      where
        toRow :: Tuple Var.Var Term -> ReactElement
        toRow (Tuple (Var.Var nm) tm) =
          RD.tr' [ RD.td' [ RD.code' [ RD.text ("#" <> show nm) ] ]
                 , RD.td' [ RD.code' [ RD.text (renderTerm (walk (stateSubst state.state) tm)) ] ]
                 ]

    remainingGoalPane :: ReactElement
    remainingGoalPane =
        RD.div [ RP.className "panel panel-default"]
               [ RD.div [ RP.className "panel-heading"]
                        [ RD.text "Remaining Goals" ]
               , RD.table [ RP.className "table table-condensed"]
                          [ RD.tbody' (fromFoldable (map toRow (stateStack state.state)))
                          ]
               ]
      where
        toRow :: Goal -> ReactElement
        toRow g =
          RD.tr' [ RD.td' [ RD.code' [ RD.text (renderShortGoal g) ] ] ]

    stackPane :: ReactElement
    stackPane =
        RD.div [ RP.className "panel panel-default"]
               [ RD.div [ RP.className "panel-heading"]
                        [ RD.text "Execution Trace" ]
               , RD.table [ RP.className "table table-condensed"]
                          [ RD.tbody' (fromFoldable (map toRow (stateHistory state.state)))
                          ]
               ]
      where
        toRow :: State -> ReactElement
        toRow s =
          RD.tr' [ RD.td' [ RD.a [ RP.href "#"
                                 , RP.onClick \_ -> send (GoToState s)
                                 ]
                                 [ RD.code' [ RD.text (renderShortGoal (stateGoal s)) ] ] ]
                 ]

    renderShortGoal :: Goal -> String
    renderShortGoal Done = "Done"
    renderShortGoal Fail = "Fail"
    renderShortGoal (Fresh _ _) = "fresh"
    renderShortGoal (Unify _ _) = "unify"
    renderShortGoal (Disj _) = "disj"
    renderShortGoal (Conj _) = "conj"
    renderShortGoal (Named name _) = name

    spaces :: Int -> String
    spaces n = fromCharArray (replicate n ' ')

    inContext :: forall x. List x -> List (Tuple x (List x))
    inContext = go Nil Nil
      where
      go acc _  Nil      = acc
      go acc ys (x : xs) = go (Tuple x (ys <> xs) : acc) (ys <> singleton x) xs

    leftColumn :: ReactElement
    leftColumn =
      RD.div [ RP.className (if state.editing then "col-xs-12" else "col-xs-6") ]
             [ goalPane
             , editorPane
             , errorPane
             ]

    navBar :: ReactElement
    navBar =
      RD.ul [ RP.className "nav nav-pills" ]
            [ RD.li [ RP.role "presentation" ]
                    [ RD.a [ RP.href "#"
                           , RP.onClick \_ -> send ToggleEditing
                           ]
                           [ RD.span [ RP.className (if state.editing then "fa fa-play" else "fa fa-edit") ] []
                           , RD.text (if state.editing then " Run" else " Edit")
                           ]
                    ]
            , RD.li [ RP.role "presentation" ]
                    [ RD.a [ RP.href "https://github.com/functorial/mu-kanren/blob/gh-pages/README.md"
                           , RP.target "_blank"
                           ]
                           [ RD.span [ RP.className "fa fa-question" ] []
                           , RD.text " Help"
                           ]
                    ]
            ]

    goalPane :: ReactElement
    goalPane | state.editing = RD.div' []
             | otherwise =
        RD.div [ RP.className "panel panel-default"]
               [ RD.div [ RP.className "panel-heading"]
                        [ RD.text "Current Goal" ]

               , RD.div [ RP.className "lines" ]
                        (renderGoal true (stateGoal state.state))
               ]
      where
        renderGoal :: Boolean -> Goal -> Array ReactElement
        renderGoal _           Done = [ RD.p' [ RD.text "Evaluation complete" ] ]
        renderGoal _           Fail = [ RD.p' [ RD.text "Contradiction!" ] ]
        renderGoal renderLinks (Fresh ns g) =
          let curVar = stateVar state.state
              freshNames = TmVar <<< Var.Var <$> (Var.runVar curVar .. (Var.runVar curVar + length ns - 1))
              newState = State (replaceAll (zip ns freshNames) g)
                               (stateSubst state.state)
                               (Var.Var (Var.runVar curVar + length ns))
                               (stateStack state.state)
                               (state.state : stateHistory state.state)
          in [ line (link renderLinks (GoToState newState) [ RD.text ("(fresh " <> intercalate " " ns <> "") ])
             , indented (renderGoal false g)
             , line (RD.text ")")
             ]
        renderGoal renderLinks (Unify u v) =
          let subst = stateSubst state.state
              var = stateVar state.state
              rgs = stateStack state.state
              hist = stateHistory state.state
              text = "(= " <> renderTerm u <> " " <> renderTerm v <> ")"
              nextState = case unify u v subst of
                Nothing -> State Fail subst var rgs (state.state : hist)
                Just subst' -> unwindStack $ State Done subst' var rgs (state.state : hist)
          in [ line (link renderLinks (GoToState nextState) [ RD.text text ]) ]
        renderGoal renderLinks (Disj gs) =
            [ line (RD.text "(disj")
            , RD.div' (fromFoldable (map toLink gs))
            , line (RD.text ")")
            ]
          where
            toLink g =
              indented
                [ link renderLinks
                    (GoToState (unwindStack (pushGoal g state.state)))
                    (renderGoal false g)
                ]
        renderGoal renderLinks (Conj gs) =
            [ line (RD.text "(conj")
            , RD.div' (fromFoldable (map toLink (inContext gs)))
            , line (RD.text ")")
            ]
          where
            toLink (Tuple g rest) =
              indented
                [ link renderLinks
                    (GoToState (unwindStack (pushUnsolvedGoals rest (pushGoal g state.state))))
                    (renderGoal false g)
                ]
        renderGoal renderLinks (Named nm ts) = do
          case builtIn state.definitions nm ts of
            Left err -> [ RD.p' [ RD.text err ] ]
            Right newGoal ->
              let text = "(" <> nm <> " " <> intercalate " " (map renderTerm ts) <> ")"
                  newState = pushGoal newGoal state.state
              in [ line (link renderLinks (GoToState newState) [ RD.text text ]) ]

        link :: Boolean -> Action -> Array ReactElement -> ReactElement
        link true action = RD.a [ RP.href "#"
                                , RP.onClick \_ -> send action
                                ]
        link false _ = RD.span'

        indented :: Array ReactElement -> ReactElement
        indented xs = RD.div [ RP.className "indented" ] xs

        line :: ReactElement -> ReactElement
        line x = RD.div [ RP.className "line" ] [ x ]

    renderTerm :: Term -> String
    renderTerm (TmVar (Var.Var v)) = "#" <> show v
    renderTerm (TmObj (Obj o)) = o
    renderTerm (TmPair t1 t2) = "(" <> renderTerm t1 <> " " <> renderTerm t2 <> ")"

    editorPane :: ReactElement
    editorPane | not state.editing = RD.div' []
               | otherwise =
      RD.div' [ RD.div [ RP.className "panel panel-default"]
                       [ RD.div [ RP.className "panel-heading"]
                                [ RD.text "Edit Goal" ]

                       , RD.textarea [ RP.value state.code
                                     , RP.onChange (send <<< UpdateCode <<< _.target.value <<< unsafeCoerce)
                                     ] []
                       ]
              , RD.div [ RP.className "panel panel-default"]
                       [ RD.div [ RP.className "panel-heading"]
                                [ RD.text "Edit Definitions" ]

                       , RD.textarea [ RP.value state.code
                                     , RP.onChange (send <<< UpdateDefinitions <<< _.target.value <<< unsafeCoerce)
                                     ] []
                       ]
              ]

    errorPane :: ReactElement
    errorPane =
      case state.error of
        Nothing ->
          RD.div' []
        Just err ->
          RD.div [ RP.className "alert alert-danger alert-dismissible"
                 , RP.role "alert"
                 ]
                 [ RD.text err ]

spec :: forall a b. T.Spec a AppState b Action
spec = T.simpleSpec performAction render

-- | The main method creates the task list component, and renders it to the document body.
main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial fromJust <<< toMaybe <$> DOM.querySelector "#app" (DOM.htmlDocumentToParentNode document)
  RDOM.render (R.createFactory component unit) container
