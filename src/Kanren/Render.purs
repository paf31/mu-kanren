module Kanren.Render where

import DOM

import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Array (length, null, sortBy, (..))
import Data.Foldable (intercalate)
import Data.Traversable (for)
import Data.Foreign
import Data.Foreign.Class

import Control.Bind
import Control.Apply
import Control.Monad (when, unless)
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.JQuery

import Kanren.Eval
import Kanren.State
import Kanren.Goal
import Kanren.Term
import Kanren.Subst
import Kanren.Var
import Kanren.Obj
import Kanren.Unify
import Kanren.Parser
 
showError :: forall eff. String -> Eff (dom :: DOM | eff) Unit
showError err = void do
  button <- create "<button type='button' class='close' data-dismiss='alert'>&times;</button>"
  alert <- create "<div>"
    >>= addClass "alert alert-danger alert-dismissible"
    >>= attr { role: "alert" }
    >>= append button
    >>= appendText err
  select "#error" >>= append alert 

showEditor :: forall eff. Eff (dom :: DOM | eff) Unit
showEditor = void do
  select "#editor" 
    >>= css { display: "block" } 
  select "#goal" 
    >>= css { display: "none" }
  select "#editButton"  
    >>= css { display: "none" }
  select "#undoButton"  
    >>= css { display: "none" }
    
hideEditor :: forall eff. Eff (dom :: DOM | eff) Unit
hideEditor = void do
  select "#editor" 
    >>= css { display: "none" } 
  select "#goal" 
    >>= css { display: "block" }
  select "#editButton" 
    >>= css { display: "block" }
  select "#undoButton" 
    >>= css { display: "block" }
    
eval :: forall eff. RefVal (Maybe [Define]) -> RefVal [State] -> Eff (ref :: Ref, dom :: DOM | eff) Unit
eval definesRef history = do
  code <- select "#code" >>= getValue
  defines <- select "#defines" >>= getValue
  
  case Tuple <$> read code <*> read defines of
    Left err -> showError (show err)
    Right (Tuple code defines) -> 
      case Tuple <$> parseGoal code <*> parseDefines defines of
        Left err -> showError err
        Right (Tuple goal defines) -> do
          writeRef history []
          writeRef definesRef $ Just defines
          render definesRef history (State goal [] zero [])
  
renderSavingState :: forall eff. RefVal (Maybe [Define]) -> State -> RefVal [State] -> State -> Eff (ref :: Ref, dom :: DOM | eff) Unit   
renderSavingState definesRef oldState history newState = do  
  modifyRef history ((:) oldState)
  render definesRef history newState
  
render :: forall eff. RefVal (Maybe [Define]) -> RefVal [State] -> State -> Eff (ref :: Ref, dom :: DOM | eff) Unit   
render definesRef history st@(State g su var stk) = void do 
      
  -- Hide the editor panel
    
  hideEditor    
  
  -- Update the Undo button
  
  sts <- readRef history
  select "#undoButton" 
    >>= css { display: if null sts then "none" else "block" }  
    
  -- Update the goal    
    
  select "#goal .lines" >>= remove    
    
  goal <- create "<div>" >>= addClass "lines"
  renderGoal true goal g
  select "#goal" >>= append goal
    
  -- Update the stack    
    
  select "#stack tbody tr" >>= remove
  stackBody <- select "#stack tbody"
  
  for stk $ \g' -> void do 
    tr <- create "<tr>"
    td <- create "<td>"
    pre <- create "<pre>" >>= appendText (renderShortGoal g')
    pre `append` td
    td `append` tr
    tr `append` stackBody
  
  -- Update the substitution
  
  select "#subst tbody tr" >>= remove
  
  substBody <- select "#subst tbody"
  
  for (sortBy (compare `Data.Function.on` fst) su) $ \(Tuple (Var nm) tm) -> do
    tr <- create "<tr>"
    td1 <- create "<td>" >>= appendText ("#" ++ show nm)
    pre <- create "<pre>" >>= appendText (renderTerm (walk su tm))
    td2 <- create "<td>" >>= append pre
    td1 `append` tr
    td2 `append` tr
    tr `append` substBody
  where
      
  renderShortGoal :: Goal -> String
  renderShortGoal Done = "Done"
  renderShortGoal (Fresh ns _) = "fresh " ++ intercalate " " ns
  renderShortGoal (Unify u v) = renderTerm u ++ " == " ++ renderTerm v
  renderShortGoal (Disj _) = "disj"
  renderShortGoal (Conj _) = "conj"
  renderShortGoal (Named name _) = name
    
  renderGoal :: forall eff. Boolean -> JQuery -> Goal -> Eff (ref :: Ref, dom :: DOM | eff) Unit 
  renderGoal _           jq Done = void do
    "Evaluation complete" `appendText` jq
  renderGoal _           jq Fail = void do
    "Contradiction!" `appendText` jq
  renderGoal renderLinks jq (Fresh ns g) = void do
    let freshNames = TmVar <<< Var <$> (runVar var .. (runVar var + length ns - 1))
        newState = State (replaceAll (zip ns freshNames) g) su nextVar stk
        nextVar = Var (runVar var + length ns)
    link <- linkTo renderLinks (renderSavingState definesRef st history newState)
              >>= appendText ("(fresh " ++ intercalate " " ns ++ "")
    line <- newLine >>= append link
    line `append` jq
    rest <- indented
    renderGoal false rest g
    rest `append` jq
    close <- newLine >>= appendText ")"
    close `append` jq
  renderGoal renderLinks jq (Unify u v) = void do
    let text = "(= " ++ renderTerm u ++ " " ++ renderTerm v ++ ")"
        action = case unify u v su of
          Nothing -> renderSavingState definesRef st history $ State Fail su var stk
          Just su' -> renderSavingState definesRef st history $ unwind $ State Done su' var stk
    link <- linkTo renderLinks action 
              >>= appendText text
    line <- newLine >>= append link
    line `append` jq
  renderGoal renderLinks jq (Named nm ts) = do
    Just defines <- readRef definesRef
    case builtIn defines nm ts of
      Left err -> showError err
      Right newGoal ->  void do
        let text = "(" ++ nm ++ " " ++ intercalate " " (renderTerm <$> ts) ++ ")"
            newState = State newGoal su var stk
        link <- linkTo renderLinks (renderSavingState definesRef st history newState) 
                  >>= appendText text
        line <- newLine >>= append link
        line `append` jq
  renderGoal renderLinks jq (Disj gs) = void do
    line <- newLine >>= appendText "(disj"
    line `append` jq
    
    for gs $ \g -> do
      i <- indented
      a <- linkTo renderLinks (renderSavingState definesRef st history (unwind (State g su var stk))) 
      renderGoal false a g
      a `append` i
      i `append` jq
    
    close <- newLine >>= appendText ")"
    close `append` jq
  renderGoal renderLinks jq (Conj gs) = void do
    line <- newLine >>= appendText "(conj"
    line `append` jq
    
    for (inContext gs) $ \(Tuple g rest) -> do
      i <- indented
      a <- linkTo renderLinks (renderSavingState definesRef st history (unwind (State g su var (rest ++ stk)))) 
      renderGoal false a g
      a `append` i
      i `append` jq
      
    close <- newLine >>= appendText ")"
    close `append` jq
  
  unwind :: State -> State
  unwind (State Done subst var (goal : stack)) = State goal subst var stack
  unwind other = other
  
  linkTo :: forall eff a. Boolean -> (Eff (dom :: DOM | eff) a) -> Eff (dom :: DOM | eff) JQuery 
  linkTo true action =
    create "<a href='#'>" 
      >>= on "click" (\e _ -> action *> preventDefault e)
  linkTo false _ = 
    create "<span>"
        
  indented :: forall eff. Eff (dom :: DOM | eff) JQuery
  indented = create "<div>" >>= addClass "indented"
  
  newLine :: forall eff. Eff (dom :: DOM | eff) JQuery
  newLine = create "<div>" >>= addClass "line"
    
  renderTerm :: Term -> String
  renderTerm (TmVar (Var v)) = "#" ++ show v
  renderTerm (TmObj (Obj o)) = o
  renderTerm (TmPair t1 t2) = "(" ++ renderTerm t1 ++ " " ++ renderTerm t2 ++ ")"
    
  spaces = go ""
    where
    go acc 0 = acc
    go acc n = go (acc ++ "  ") (n - 1)
    
  inContext :: forall a. [a] -> [Tuple a [a]]
  inContext = go [] []
    where
    go acc _  []       = acc
    go acc ys (x : xs) = go (Tuple x (ys ++ xs) : acc) (ys ++ [x]) xs