module Kanren.Parser
  ( parseGoal
  , parseDefines
  ) where

import Prelude
import Control.Alternative ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.List (List(..), length, many, null, (:))
import Data.Traversable (traverse)
import Kanren.Goal (Define(..), Goal(..))
import Kanren.Term (Term(..), obj)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (try)
import Data.Array as A
import Data.List.Partial as LP
import Data.String as S
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser.String (eof, noneOf, string, whiteSpace)

data Sexpr = Atom String | Sexpr (List Sexpr)

instance showSexpr :: Show Sexpr where
  show (Atom s) = "(Atom " <> show s <> ")"
  show (Sexpr ss) = "(Sexpr " <> show ss <> ")"

parseSexpr :: Parser String Sexpr
parseSexpr = fix $ \p -> whiteSpace *> (try parseAtom <|> parens (Sexpr <$> many p)) <* whiteSpace
  where
    parens :: forall a. Parser String a -> Parser String a
    parens p = string "(" *> whiteSpace *> p <* whiteSpace <* string ")"

    parseAtom :: Parser String Sexpr
    parseAtom = Atom <<< S.fromCharArray <$> A.some (noneOf [' ', ')', '(', '\n', '\t', '\r'])

parseGoal :: String -> Either String Goal
parseGoal s =
  case runParser s (parseSexpr <* eof) of
    Left err -> Left (parseErrorMessage err)
    Right sexpr -> sexprToGoal sexpr

parseDefines :: String -> Either String (List Define)
parseDefines s =
    case runParser s (many parseSexpr <* eof) of
      Left err -> Left (parseErrorMessage err)
      Right ss -> traverse sexprToDefine ss
  where
    sexprToDefine :: Sexpr -> Either String Define
    sexprToDefine (Sexpr (Atom "define" : Atom name : rest)) | not (null rest) =
      unsafePartial $
        Define name <$> traverse fromAtom (LP.init rest) <*> sexprToGoal (LP.last rest)
    sexprToDefine (Sexpr (Atom "define" : _)) = Left "define expects two or more arguments"
    sexprToDefine _ = Left "Cannot parse definition"

sexprToGoal :: Sexpr -> Either String Goal
sexprToGoal (Sexpr (Atom "fresh" : rest)) | length rest > 1 = unsafePartial $
  Fresh <$> traverse fromAtom (LP.init rest) <*> sexprToGoal (LP.last rest)
sexprToGoal (Sexpr (Atom "fresh" : _)) = Left "fresh expects two or more arguments"
sexprToGoal (Sexpr (Atom "=" : t1 : t2 : Nil)) =
  Unify <$> sexprToTerm t1 <*> sexprToTerm t2
sexprToGoal (Sexpr (Atom "=" : _)) = Left "= expects two arguments"
sexprToGoal (Sexpr (Atom "disj" : rest)) | length rest > 1 =
  Disj <$> traverse sexprToGoal rest
sexprToGoal (Sexpr (Atom "disj" : _)) = Left "disj expects at least two arguments"
sexprToGoal (Sexpr (Atom "conj" : rest)) | length rest > 1 =
  Conj <$> traverse sexprToGoal rest
sexprToGoal (Sexpr (Atom "conj" : _)) = Left "conj expects at least two arguments"
sexprToGoal (Sexpr (Atom nm : rest)) =
  Named nm <$> traverse sexprToTerm rest
sexprToGoal _ = Left "Cannot parse goal"

sexprToTerm :: Sexpr -> Either String Term
sexprToTerm (Atom s) = Right $ obj s
sexprToTerm (Sexpr (t1 : t2 : Nil)) = TmPair <$> sexprToTerm t1 <*> sexprToTerm t2
sexprToTerm _ = Left "Invalid term"

fromAtom :: Sexpr -> Either String String
fromAtom (Atom s) = Right s
fromAtom _ = Left "Expected atom"
