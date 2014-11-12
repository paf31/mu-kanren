module Kanren.Parser (parseGoal, parseDefines) where
    
import Data.Maybe 
import Data.Either
import Data.Tuple  
import Data.Foldable  
import Data.Traversable  
    
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import qualified Data.String as S 
    
import Text.Parsing.Parser   
import Text.Parsing.Parser.Combinators   
import Text.Parsing.Parser.String  
    
import Control.Lazy
import Control.Alt    
import Control.Alternative
import Control.Apply    
    
import Kanren.Goal
import Kanren.Term    
    
data Sexpr = Atom String | Sexpr [Sexpr]

instance showSexpr :: Show Sexpr where
  show (Atom s) = "(Atom " ++ show s ++ ")"
  show (Sexpr ss) = "(Sexpr " ++ show ss ++ ")"

parseSexpr :: Parser String Sexpr
parseSexpr = fix1 $ \p -> whiteSpace *> (try parseAtom <|> parens (Sexpr <$> many p)) <* whiteSpace
  where
  parens :: forall a. Parser String a -> Parser String a
  parens p = string "(" *> whiteSpace *> p <* whiteSpace <* string ")"
  
  parseAtom :: forall a. Parser String Sexpr
  parseAtom = Atom <<< S.joinWith "" <$> some (noneOf [" ", ")", "(", "\n", "\t", "\r"])

parseGoal :: String -> Either String Goal
parseGoal s = 
  case runParser s (parseSexpr <* eof) of
    Left (ParseError err) -> Left err.message
    Right sexpr -> sexprToGoal sexpr
  
parseDefines :: String -> Either String [Define]
parseDefines s = 
  case runParser s (many parseSexpr <* eof) of
    Left (ParseError err) -> Left err.message
    Right ss -> traverse sexprToDefine ss
    
  where
  sexprToDefine :: Sexpr -> Either String Define
  sexprToDefine (Sexpr (Atom "define" : Atom name : rest)) | not (A.null rest) = 
    Define name <$> traverse fromAtom (AU.init rest) <*> sexprToGoal (AU.last rest)
  sexprToDefine (Sexpr (Atom "define" : _)) = Left "define expects two or more arguments"
  sexprToDefine _ = Left "Cannot parse definition"
    
sexprToGoal :: Sexpr -> Either String Goal
sexprToGoal (Sexpr (Atom "fresh" : rest)) | A.length rest > 1 = 
  Fresh <$> traverse fromAtom (AU.init rest) <*> sexprToGoal (AU.last rest)
sexprToGoal (Sexpr (Atom "fresh" : _)) = Left "fresh expects two or more arguments"
sexprToGoal (Sexpr [Atom "=", t1, t2]) = 
  Unify <$> sexprToTerm t1 <*> sexprToTerm t2
sexprToGoal (Sexpr (Atom "=" : _)) = Left "= expects two arguments"
sexprToGoal (Sexpr (Atom "disj" : rest)) | A.length rest > 1 = 
  Disj <$> traverse sexprToGoal rest
sexprToGoal (Sexpr (Atom "disj" : _)) = Left "disj expects at least two arguments"
sexprToGoal (Sexpr (Atom "conj" : rest)) | A.length rest > 1 = 
  Conj <$> traverse sexprToGoal rest
sexprToGoal (Sexpr (Atom "conj" : _)) = Left "conj expects at least two arguments"
sexprToGoal (Sexpr (Atom nm : rest)) = 
  Named nm <$> traverse sexprToTerm rest
sexprToGoal _ = Left "Cannot parse goal"

sexprToTerm :: Sexpr -> Either String Term
sexprToTerm (Atom s) = Right $ obj s
sexprToTerm (Sexpr [t1, t2]) = TmPair <$> sexprToTerm t1 <*> sexprToTerm t2
sexprToTerm _ = Left "Invalid term"

fromAtom :: Sexpr -> Either String String
fromAtom (Atom s) = Right s
fromAtom _ = Left "Expected atom"