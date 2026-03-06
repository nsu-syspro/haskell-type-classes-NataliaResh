{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where
import Text.Read (readMaybe)

-- * Expression data type

-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show

-- * Evaluation

-- | Evaluates given 'IExpr'
--
-- Usage example:
--
-- >>> evalIExpr (Lit 2)
-- 2
-- >>> evalIExpr (Add (Lit 2) (Lit 3))
-- 5
-- >>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- 9
--
evalIExpr :: IExpr -> Integer
evalIExpr expr = case expr of
  Lit x -> x
  Add expr1 expr2 -> culculate expr1 expr2 (+)
  Mul expr1 expr2 -> culculate expr1 expr2 (*)
  where
    culculate :: IExpr -> IExpr -> (Integer -> Integer -> Integer) -> Integer
    culculate e1 e2 op = op (evalIExpr e1) (evalIExpr e2)

-- * Parsing

-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe IExpr
-- Just (Lit 2)
-- >>> parse "2 3 +" :: Maybe IExpr
-- Just (Add (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe IExpr
-- Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe IExpr
-- Nothing
-- >>> parse "2 3" :: Maybe IExpr
-- Nothing
--
instance Parse IExpr where
  parse :: String -> Maybe IExpr
  parse s = go (words s) []
    where
      go :: [String] -> [IExpr] -> Maybe IExpr
      go [] stack     = case stack of
        [x] -> Just x
        _   -> Nothing
      go (x:xs) stack = case x of
        "+" -> applyOp xs stack Add
        "*" -> applyOp xs stack Mul
        _   -> case parse x of
          Just n  -> go xs (Lit n : stack)
          Nothing -> Nothing
       
      applyOp :: [String] -> [IExpr] -> (IExpr -> IExpr -> IExpr) -> Maybe IExpr
      applyOp xs stack op = case stack of 
          (e1:e2:es) -> go xs (op e2 e1 : es)
          _          -> Nothing

instance Parse Integer where
  parse :: String -> Maybe Integer
  parse = readMaybe

instance Parse Bool where
  parse :: String -> Maybe Bool
  parse "True"  = Just True
  parse "False" = Just False
  parse _       = Nothing

-- * Evaluation with parsing

-- | Parses given expression in Reverse Polish Notation and evaluates it
--
-- Returns 'Nothing' in case the expression could not be parsed.
--
-- Usage example:
--
-- >>> evaluateIExpr "2"
-- Just 2
-- >>> evaluateIExpr "2 3 +"
-- Just 5
-- >>> evaluateIExpr "3 2 * 3 +"
-- Just 9
-- >>> evaluateIExpr "2 +"
-- Nothing
-- >>> evaluateIExpr "2 3"
-- Nothing
--
evaluateIExpr :: String -> Maybe Integer
evaluateIExpr s = case parse s of
  Just e  -> Just (evalIExpr e)
  Nothing -> Nothing
