{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where
    
import Task1 (Parse, Parse(..))
import Task2 (Expr, Expr(..), Eval, Eval(..), evaluate, evalExpr)
import Data.Bits (xor)
import Data.List (nub)

data BoolOp = And | Or | Xor
  deriving Show


instance Parse BoolOp where
  parse :: String -> Maybe BoolOp
  parse "and" = Just And
  parse "or"  = Just Or
  parse "xor" = Just Xor
  parse _     = Nothing

instance Eval Bool BoolOp where
  evalBinOp :: BoolOp -> Bool -> Bool -> Bool
  evalBinOp And x y = x && y
  evalBinOp Or x y  = x || y
  evalBinOp Xor x y = x `xor` y

evaluateBool :: [(String, Bool)] -> String -> Maybe Bool
evaluateBool = evaluate @_ @BoolOp

-- | Solves SAT problem for given boolean formula written in Reverse Polish Notation
--
-- Returns whether given formula is satifiable
-- wrapped into 'Maybe' with 'Nothing' indicating parsing failure.
--
-- Only following binary operations are allowed:
-- - and
-- - or
-- - xor
--
-- All other words are considered as variables.
--
-- Usage example:
--
-- >>> solveSAT "x y and"
-- Just True
-- >>> solveSAT "x y xor x y and and"
-- Just False
-- >>> solveSAT "x y xor x y and or"
-- Just True
-- >>> solveSAT "x xor"
-- Nothing
--
solveSAT :: String -> Maybe Bool
solveSAT s = case (parse s :: Maybe (Expr Bool BoolOp)) of
  Just e -> Just (any ((== Just True) . (`evalExpr` e)) (getAssignments $ getVars e))
  Nothing -> Nothing
  where
    getVars :: Expr a op -> [String]
    getVars = nub . go
      where
        go (Var x)         = [x]
        go (Lit _)         = []
        go (BinOp _ e1 e2) = getVars e1 ++ getVars e2

    getAssignments :: [String] -> [[(String, Bool)]]
    getAssignments []     = [[]]
    getAssignments (x:xs) = [(x, val) : rest | val <- [True, False], rest <- getAssignments xs]
