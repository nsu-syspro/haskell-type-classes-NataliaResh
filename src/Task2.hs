{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Task2 where

import Task1 (Parse, Parse(..))

import Data.List (find)

-- * Expression data type

-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show

-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving Show

-- * Parsing

-- | Parses given expression in Reverse Polish Notation
-- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
--
-- Usage example:
--
-- >>> parse "2" :: Maybe (Expr Integer IntOp)
-- Just (Lit 2)
-- >>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Sub (Lit 2) (Lit 3))
-- >>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
-- Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
-- >>> parse "2 +" :: Maybe (Expr Integer IntOp)
-- Nothing
-- >>> parse "2 3" :: Maybe (Expr Integer IntOp)
-- Nothing
--
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse :: (Parse a, Parse op) => String -> Maybe (Expr a op)
  parse s = go (words s) []
    where
      go :: [String] -> [Expr a op] -> Maybe (Expr a op)
      go [] stack     = case stack of
        [x] -> Just x
        _   -> Nothing
      go (x:xs) stack = case (parse x :: Maybe op) of
        Just op -> applyOp xs stack op
        Nothing -> case(parse x :: Maybe a) of
          Just e -> go xs (Lit e : stack)
          _      -> go xs (Var x : stack)
          
      applyOp xs stack op = case stack of 
          (e1:e2:es) -> go xs (BinOp op e2 e1 : es)
          _          -> Nothing


instance Parse IntOp where
  parse :: String -> Maybe IntOp
  parse "+" = Just Add
  parse "-" = Just Sub
  parse "*" = Just Mul
  parse _   = Nothing

-- * Evaluation

-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a


instance Eval Integer IntOp where
  evalBinOp :: IntOp -> Integer -> Integer -> Integer
  evalBinOp Add x y = x + y
  evalBinOp Sub x y = x - y
  evalBinOp Mul x y = x * y

-- | Evaluates given 'Expr' using given association list of variable values
--
-- Returns 'Nothing' in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
-- Just 2
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
-- Just 5
-- >>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
-- Nothing
--
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
evalExpr dict e = case e of
  Lit n         -> Just n
  Var x         -> case find (\y -> fst y == x) dict of
    Just p  -> Just (snd p)
    Nothing -> Nothing
  (BinOp op x y) -> case evalExpr dict x of 
    (Just e1) -> case evalExpr dict y of
      (Just e2) -> Just $ evalBinOp op e1 e2
      Nothing   -> Nothing
    Nothing   -> Nothing

-- | Parses given integer expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- Usage example:
--
-- >>> evaluateInteger [] "2"
-- Just 2
-- >>> evaluateInteger [("x", 3)] "2 x -"
-- Just (-1)
-- >>> evaluateInteger [("x", 3)] "2 y -"
-- Nothing
-- >>> evaluateInteger [] "3 2 * 3 +"
-- Just 9
-- >>> evaluateInteger [] "2 +"
-- Nothing
-- >>> evaluateInteger [] "2 3"
-- Nothing
--
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
evaluateInteger = evaluate @_ @IntOp 

-- | Parses given expression in Reverse Polish Notation and evaluates it
-- using given association list of variable values
--
-- Returns 'Nothing' in case the expression could not be parsed
-- or in case appropriate variable value is missing.
--
-- The 'forall a op.' part is required to define generic type
-- of intermediate 'Expr' expression that uses scoped type variables 'a' and 'op'.
--
evaluate :: forall a op. (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
evaluate m s = case parse s of
  Just e -> evalExpr m (e :: Expr a op)
  Nothing -> Nothing

