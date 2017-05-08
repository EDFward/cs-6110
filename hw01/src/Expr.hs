module Expr
  ( Expr(..)
  , Name
  ) where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving (Show, Eq)
