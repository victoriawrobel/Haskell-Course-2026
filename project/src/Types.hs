module Types
  ( Addr
  , Value(..)
  , Op(..)
  , RangeOp(..)
  , Expr(..)
  , Content(..)
  , Cell(..)
  , Sheet(..)
  , Env
  ) where

import Data.Map (Map)

type Addr = (String, Int)

data Value 
  = NumV Double 
  | BoolV Bool 
  | StrV String 
  | ErrV String 
  deriving (Show, Eq, Ord)

data Op = Add | Sub | Mul | Div | Pow | EqOp | LtOp | GtOp deriving (Show, Eq, Ord)
data RangeOp = SumR | AvgR | MinR | MaxR | CountR deriving (Show, Eq, Ord)

data Expr
  = Ref Addr
  | LitE Value
  | BinOp Op Expr Expr
  | RangeOp RangeOp Addr Addr
  deriving (Show, Eq, Ord)

data Content
  = Lit Value
  | Form Expr
  deriving (Show, Eq, Ord)

data Cell = Cell
  { addr    :: Addr
  , content :: Content
  } deriving (Show, Eq, Ord)

newtype Sheet = Sheet { unSheet :: Map Addr Cell } deriving (Show, Eq)

type Env = Map Addr Value
