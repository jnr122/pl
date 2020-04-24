module Lang.L4.Data where

import Data.Map (Map)

-- NEW
data Pair =
    FstE Expr
  | SndE Expr
  | FstV Value
  | SndV Value

data TagUn =
    LeftE Expr
  | RightE Expr
  | LeftV Value
  | RIghtV Value
  
data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  | BoolE Bool
  | IfE Expr Expr Expr
  | VarE String
  | LetE String Expr Expr
  | CallE String [Expr]

  -- NEW
  | PairE Expr Expr
  | TagUnE Expr Expr
  deriving (Eq,Ord,Show)

data Command =
    DefC String [String] Expr
  deriving (Eq,Ord,Show)

data Program =
    Program [Command] Expr
  deriving (Eq,Ord,Show)

data Value = 
    IntV Integer
  | BoolV Bool

  -- NEW
  | PairV Value Value
  | TagUnV Value Value
  
  deriving (Eq,Ord,Show)

data Answer = 
    ValueA Value
  | BadA
  deriving (Eq,Ord,Show)

type Env = Map String Value
type FEnv = Map String ([String],Expr)
