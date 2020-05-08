module Lang.L5.Data where

import Data.Map (Map)


-----------
-- TYPES --
-----------

data Type =
    IntT
  | BoolT
  | FunT Type Type
  | PairT Type Type
  | TUnionT Type Type
  | StringT
  deriving (Eq,Ord,Show)


-----------------
-- EXPRESSIONS --
-----------------

data Expr = 
    IntE Integer
  | PlusE Expr Expr 
  | TimesE Expr Expr
  | BoolE Bool
  | IfE Expr Expr Expr
  | VarE String
  | LetE String Expr Expr
  | FunE String Expr
  | AppE Expr Expr

  -- NEW
  | PairE Expr Expr
  | FstE Expr
  | SndE Expr
  | CaseE Expr String Expr String Expr
  | LeftE Expr
  | RightE Expr
  | StringE String
  deriving (Eq,Ord,Show)


---------------------------
-- ENVIRONMENT SEMANTICS --
---------------------------

data ValueE = 
    IntEV Integer
  | BoolEV Bool
  | CloEV String Expr EnvE

  -- NEW
  | PairEV ValueE ValueE
  | LeftEV ValueE
  | RightEV ValueE
  | StringEV String
  deriving (Eq,Ord,Show)

data AnswerE = 
    ValueEA ValueE
  | BadEA
  deriving (Eq,Ord,Show)

type EnvE = Map String ValueE
type EnvT = Map String Type

----------------------------
-- SUBSTITUTION SEMANTICS --
----------------------------

data ValueS =
    IntSV Integer
  | BoolSV Bool
  | FunSV String Expr
  deriving (Eq,Ord,Show)

data AnswerS =
    ValueSA ValueS
  | BadSA
  deriving (Eq,Ord,Show)

type EnvS = Map String ValueS
