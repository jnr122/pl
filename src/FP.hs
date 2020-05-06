{-# LANGUAGE QuasiQuotes #-}
module FP where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Lex
import Util.Testing

import qualified Lang.L5 as L5

import Lang.Mega as LM

import Control.Monad

-----------------
-- Translator --
-----------------
translateE :: LM.Expr -> L5.Expr
translateE e = case e of
  LM.IntE i -> L5.IntE i
  LM.PlusE e1 e2 -> L5.PlusE (translateE e1) (translateE e2)
  LM.TimesE e1 e2 -> L5.TimesE (translateE e1) (translateE e2)
  LM.BoolE b -> L5.BoolE b
  LM.IfE e1 e2 e3 -> L5.IfE (translateE e1) (translateE e2) (translateE e3)
  LM.VarE s -> L5.VarE s

  _ -> error "Not Implemented"

-----------------
-- Interpreter --
-----------------
interpWithEnv :: L5.EnvE -> L5.Expr -> L5.AnswerE
interpWithEnv env e = case e of
  L5.IntE i -> L5.ValueEA (L5.IntEV i)
  L5.PlusE e1 e2 -> case (interpWithEnv env e1, interpWithEnv env e2) of
    (L5.ValueEA (L5.IntEV i1), L5.ValueEA (L5.IntEV i2)) -> L5.ValueEA (L5.IntEV (i1 + i2))
    _ -> L5.BadEA
  L5.TimesE e1 e2 -> case (interpWithEnv env e1, interpWithEnv env e2) of
    (L5.ValueEA (L5.IntEV i1), L5.ValueEA (L5.IntEV i2)) -> L5.ValueEA (L5.IntEV (i1 * i2))
    _ -> L5.BadEA
  L5.BoolE b -> L5.ValueEA (L5.BoolEV b)
  L5.IfE e1 e2 e3 -> case interpWithEnv env e1 of
    L5.ValueEA (L5.BoolEV b) ->
      if b
         then interpWithEnv env e2
         else interpWithEnv env e3
    _ -> L5.BadEA
  L5.VarE x -> case Map.lookup x env of
    Just v -> L5.ValueEA v
    Nothing -> L5.BadEA
  L5.LetE x e1 e2 -> case interpWithEnv env e1 of
    L5.ValueEA v -> interpWithEnv (Map.insert x v env) e2
    _ -> L5.BadEA
  L5.FunE x e -> L5.ValueEA (L5.CloEV x e env)
  L5.AppE e1 e2 -> case (interpWithEnv env e1, interpWithEnv env e2) of
    (L5.ValueEA (L5.CloEV x e' env'), L5.ValueEA v) -> interpWithEnv (Map.insert x v env') e'
    _ -> L5.BadEA

  --NEW
  L5.PairE e1 e2 -> case (interpWithEnv env e1, interpWithEnv env e2) of
    (L5.ValueEA v1, L5.ValueEA v2) -> L5.ValueEA (L5.PairEV v1 v2)
    _ -> L5.BadEA
  L5.FstE e -> case interpWithEnv env e of
    L5.ValueEA (L5.PairEV v1 v2) -> L5.ValueEA v1
    _ -> L5.BadEA 
  L5.SndE e -> case interpWithEnv env e of
    L5.ValueEA (L5.PairEV v1 v2) -> L5.ValueEA v2
    _ -> L5.BadEA

  L5.CallE e1 x1 e2 x2 e3 -> case interpWithEnv env e1 of
    L5.ValueEA (L5.LeftEV v)  -> interpWithEnv (Map.insert x1 v env) e2
    L5.ValueEA (L5.RightEV v) -> interpWithEnv (Map.insert x2 v env) e3
    _ -> L5.BadEA
  L5.LeftE e -> case interpWithEnv env e of
    L5.ValueEA v -> L5.ValueEA (L5.LeftEV v)
    _ -> L5.BadEA
  L5.RightE e -> case interpWithEnv env e of
    L5.ValueEA v -> L5.ValueEA (L5.RightEV v)
    _ -> L5.BadEA

------------------
-- Type Checker --
------------------
typeCheck :: LM.Env -> LM.Expr -> Maybe (LM.Type)
typeCheck env e = case e of
  LM.IntE i -> Just (LM.IntT)
  LM.PlusE e1 e2 -> Nothing

  _ -> Nothing


test1 :: Test
test1 = TestDir
  ( "T1"                 -- e.g., "T1"
  , "identity function"  -- e.g., "interp"
  , id                   -- the function, e.g., (\ e -> interp e Map.empty Map.empty)
  , "tests/fp"           -- the directory where tests live, e.g., "tests/fp/t1"
  , parseTest LM.pExpr LM.pExpr
  )

test2 :: Test
test2 = Test1
  ( "T2"
  , "identity function"
  , id
  , [ -- test input
      ( [lme| let p = (1,2) in fst p |] 
      -- expeced output
      , [lme| let p = (1,2) in fst p |]
      )
    ]
  )

    
testE3 :: Test
testE3 = TestDir
  ( "E3"
  , "interpWithEnv"
  , interpWithEnv Map.empty
  , "tests/fp"
  , parseTest L5.pExpr L5.pAnswerE
  )

main :: IO ()
main = do
  putStrLn "TESTS"
  runTests 
    [
     testE3
    ]
  putStrLn "EXAMPLE"
  putStrLn (show [lme| let p = (1,2) in fst p |])
