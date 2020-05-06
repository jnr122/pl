{-# LANGUAGE QuasiQuotes #-}
module FP where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Lex
import Util.Testing

import qualified Lang.L4 as L4
import qualified Lang.L5 as L5

import Lang.Mega as LM

import Control.Monad


-- ========== --
-- HELPERS    --
-- ========== --

-- An interpreter for commands. A command just adds a new function
-- definition to the function environment.
interpCommand :: L4.FEnv -> L4.Command -> L4.FEnv
interpCommand fenv c = case c of
  L4.DefC fx xs e -> Map.insert fx (xs,e) fenv


-- A function that interprets many commands at once.
interpCommandMany :: L4.FEnv -> [L4.Command] -> L4.FEnv
interpCommandMany fenv cs = case cs of
  [] -> fenv
  c:cs' ->
    let fenv' = interpCommand fenv c
    in interpCommandMany fenv' cs'

-- A function that extends a standard environment to map a list of names
-- to a list of values.
extendEnvMany :: [String] -> [L4.Value] -> L4.Env -> Maybe L4.Env
extendEnvMany xs vs env = case (xs,vs) of
  ([],[]) -> Just env
  (x:xs',v:vs') -> case extendEnvMany xs' vs' env of
    Just env' -> Just (Map.insert x v env')
    Nothing -> Nothing
  ([],_:_) -> Nothing
  (_:_,[]) -> Nothing


-- =========== --
-- INTERPRETER
-- =========== --

-- An interpreter for a language with integers, plus, times, booleans,
-- conditionals, variables, let-binding, function definitions and function
-- calls.
--
-- e ∈ expr ⩴ i | e + e | e * e | b | if e then e else e 
--          | x | let x = e in e
--          | f(e,…,e)
interpExpr :: L4.FEnv -> L4.Env -> L4.Expr -> L4.Answer
interpExpr fenv env e = case e of
  L4.IntE i -> L4.ValueA (L4.IntV i)
  L4.PlusE e1 e2 -> case (interpExpr fenv env e1,interpExpr fenv env e2) of
    (L4.ValueA (L4.IntV i1),L4.ValueA (L4.IntV i2)) -> L4.ValueA (L4.IntV (i1 + i2))
    _ -> L4.BadA
  L4.TimesE e1 e2 -> case (interpExpr fenv env e1,interpExpr fenv env e2) of
    (L4.ValueA (L4.IntV i1),L4.ValueA (L4.IntV i2)) -> L4.ValueA (L4.IntV (i1 * i2))
    _ -> L4.BadA
  L4.BoolE b -> L4.ValueA (L4.BoolV b)
  L4.IfE e1 e2 e3 -> case interpExpr fenv env e1 of
    L4.ValueA (L4.BoolV b) ->
      if b
      then interpExpr fenv env e2
      else interpExpr fenv env e3
    _ -> L4.BadA
  L4.VarE x -> case Map.lookup x env of
    Just v -> L4.ValueA v
    Nothing -> L4.BadA
  L4.LetE x e1 e2 -> case interpExpr fenv env e1 of
    L4.ValueA v -> interpExpr fenv (Map.insert x v env) e2
    _ -> L4.BadA
  L4.CallE fx es -> case Map.lookup fx fenv of
    Just (xs,e) -> case interpExprMany fenv env es of
      Just vs -> case extendEnvMany xs vs Map.empty of
        Just env' -> interpExpr fenv env' e
        Nothing -> L4.BadA
      Nothing -> L4.BadA
    Nothing -> L4.BadA

  -- NEW

  --Pairs
  L4.FstE e -> case interpExpr fenv env e of
    L4.ValueA (L4.PairV v1 v2) -> L4.ValueA v1
    _ -> L4.BadA
  L4.SndE e -> case interpExpr fenv env e of
    L4.ValueA (L4.PairV v1 v2) -> L4.ValueA v2
    _ -> L4.BadA
  L4.PairE e1 e2 -> case (interpExpr fenv env e1, interpExpr fenv env e2) of
    (L4.ValueA v1, L4.ValueA v2) -> L4.ValueA (L4.PairV v1 v2)
    _ -> L4.BadA

  --Tagged Unions
  L4.LeftE e -> case interpExpr fenv env e of
    L4.ValueA v -> L4.ValueA (L4.LeftV v)
    _ -> L4.BadA
  L4.RightE e -> case interpExpr fenv env e of
    L4.ValueA v -> L4.ValueA (L4.RightV v)
    _ -> L4.BadA
  L4.TagUnE e1 e2 e3 x1 x2 -> case interpExpr fenv env e1 of
    L4.ValueA (L4.LeftV v)  -> interpExpr fenv (Map.insert x1 v env) e2
    L4.ValueA (L4.RightV v) -> interpExpr fenv (Map.insert x2 v env) e3
    _ -> L4.BadA

interpExprMany :: L4.FEnv -> L4.Env -> [L4.Expr] -> Maybe [L4.Value]
interpExprMany fenv env es = case es of
  [] -> Just []
  e:es' -> case interpExpr fenv env e of
    L4.ValueA v -> case interpExprMany fenv env es' of
      Just vs -> Just (v:vs)
      Nothing -> Nothing
    L4.BadA -> Nothing

interp :: L4.Program -> L4.Answer
interp (L4.Program cs e) =
  let fenv' = interpCommandMany Map.empty cs
  in interpExpr fenv' Map.empty e

  
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
  , "interp"
  , interp
  , "tests/fp"
  , parseTest L4.pProgram L4.pAnswer
  )
  

main :: IO ()
main = do
  putStrLn "TESTS"
  runTests 
    [
      test2
    , testE3
    ]
  putStrLn "EXAMPLE"
  putStrLn (show [lme| let p = (1,2) in fst p |])
