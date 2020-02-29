module HW05 where

{-

Name: 

Jonah Rubin

Collaboration Statement:

Talked with Josh Childs and Chris Deluca about the structure of the LetE and CallE case of E3

Collaboration on high-level ideas and approach on assignments is
encouraged. Copying someone else's work is not allowed. Copying solutions
from an online source is not allowed. Any collaboration or online
resources, even if used only a small amount, must be declared in your
assignment when you submit it. For example: “I discussed high-level
strategies for solving problem 2 and 5 with Alex; I found this
stackoverflow post (<link>) helpful while working on problem 3.” Students
caught copying work are eligible for immediate failure of the course and
disciplinary action by the University. All academic integrity misconduct
will be treated according to UVM's Code of Academic Integrity.

-}

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Util.Testing

import Util.Lex

import qualified Lang.L4 as L4

-- ========== --
-- SUBMITTING --
-- ========== --

-- How to submit:
--
-- Submit a copy of this file only “HWXX.hs” (for XX = 01, 02, etc.) to
-- gradescope.

-- ======== --
-- EXAMPLES --
-- ======== --

----------
-- [X1] --
----------

-- Fine the value associated with Walter in the map and add 1 to the result.

findWalterAddOne :: Map String Integer -> Maybe Integer
findWalterAddOne m = case Map.lookup "Walter" m of
  Just i -> Just (i + 1)
  Nothing -> Nothing

testX1 :: Test
testX1 = TestDir
  ( "X1"
  , "findWalterAddOne"
  , findWalterAddOne
  , "tests/hw05/x1"
  , parseTest (pMap pVar pInt) (pMaybe pInt)
  )

----------
-- [X2] -- 
----------

-- Write an interpreter for commands. A command just adds a new function
-- definition to the function environment.

interpCommand :: L4.FEnv -> L4.Command -> L4.FEnv
interpCommand fenv c = case c of
  L4.DefC fx xs e -> Map.insert fx (xs,e) fenv

testX2 :: Test
testX2 = TestDir
  ( "X2"
  , "interpCommand"
  , uncurry interpCommand
  , "tests/hw05/x2"
  , parseTest (pPair L4.pFEnv L4.pCommand) L4.pFEnv
  )

----------
-- [X3] --
----------

-- Combine words from two lists pairwise by adding a hyphen between pairwise
-- entries. You may find this helpful for E2.

combineWords :: [String] -> [String] -> Maybe [String]
combineWords ss1 ss2 = case (ss1,ss2) of
  ([],[]) -> Just []
  (s1:ss1',s2:ss2') -> case combineWords ss1' ss2' of
    Just ss -> Just ((s1 ++ "-" ++ s2) : ss)
    Nothing -> Nothing
  ([],_:_) -> Nothing
  (_:_,[]) -> Nothing


testX3 :: Test
testX3 = TestDir
  ( "X3"
  , "combineWords"
  , uncurry combineWords
  , "tests/hw05/x3"
  , parseTest (pPair (pMany pVar) (pMany pVar)) (pMaybe (pMany pVar))
  )

-- ========== --
-- ASSIGNMENT --
-- ========== --

-- ---------------
-- -- [E1]: ★☆☆ --
-- ---------------

-- Write a function that interprets many commands at once. Commands transform
-- an incoming function environment and add a new entry into the outgoing
-- function environment. See test cases for expected functionality.
-- use X2 (interpCommand)

interpCommandMany :: L4.FEnv -> [L4.Command] -> L4.FEnv
interpCommandMany fenv cs = case cs of
  [] -> fenv
  c:cs' -> interpCommandMany (interpCommand fenv c) cs'

testE1 :: Test
testE1 = TestDir
  ( "E1"
  , "interpCommandMany"
  , uncurry interpCommandMany
  , "tests/hw05/e1"
  , parseTest (pPair L4.pFEnv (pMany L4.pCommand)) L4.pFEnv
  )

---------------
-- [E2]: ★★☆ --
---------------

-- Write a function that extends a standard environment to map a list of names
-- to a list of values. If the number of names is different from the number of
-- values, this should fail by returning `Nothing`.
-- see E3

extendEnvMany :: [String] -> [L4.Value] -> L4.Env -> Maybe L4.Env
extendEnvMany xs vs env = case (xs, vs) of
  (x:xs', v:vs') -> extendEnvMany xs' vs' (Map.insert x v (env))
  ([], []) -> Just env
  (_, _) -> Nothing

testE2 :: Test
testE2 = TestDir
  ( "E2"
  , "extendEnvMany"
  , uncurry $ uncurry extendEnvMany
  , "tests/hw05/e2"
  , parseTest (pPair (pPair (pMany pVar) (pMany L4.pValue)) L4.pEnv) (pMaybe L4.pEnv)
  )

---------------
-- [E3]: ★★★ --
---------------

-- Write an interpreter for a language with integers, plus, times, booleans,
-- conditionals, variables, let-binding, function definitions and function
-- calls.
--
-- e ∈ expr ⩴ i | e + e | e * e | b | if e then e else e 
--          | x | let x = e in e
--          | f(e,…,e)
--
-- A helper function `interpExprMany` is provided for you which is mutually
-- recirsive with interpExpr, and will interpret multiple expressions to return
-- (maybe) multiple values.
--
-- A top-level driver `interp` is also provided which interprets a program by
-- first interpreting all of the commands, and then finally interpreting the
-- program expression. The test cases for this exercise are in terms of
-- `interp` (which provided to you and calls `interpExpr`), whereas you need to
-- implement `interpExpr`.

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
    Nothing -> L4.BadA
    Just a -> L4.ValueA a
  L4.LetE s e1 e2 -> case (interpExpr fenv env e1) of
    L4.ValueA v -> interpExpr fenv (Map.insert s v env) e2
    _ -> L4.BadA

  L4.CallE s e -> case (interpExprMany fenv env e) of
    Nothing -> L4.BadA
    Just [vs] -> case Map.lookup s fenv of
      Nothing -> L4.BadA
      Just ([s], e) -> case extendEnvMany [s] [vs] Map.empty of
       Just m -> interpExpr fenv m e
       Nothing -> L4.BadA

  _ -> undefined

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
    
testE3 :: Test
testE3 = TestDir
  ( "E3"
  , "interp"
  , interp
  , "tests/hw05/e3"
  , parseTest L4.pProgram L4.pAnswer
  )

main :: IO ()
main = runTests 
  -- examples
  [ testX1
  , testX2
  , testX3
  -- assignment
  , testE1
  , testE2
  , testE3
  ]
