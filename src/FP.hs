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
-- Translators --
-----------------

-- Expr
translateE :: LM.Expr -> L5.Expr
translateE e = case e of
  LM.IntE i -> L5.IntE i
  LM.PlusE e1 e2 -> L5.PlusE (translateE e1) (translateE e2)
  LM.TimesE e1 e2 -> L5.TimesE (translateE e1) (translateE e2)
  LM.BoolE b -> L5.BoolE b
  LM.IfE e1 e2 e3 -> L5.IfE (translateE e1) (translateE e2) (translateE e3)
  LM.VarE s -> L5.VarE s
  LM.LetE s e1 e2 -> L5.LetE s (translateE e1) (translateE e2)
  LM.FunE s t e -> L5.FunE s (translateE e)
  LM.AppE e1 e2 -> L5.AppE (translateE e1) (translateE e2)
  --NEW
  LM.PairE e1 e2 -> L5.PairE (translateE e1) (translateE e2)
  LM.FstE e -> L5.FstE (translateE e)
  LM.SndE e -> L5.SndE (translateE e)
  LM.CaseE e1 s1 e2 s2 e3-> L5.CaseE (translateE e1) s1 (translateE e2) s2 (translateE e3)
  LM.LeftE t e -> L5.LeftE  (translateE e)
  LM.RightE t e -> L5.RightE (translateE e)
  LM.StringE s -> L5.StringE s
  _ -> error "not implemented"

-- Type
translateT ::  LM.Type -> L5.Type
translateT t = case t of
  LM.IntT -> L5.IntT 
  LM.BoolT -> L5.IntT
  LM.StringT -> L5.StringT
  LM.FunT t1 t2 -> L5.FunT (translateT t1) (translateT t2)
  LM.PairT t1 t2 -> L5.PairT (translateT t1) (translateT t2)
  LM.TUnionT t1 t2 -> L5.TUnionT (translateT t1) (translateT t2)
  _ -> error "not implemented"

--Value
translateV :: LM.Value -> L5.ValueE
translateV v = case v of
  LM.IntV i -> L5.IntEV i
  LM.BoolV b -> L5.BoolEV b
  LM.StringV s -> L5.StringEV s
  LM.PairV p1 p2  -> L5.PairEV (translateV p1) (translateV p2)
  LM.LeftV l -> L5.LeftEV (translateV l)
  LM.RightV r -> L5.RightEV (translateV r)
  _ -> error "not implemented"

--Answer
translateA :: LM.Answer -> L5.AnswerE
translateA a = case a of
  LM.SuccessA s v -> L5.ValueEA (translateV v)
  LM.BadA -> L5.BadEA
  _ -> error "not implemented"

-----------------
-- Interpreter --
-----------------
interpWithEnv :: L5.EnvE -> L5.Expr -> L5.AnswerE
interpWithEnv env e = case e of
  L5.IntE i -> L5.ValueEA (L5.IntEV i)
  L5.StringE x -> L5.ValueEA (L5.StringEV x)
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

  L5.CaseE e1 x1 e2 x2 e3 -> case interpWithEnv env e1 of
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
typeCheck :: L5.EnvT -> L5.Expr -> Maybe (L5.Type)
typeCheck env e = case e of
  L5.IntE i -> Just (L5.IntT)
  L5.PlusE e1 e2 -> case (typeCheck env e1, typeCheck env e2) of
    (Just (L5.IntT), Just (L5.IntT)) -> Just (L5.IntT)
    (_,_) -> Nothing
  L5.BoolE b -> Just (L5.BoolT)
  L5.StringE x -> (Map.lookup x env)
  L5.IfE b e1 e2 -> case typeCheck env b of
    Just (L5.BoolT) -> Nothing
    _ -> Nothing
    
  L5.LetE s e1 e2 -> case typeCheck env e1 of
    Just (t1) -> (typeCheck (Map.insert s t1 env) e2)
    _ -> Nothing
         
  _ -> Nothing

{-
test1 :: Test
test1 = TestDir
  ( "T1"                 -- e.g., "T1"
  , "identity function"  -- e.g., "interp"
  , (\ e -> interpWithEnv Map.empty (translateE e))  -- the function, e.g., (\ e -> interp e Map.empty Map.empty)
  , "tests/fp"           -- the directory where tests live, e.g., "tests/fp/t1"
  , parseTest LM.pExpr LM.pAnswer
  )
-}
test2 :: Test
test2 = Test1
  ( "T1"
  , "Interp Tester"
  , interpWithEnv Map.empty
  , [ -- test input
      ( translateE([lme| let p = (1,2) in snd p |])
      -- expeced output
      , translateA([lma| <success> {} , 2 |])
      )
    ,
      ( translateE([lme| let x = 1 in x |])
      -- expeced output
      , translateA([lma| <success> {} , 1 |])
      )
    ,
      ( translateE([lme| let p = (1+1, 2+2) in
                         let p2 = (3, 5) in
                         fst p2 * fst p + snd p2 |])
      -- expeced output
      , translateA([lma| <success> {} , 11 |])
      )
    
    ,
      ( translateE([lme|let tu1 = left 4 in
                       let tu2 = right false in
                       let r1 = case tu1 {left x => x * x} {right x => if x then 1 else 2} in
                       let r2 = case tu2 {left x => x * x} {right x => if x then 1 else 2} in
                       r1 + r2 |])
      -- expeced output
      , translateA([lma| <success> {} , 18 |])
      )

    
       ,
      ( translateE([lme| let p =
                       (
                         let x = case left 4 {left x => x * x} {right x => if x then 1 else 2}
                         in x
                       ,
                         let y = case right true {left y => y + 3} {right y => if y then 1 else 2}
                         in y
                       ) in snd p |])
      -- expeced output
      , translateA([lma| <success> {} , 1 |])
      )


    ]
  )



test4 :: Test
test4 = Test1
  ( "T2"
  , "Type Check Tester"
  , typeCheck Map.empty
  , [ -- test input
      (translateE([lme| 2 * 1 |])
      -- expeced output
      ,
      (translateT([lmt| int * bool |]))
      )
    ]
  )


main :: IO ()
main = do
  putStrLn "TESTS"
  runTests 
    [
       --test1,
       test2
    ]

  putStrLn "\n"
  putStrLn "EX"

  putStrLn (show [lme| let p = (1,2) in fst p |])
  putStrLn (show [lmt| int * bool |])
  putStrLn (show [lmv| < fun x => y + 1 , {y = 2} > |])
  putStrLn (show [lma| <success> { loc 2 = 4 } , 1 |])
  putStrLn (show [lma| <error> { loc 2 = 4 } , "message" |])
  putStrLn (show [lma| <bad> |])
  putStrLn (show [lme| fun x : int => x + x |])
  putStrLn (show [lme| let p = (1*1, 2+2) in 
                       fst p * fst p + snd p |])
  putStrLn (show [lme| let tu2 = right false in 
                       let r2 = case tu2 {left x => x * x} {right x => if x then 1 else 2} in
                       r2 |])
