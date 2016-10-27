{-# LANGUAGE FlexibleInstances, TupleSections #-}

module Main where

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Syntax
import Machine

tests :: TestTree
tests = testGroup "Test suite"
            [
              testGroup "Unit tests"
                  [
                    testCase "Compiler code" $ compileProc (incTwice counter) [] @?= res
                  ]
            , testGroup "Quickcheck tests"
                  [
                    QC.testProperty "Compiler correctness" $ correctness
                  ]
            ]

main :: IO ()
main = defaultMain tests

-- sample test cases

increment :: Var -> Tran
increment c = TWrite c (TRead c :++: TVal 1)

incTwice :: Var -> Proc
incTwice c = PFork (PAtomic (increment c)) :+: PFork (PAtomic (increment c)) 

counter :: Var
counter = 0

res :: Code
res = [ Fork [Begin, Read counter, Push 1, Add, Write counter, Commit ]
      , Fork [Begin, Read counter, Push 1, Add, Write counter, Commit ]
      , Add]

-- arbitrary instances

instance Arbitrary Tran where
  arbitrary
    = sized genTran
  shrink = shrinkTran      
        

shrinkTran :: Tran -> [Tran]
shrinkTran ((TVal n) :++: (TVal m))
  = [TVal (n + m)]
shrinkTran (TWrite v t)
  = [TWrite v t' | t' <- shrinkTran t]
shrinkTran _ = []  

genTran :: Int -> Gen Tran
genTran n
   | n <= 0 = TVal <$> arbitrary
   | otherwise
      = frequency
          [
            (n + 1, TVal <$> arbitrary)
          , (n `div` 2, TRead <$> arbitrary)
          , (n `div` 2, TWrite <$> arbitrary <*> genTran (n - 1))
          , (n,  (:++:) <$> genTran n2 <*> genTran n2)
          ]
    where
      n2 = n `div` 2

instance Arbitrary Proc where
  arbitrary
    = sized genProc
  shrink = shrinkProc  

genProc :: Int -> Gen Proc
genProc n
   | n <= 0 = PVal <$> arbitrary
   | otherwise
       = frequency
           [
             (n + 1, PVal <$> arbitrary)
           , (n `div` 2, PFork <$> genProc (n - 1))
           , (n `div` 2, PAtomic <$> arbitrary)
           , (n, (:+:) <$> genProc n2 <*> genProc n2)
           ]
     where
       n2 = n `div` 2

shrinkProc :: Proc -> [Proc]
shrinkProc ((PVal n) :+: (PVal m))
   = [PVal (n + m)]
shrinkProc (PFork p)
   = [PFork p' | p' <- shrinkProc p]
shrinkProc (PAtomic t)
   = [PAtomic t' | t' <- shrinkTran t]
shrinkProc _ = []  

load :: [Proc] -> [Thread]
load = map (\p -> (compileProc p [], [], [], Map.empty, Map.empty))
       
correctness :: Proc -> Bool
correctness p = eval (Map.empty,[p]) == exec (Map.empty, load [p])
