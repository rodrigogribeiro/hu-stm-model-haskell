module Machine where

import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Syntax

type Code = [Instr]

data Instr = Push Int
           | Add
           | Read Var
           | Write Var
           | Begin
           | Commit
           | Fork Code
           deriving (Eq, Ord, Show)


compileTran :: Tran -> Code -> Code
compileTran e c
  = case e of
      TVal i -> Push i : c
      x :++: y -> compileTran x (compileTran y (Add : c))
      TRead v -> Read v : c
      TWrite v t -> compileTran t (Write v : c)
      
compileProc :: Proc -> Code -> Code
compileProc e c
   = case e of
       PVal i -> Push i : c
       x :+: y -> compileProc x (compileProc y (Add : c))
       PAtomic t -> Begin : compileTran t (Commit : c)
       PFork p -> Fork (compileProc p []) : c


type Thread = (Code, Stack, Code, Log, Log)
type Stack = [Int]
type Log = Map Var Int


stepM :: Rel (Heap, [Thread])
stepM (h,[])
  = Set.empty
stepM (h,t : s)
  = (second (t :) `Set.map` stepM (h,s)) `Set.union` stepT t (h,s)

stepT :: Thread -> Rel (Heap, [Thread])
stepT ([], sig, f, r, w) _
  = Set.empty
stepT (i : c, sig, f, r, w) (h,s)
  = stepI i
    where
      ~(n : sig1) = sig
      ~(m : sig2) = sig1
      stepI (Fork c')
         = Set.singleton (h, (c, 0 : sig, f, r, w) : t : s)
           where t = (c', [], [], Map.empty, Map.empty)
      stepI (Push n')
         = Set.singleton (h, (c, n' : sig, f, r, w) : s)
      stepI Add
         = Set.singleton (h, (c, (n + m) : sig2, f, r, w) : s)
      stepI Begin
         = Set.singleton (h, (c, sig, Begin : c, r, w) : s)
      stepI (Read v)
         = Set.singleton (h, (c, n' : sig, f, r', w) : s)
           where
             (n',r') = case (Map.lookup v w, Map.lookup v r, h ? v) of
                          (Just m', _, _) -> (m', r)
                          (Nothing, Just m', _) -> (m', r)
                          (Nothing, Nothing, m') -> (m', Map.insert v m' r)
      stepI (Write v)
         = Set.singleton (h, (c, n : sig1, f, r, Map.insert v n w) : s)
      stepI Commit
         = Set.singleton (h', (c', sig', f, r, w) : s)
           where
             (h', c', sig') = case (r `Map.intersection` h) `Map.isSubmapOf` h of
                                  True -> (Map.unionWith (flip const) h w, c, n : sig1)
                                  False -> (h, f, sig)
      
haltedM :: (Heap, [Thread]) -> Maybe (Heap, [Int])
haltedM (h,s)
  = case traverse haltedT s of
      Just ns -> Just (h,ns)
      Nothing -> Nothing
    where
      haltedT ([], n : [], _ , _, _) = Just n
      haltedT _ = Nothing


exec :: REL (Heap, [Thread]) (Heap, [Int])
exec = reduceUntil haltedM stepM
