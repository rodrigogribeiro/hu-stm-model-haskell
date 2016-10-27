{-#LANGUAGE ScopedTypeVariables #-}

module Syntax where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set

type Var = Int

infixl 6 :++:
infixl 6 :+:

data Tran
    = TVal Int
    | TRead Var
    | TWrite Var Tran
    | Tran :++: Tran
    deriving (Eq, Ord, Show)

data Proc
    = PVal Int
    | PFork Proc
    | PAtomic Tran
    | Proc :+: Proc
    deriving (Eq, Ord, Show)  

type Soup = [Proc]

type Heap = Map Var Int

(?) :: Heap -> Var -> Int
h ? v = Map.findWithDefault 0 v h

type REL a b = a -> Set b
type Rel a = REL a a

reduceTran :: Rel (Heap, Tran)
reduceTran (h, TVal _)
   = Set.empty 
reduceTran (h, TRead v)
   = Set.singleton (h, TVal (h ? v))
reduceTran (h, TWrite v t)
   = case t of
       TVal n -> Set.singleton (Map.insert v n h, TVal n)
       _ -> second (TWrite v) `Set.map` reduceTran (h,t)
reduceTran (h, a :++: b)
   = case (a,b) of
       (TVal m, TVal n) -> Set.singleton (h, TVal (n + m))
       (TVal m, _) -> second (TVal m :++:) `Set.map` reduceTran (h,b)
       (_,_) -> second (:++: b) `Set.map` reduceTran (h,a)

joinSet :: Ord a => Set (Set a) -> Set a
joinSet = Set.fold Set.union Set.empty

reduceUntil :: (Ord a, Ord b) => (a -> Maybe b) -> Rel a -> REL a b
reduceUntil p reduce init
   = step (Set.singleton init, Set.empty)
     where
       --step :: (Set a, Set b) -> Set b
       step (running, finished)
           = case Set.null running of
                 True -> finished
                 False -> step (first (joinSet . Set.map reduce)
                                      (Set.fold partition (Set.empty,finished) running))

       --partition :: a -> (Set a, Set b) -> (Set a, Set b)
       partition e
           = case p e of
               Nothing -> first (Set.insert e)
               Just n  -> second (Set.insert n)


isValT :: (Heap,Tran) -> Maybe (Heap, Int)
isValT (h,TVal n) = Just (h,n)
isValT _ = Nothing


reduceSoup :: Rel (Heap, Soup)
reduceSoup (h, [])
   = Set.empty
reduceSoup (h, p : s)
   = (second (p :) `Set.map` reduceSoup (h,s)) `Set.union` reduceProc p (h,s)
   

reduceProc :: Proc -> Rel (Heap, [Proc])
reduceProc (PVal n) _
   = Set.empty
reduceProc (PFork p) (h,s)
   = Set.singleton (h, PVal 0 : p : s)
reduceProc (PAtomic t) (h,s)
   = second (\n -> PVal n : s) `Set.map` reduceUntil isValT reduceTran (h,t)
reduceProc (a :+: b) (h,s)
   = case (a,b) of
       (PVal m, PVal n) -> Set.singleton (h, PVal (m + n) : s)
       (PVal m, _) -> second (mapHead (PVal m :+:)) `Set.map` reduceSoup (h,b:s)
       (_,_) -> second (mapHead (:+: b)) `Set.map` reduceSoup (h,a:s)
     where
       mapHead f (p : s) = f p : s


isValSoup :: (Heap, [Proc]) -> Maybe (Heap, [Int])
isValSoup (h,s)
   = case traverse isValProc s of
         Nothing -> Nothing
         Just ns -> Just (h,ns)

     where
       isValProc :: Proc -> Maybe Int
       isValProc (PVal n) = Just n
       isValProc _ = Nothing
          
eval :: REL (Heap, [Proc]) (Heap, [Int])
eval = reduceUntil isValSoup reduceSoup
