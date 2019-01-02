{-# LANGUAGE UndecidableInstances #-}
module Scorch.TypeLevel.Disjoint where

import Data.Kind
import GHC.TypeLits

type family FlattenTree p a :: [Type] where
  FlattenTree p (p x y) = p x y : FlattenTree p x ++ FlattenTree p y
  FlattenTree p a = '[a]

type family Disjoint p a b :: Constraint where
  Disjoint p a b = DisjointSets (FlattenTree p a) (FlattenTree p b)

type DisjointSets as bs = DisjointSetsHelper (FindCommonElem as bs) as bs

type family DisjointSetsHelper e as bs :: Constraint where
  DisjointSetsHelper Nothing as bs = ()
  DisjointSetsHelper (Just e) as bs = TypeError (DisjointSetsError e as bs)

type DisjointSetsError e as bs
  = Text "The following sets of types should be disjoint:"
  :$$: Text "{" :<>: ShowTypeList as :<>: Text "}"
  :$$: Text "{" :<>: ShowTypeList bs :<>: Text "}"
  :$$: Text "But they both contain " :<>: ShowType e

type family ShowTypeList xs :: ErrorMessage where
  ShowTypeList '[] = Text ""
  ShowTypeList '[x] = ShowType x
  ShowTypeList (x : xs) = ShowType x :<>: Text ", " :<>: ShowTypeList xs

type family FindCommonElem (as :: [k]) (bs :: [k]) :: Maybe k where
  FindCommonElem '[] bs = Nothing
  FindCommonElem as '[] = Nothing
  FindCommonElem (e : as) (e : bs) = Just e
  FindCommonElem (a : as) (b : bs) = FindIn a bs <|> FindIn b as <|> FindCommonElem as bs

type family FindIn (x :: k) (ys :: [k]) :: Maybe k where
  FindIn x '[] = Nothing
  FindIn x (x : ys) = Just x
  FindIn x (y : ys) = FindIn x ys

type family (<|>) (x :: Maybe k) (y :: Maybe k) :: Maybe k where
  (<|>) (Just x) y = Just x
  (<|>) Nothing y = y

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  (++) '[] ys = ys
  (++) (x : xs) ys = x : (xs ++ ys)