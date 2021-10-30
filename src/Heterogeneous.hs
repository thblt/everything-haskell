{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Heteroegeneous where

import Data.Kind (Constraint, Type)

-- * Heterogeneous list

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

instance All Eq ts => Eq (HList ts) where
  (t :# ts) == (t' :# ts') | t == t' = ts == ts'
  _ == _ = False

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare (t :# ts) (t' :# ts')
    | t == t' = compare ts ts'
    | otherwise = compare t t'

instance (All Show ts) => Show (HList ts) where
  show HNil = "[]"
  show (t :# HNil) = show t
  show (t :# ts) = show t ++ ", " ++ show ts

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

hTail :: HList (t ': ts) -> HList ts
hTail (_ :# ts) = ts

liste = 194 :# "Hello" :# (Just EQ) :# HNil

type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[] = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

type family All (c :: Type -> Constraint)
                (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)
