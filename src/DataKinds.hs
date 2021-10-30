{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module DataKinds where

import Data.List (intercalate)

data Nat = Zero | Succ Nat
  deriving(Eq, Ord, Show)

data Vec n a where
    Nil  ::                 Vec Zero a
    Cons :: a -> Vec n a -> Vec (Succ n) a

instance forall n a. (Show a) => Show (Vec n a) where
  show x = "[" ++ (show' x) ++ "]"
    where
      show' :: Show a => Vec n a -> String
      show' Nil = ""
      show' (Cons h t) = (show h) ++ ", " ++ (show' t)
