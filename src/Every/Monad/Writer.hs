{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Every.Monad.Writer where

import Control.Monad
import Data.Maybe
import Control.Monad.Writer

data MyWriter w a = MyWriter w a
  deriving (Eq, Ord, Show)

instance Functor (MyWriter w)  where
  fmap f (MyWriter w a) = MyWriter w (f a)

instance Monoid w => Applicative (MyWriter w) where
  pure = MyWriter mempty
  (MyWriter w f) <*> (MyWriter w' a) = MyWriter (w <> w') (f a)

instance Monoid w  => Monad (MyWriter w) where
  left@(MyWriter w a) >>= f = merge left (f a)
    where
      merge (MyWriter w _) (MyWriter w' b) = MyWriter (w <> w') b

instance (Monoid w) => MonadWriter w (MyWriter w) where
  writer (a,  w) = MyWriter w a
  tell w = MyWriter w ()
  pass (MyWriter w (a, f)) = MyWriter (f w) a
  listen (MyWriter w a) = MyWriter w (a, w)

func1 :: MyWriter [String] ()
func1 = do
  tell ["Booting!"]
  tell ["Booting2!"]
  return ()

func2 :: MyWriter [String] Bool
func2 = do
  tell ["Evaling?"]
  return True

inst :: MyWriter [String] Int
inst = do
  tell ["Hello"]
  return 3
