{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE KindSignatures   #-}

module Cont where

-- * Continuation Monad

newtype ContM a = ContM
  { unCont :: forall r. (a -> r) -> r }

getCont :: ContM a -> a
getCont (ContM x) = x id

instance Functor ContM where
  fmap :: (a -> b) -> ContM a -> ContM b
  fmap f (ContM u) = ContM (\f2 -> f2 (f (u id)))

instance Applicative ContM where
  pure a = ContM $ (\f -> f a)
  (ContM f) <*> a = fmap (f id) a

instance Monad ContM where
  return = pure
  (>>=) :: ContM a -> (a -> ContM b) -> ContM b
  (ContM a) >>= f = f (a id)

withVersionNumber :: (Double -> r) -> r
withVersionNumber f = f 1.0

withProgramName :: (String -> r) -> r
withProgramName f = f "Minefield"

withBuildNumber :: (Int -> r) -> r
withBuildNumber f = f 3434

releaseString = do
  p <- ContM withProgramName
  v <- ContM withVersionNumber
  b <- ContM withBuildNumber
  pure $ p ++ " v." ++ (show v) ++ " (build #" ++ (show b) ++ ")"
