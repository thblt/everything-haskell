{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Logger where

import Control.Monad.State
import Control.Monad.Identity

class Truc k where
  type GMap k :: * -> *

class Collection c i | c -> i where
  take :: c -> Maybe i
  empty :: c -> Bool

data Iterator i = Iterator Int [i]

instance Collection (Iterator i) i where
  take it@(Iterator i xs) | (empty it) = Nothing
                          | otherwise = Just . head . drop i $ xs

  empty (Iterator i xs)   | i > 0 = i >= (length xs)
                          | otherwise = False

class MyLogger logger where
  type LoggerMonad logger :: * -> *
  getHistory :: logger -> [String]
  log :: String -> LoggerMonad logger ()

newtype ListLogger = ListLogger [String]
instance MyLogger ListLogger where
  type (LoggerMonad ListLogger) = State ListLogger
  getHistory (ListLogger ss) = ss
  log :: String -> LoggerMonad ListLogger ()
  log m = do
    (ListLogger ms) <- get
    put $ ListLogger (m : ms)

newtype StdoutLogger = StdoutLogger ()
instance MyLogger StdoutLogger where
  type (LoggerMonad StdoutLogger) = IO
  getHistory = const []
  log = putStrLn
