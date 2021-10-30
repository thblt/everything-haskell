{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Printf where

extra :: (Monoid m) => (m -> m) -> (m -> m -> m)
extra f a b = f (a <> b)

a :: Int
a = 33
b :: Int
b = 52
plus1 a = a + 1
