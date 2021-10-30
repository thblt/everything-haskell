{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Existentials where

import Data.Typeable
import Data.Kind (Constraint, Type)

-- * A Show Existential

data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  show = elimHasShow show

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

-- * Dynamic

data Dynamic where
  Dynamic :: Typeable t => t -> Dynamic

elimDynamic :: (forall a. Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic x) = f x

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2 :: forall a b r. (Typeable a, Typeable b, Typeable r)
  => Dynamic
  -> Dynamic
  -> (a -> b -> r)
  -> Maybe Dynamic
liftD2 a b f = fmap Dynamic . f
  <$> fromDynamic @a a
  <*> fromDynamic @b b

-- * Generalization

data Has (c :: Type -> Constraint) where
  Has :: c t => t -> Has c

elimHas :: (forall a. c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a
