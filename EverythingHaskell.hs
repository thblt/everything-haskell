{-# LANGUAGE TypeSynonymInstancesh #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

newtype Triplet a = Triplet { triple :: (a, a, a) }
  deriving(Eq, Read, Show)

instance Num a => Num (Triplet a) where
  (+) (Triplet (a, b, c)) (Triplet (a', b', c')) = Triplet (a + a', b + b', c + c')

type Triple a = (a, a, a)

type Triplish a = (a, a, a)

instance Num a => Num (Triple a) where
  (+) ((a, b, c)) ((a', b', c')) = (a + a', b + b', c + c')
  (-) ((a, b, c)) ((a', b', c')) = (a - a', b - b', c - c')
  (*) ((a, b, c)) ((a', b', c')) = (a * a', b * b', c * c')

instance Num a => Num (Triplish a) where
  (+) ((a, b, c)) ((a', b', c')) = (a + a', b + b', c + c')
  (-) ((a, b, c)) ((a', b', c')) = (a - a', b - b', c - c')
  (*) ((a, b, c)) ((a', b', c')) = (a * a', b * b', c * c')


a :: Triple
a = (1, 2, 3)

b :: Triple
b = (1, 2, 3)
