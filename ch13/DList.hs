-- Difference List 이용하는 예제. Partially applied function 형태를
-- 그대로 사용할 수 없으므로, 이를 적절하게 변형하여 이용하는 방법에
-- 대해 알아본다.

module DList
  (DList
  , fromList
  , toList
  , empty
  , append
  , cons
  , dfoldr ) where

import Monoid

newtype DList a =
  DL { unDL :: [a] -> [a] }

append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

append' :: DList a -> DList a -> DList a
append' (DL xs) (DL ys) = DL (xs . ys)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []

empty :: DList a
empty = DL id

cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)
infixr `cons`

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xz = foldr f z (toList xz)

safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
  (y:_) -> Just y
  _ -> Nothing

dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
  where go x xs = cons (f x) xs
instance Functor DList where
  fmap = dmap

instance Monoid (DList a) where
  mempty = empty
  mappend = append
