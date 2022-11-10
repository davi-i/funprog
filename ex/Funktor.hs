module Funktor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const


instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just $ f x

-- what about Either?

instance Funktor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right $ f y

-- what about pairs?

instance Funktor ((,) a) where
    fmap f (x, y) = (x, f y)

-- what about functions?

instance Funktor ((->) a) where
    fmap = (.)

-- what about Trees?

-- ...define Functor instances of as many * -> * things as you can think of!

