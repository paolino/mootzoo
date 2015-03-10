{-# LANGUAGE FlexibleInstances #-}
data Reader r a = Reader (r -> a)

instance Functor r where
	fmap f (Reader g) = Reader (f . g)
