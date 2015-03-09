{-# LANGUAGE ViewPatterns #-}

module Browser where

import Data.List (tails)

import Logic


data Zipper a = Zipper [a] a [a]

value (Zipper _ x _) = x

left (Zipper [] _ _) = Nothing
left (Zipper (x:xs) y zs) = Just (Zipper xs x (y:zs))

right (Zipper _ _ []) = Nothing
right (Zipper xs y (z:zs)) = Just (Zipper (y:xs) z zs)

zipper [] = Nothing
zipper (x:xs) = Just $ Zipper [] x xs

data Browser = Browser
	{	down ::  Either Conversation Browser
	,	up :: Maybe Browser
	,	now :: [Message]
	}
mark = zipWith f (cycle ["uno","due"]) where
	f x (Message _ m) = Message x m

browser :: Int -> Conversation -> Maybe Browser
browser n c  = let
	down (right -> Just z) =  Right $ Browser (down z) (up z) $ value z
	down z  = Left c
	up (left -> Just z) = Just $ Browser (down z) (up z) $ value z
	up z = Nothing
	in do
		z <- zipper $ map (take n) . tails $ mark $  deconstruct c
		return $ Browser (down z) (up z) $ value z

