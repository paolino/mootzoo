module Logic (
	-- | types
	User,
	Conversation,
	-- | data
	Message (Message),
	-- | construction
	boot,
	insert,
	construct,
	deconstruct
	)
	where

import Data.Maybe (catMaybes)

-- repeat a list forever
cycled [] = []
cycled xs = xs ++ cycled xs

type User = String



data Message = Message User String deriving Show

-- | a conversation builded right accepting new messages only if they are correct.
-- Correctness is 2 users only and alternated messages
data Conversation = Conversation {
		-- accept a new message fail when not correct
	 	accept' :: Message -> Maybe Conversation,
		-- | spits out messages
		deconstruct :: [Message]
		}
-- | insert a new message, failing on unacceptable user sequence
insert :: Message -> Conversation -> Maybe Conversation
insert = flip accept'

-- | an helper for multi message construction
construct :: [Message] -> Maybe Conversation
construct = foldl (\f x -> f >>= insert x) (Just boot)

make :: Maybe User -> Maybe User -> [String] -> Conversation
make u1 u2  xs = Conversation (accept u1 u2 xs) (ser u1 u2 xs)

-- | an empty Conversation
boot :: Conversation
boot = make Nothing Nothing []

accept :: Maybe User -> Maybe User -> [String] -> Message -> Maybe Conversation
-- primo messaggio
accept Nothing Nothing [] (Message u x) = Just $ make Nothing (Just u) [x]
-- secondo messaggio
accept Nothing (Just u2) xs (Message u x) 
	| u == u2 = Nothing
	| True =  Just (make (Just u2) (Just u) $ x:xs)
-- ennesimo messaggio
accept (Just u1) u2 xs (Message u x)
	| u == u1 = Just (make u2 (Just u1) $ x:xs)
	| otherwise = Nothing

-- ricostruzione
ser :: Maybe User -> Maybe User -> [String] -> [Message]
ser u1 u2 xs = zipWith Message (cycled $ catMaybes [u1,u2]) $ reverse xs	



