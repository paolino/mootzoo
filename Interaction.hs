module Interaction where


import Logic
import Browser

-- | number of sentences
type Window = Int
type 


data DB = DB {

	retrieve :: Int -> Maybe Conversation,
	update :: Int -> Conversation -> DB,
	

data Browsing = Browsing
	{	browser :: Browser	

data State = State 
	{	conversations :: [(Int,Browsing)]
	}

data Modify
	= Up Int
	| Down Int
	| Append String
	| Keep Int Bool
	| Refresh 
	| Taint Int Int


data Request 
	= SetUser String
	| GetPage 
	| GetConversation Int
	
data  Interaction a
	= Modification (Modify -> State -> State)
	| Interface (Request -> a

	
	
