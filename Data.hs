
{-# LANGUAGE DeriveFunctor #-}

import Data.List
import Data.List.Split
import Text.Read

type Vote = Int

data Message = Message String Vote

type User = String

-- data Messaged a = Messaged a [Message] deriving Functor

data Partecipation 
        = None
        | One User 
        | Two User User

start :: User -> Partecipation -> Color

start u None          = Blank
start u (One u') 
        | u == u'   = Personal
        | u /= u'   = Orphan
start u (Two u' u'') 
        | u == u'   = Conversata
        | u == u''  = Waiting
        | True      = Complete





type IUser = Integer

type IdConversation = Integer
type HashConversation = Integer

data CheckedConversation = CheckedConversation IdConversation HashConversation

data Method 
		=		Partecipate IdConversation
		|		Leave IdConversation
		|   Add CheckedConversation
		|		Remove CheckedConversation
		|		Vote CheckedConversation
		|		Prenote IdConversation
		|		Start IdConversation 


data Request = Get IUser Method | Post IUser Method Message

data UserConversation = UserConversation {
	messages :: [Message],
	color :: Color,
	prenoted :: Prenotation,
	hash :: HashConversation
  }

data Database m = IUser -> Request -> m (Maybe UserConversation)

data Response = Response [Message] Color Prenotation [Method]
type Application m = Database m -> 
 









renderMethod :: IUser -> Method -> String
renderMethod u (Partecipate id) =  intercalate "/" ["partecipate",show id,show u]
renderMethod u (Leave id) =  intercalate "/" ["leave",show id,show u]
renderMethod u (Add (CheckedConversation id ha)) =  intercalate "/" ["add",show id,show ha,show u]
renderMethod u (Remove (CheckedConversation id ha)) =  intercalate "/" ["remove",show id,show ha,show u]
renderMethod u (Vote (CheckedConversation id ha)) =  intercalate "/" ["vote",show id,show ha,show u]
renderMethod u (Prenote id) =  intercalate "/" ["prenote",show id,show u]
renderMethod u (Start id) =  intercalate "/" ["start",show id,show u]

parseMethod :: String -> Maybe (IUser,Method)
parseMethod x = case splitOn "/" x of
		[c,id,u] -> do 
				c' <- find (==c) ["partecipate","leave","prenote","start"] 
				id' <- readMaybe id 
				u' <- readMaybe u 
				case c of
					"partecipate" -> return (u',Partecipate id')
					"leave" -> return (u',Leave id')
					"prenote" -> return (u',Prenote id')
					"start" -> return (u',Start id')
					_ -> Nothing
		[c,id,ha,u] -> do 
				id' <- readMaybe id 
				ha' <- readMaybe ha 
				u' <- readMaybe u 
				case c of
					"add" -> return (u, (CheckedConversation id' ha'))
					"remove" -> return (u, (CheckedConversation id' ha'))
					"vote" -> return (u, (CheckedConversation id' ha'))
					_ -> Nothing
		_ -> Nothing
			




type Response m = IUser -> m (Maybe Rest)
data Database m = Database {
		startConversation :: Maybe IUser -> IdConversation -> m (Maybe UserConversation)
		queryConversation :: IUser -> Method -> m (Maybe UserConversation)
		}

data Interaction m = Interaction {
	start :: Database m -> IdConversation -> Response m 
	interact :: Database m -> Method -> Response m
	} 


requestConversation :: String -> m (Maybe String)
requestConversation 
