
{-# LANGUAGE DeriveFunctor #-}

type Vote = Int

data Message = Message String Vote [User]

type User = String

data Messaged a = Messaged a [Message] deriving Functor

data Partecipation 
        = None
        | One User 
        | Two User User

data Color 
        = Blank
        | Orphan
        | Personal
        | Waiting
        | Complete
        | Conversata
        deriving Show

get' :: User -> Partecipation -> Color

get' u None          = Blank
get' u (One u') 
        | u == u'   = Personal
        | u /= u'   = Orphan
get' u (Two u' u'') 
        | u == u'   = Conversata
        | u == u''  = Waiting
        | True      = Complete

get :: User -> Messaged Partecipation -> Messaged Color
get u = fmap (get' u)

put :: Maybe Message -> User -> Color -> Messaged Partecipation -> Messaged Partecipation
put (Just x) u  Personal     (Messaged None xs)                    
                 = Messaged (One u) (x:xs)
put Nothing  u  Blank      m@(Messaged (One u') (x:xs))      
        |u == u' = Messaged None xs
        |True    = m
put (Just x) u  Conversata m@(Messaged (One u') xs)     
        |u /= u' = Messaged (Two u' u) (x:xs)
        |True    = m
put Nothing u  Orphan      m@(Messaged (Two u' u'') (x:xs))  
        |u == u''= Messaged (One u') xs
        |u == u' = Messaged (One u'') xs
        |True    = m
put _ _ m = m 

type IM = Int
putVote :: User -> IM -> Bool -> Messaged Partecipation -> Messaged Partecipation
putVote u i b ms = let
        op = if b then (+1) else (subtract 1)
        
