{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module DB.Get where 

import Prelude hiding (readFile, putStrLn)
import Control.Applicative
import Data.String
import Control.Monad
import Control.Monad.Writer
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Random
import Data.Typeable
import Control.Exception
import Control.Monad.Error
import DB0

                
data Interface = Interface {
        canVote :: Bool,
        canDispose :: Bool,
        canRespond :: Bool,
        canRequest :: Bool
        }
                               

data Exposed = Exposed MessageId Bool String Integer  deriving Show

instance FromRow Exposed where
        fromRow = Exposed <$> field <*> field <*> field <*> field

-- messages for me

getClosed :: Env -> Login -> ConnectionMonad [Exposed]
getClosed e l = transactOnLogin e l $ \ui -> equery e "select m1.id,0,m1.message,m1.vote from messages as m1 join messages as m2 on m1.parent = m2.id where m1.type = ? and m2.user = ?" (Closed,ui)

-- messages from me, Open and Closed
getEnvelopes :: Env -> Login -> ConnectionMonad [Exposed]
getEnvelopes e l = transactOnLogin e l $ \ui -> equery e 
        "select id,0,message,vote from messages as m1 where type <> ? and user = ?" (Passage,ui)


canIVote :: Env -> UserId -> MessageRow -> ConnectionMonad Bool
canIVote e ui (MessageRow mi _ ((/=) ui -> True) ((/=) Passage -> True) _ _ _) = (null :: [(Only UserId)] -> Bool) <$> equery e "select user from voting where user=? and message=?" (ui,mi)
canIVote e ui _ = return False

fromMessageRow b (MessageRow mi tx _ _ _ _ v) = Exposed mi b tx v

getPast' :: Env -> UserId -> MessageId -> MessageRow -> Integer -> ConnectionMonad [Exposed]
getPast' e ui mi mr n =  do
        b <- canIVote e ui mr
        ps <- pastMessages e mi n
        return $ zipWith fromMessageRow (b:repeat False) ps 

getPast :: Env -> Login -> MessageId -> Integer -> ConnectionMonad [Exposed]
getPast e l mi n = transactOnLogin e l $ \ui -> checkingMessage e mi $ \mr -> do
        b <- canIVote e ui mr
        ps <- pastMessages e mi n
        return $ zipWith fromMessageRow (b:repeat False) ps 

getFuture :: Env -> Login -> MessageId -> ConnectionMonad [Exposed] 
getFuture e l mi =  transactOnLogin e l $ \ui -> checkingMessage e mi $ \_ -> do
        ps <- futureMessages e mi
        forM ps $ \mr -> do
                b <- canIVote e ui mr
                return $ fromMessageRow b mr 
getConversation :: Env -> Login -> MessageId -> ConnectionMonad [Exposed]
getConversation e l mi = transactOnLogin e l $ \ui -> checkingMessage e mi $ \(MessageRow _ _ _ _ _ ci _) -> do
        [(last,n)] <- equery e "select head,count from conversations where id=?" (Only ci)
        checkingMessage e last $ \mr -> getPast' e ui last mr n
        
        
        


getOpens :: Env -> Login -> ConnectionMonad [Exposed]
getOpens e l = transactOnLogin e l $ \ui -> do 
        equery e "select m.id,v.user isnull,m.message,m.vote from messages as m left outer join voting as v on v.message = id where type = ? and m.user <> ? and v.user = ?" (Open,ui,ui)

