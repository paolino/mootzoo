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
        canVote :: Bool, -- feedback for the message
        canPropose :: Bool, -- open a new conversation on the message
        canIntervein :: Bool, -- open a conversation with the author
        canRespond :: Bool, -- respond to teh message
        canClose :: Bool, -- close the conversation for both
        canOpen :: Bool -- open a direct message  
        } deriving Show
                               
canRespond' :: UserId -> (MessageType,UserId,ConvId) -> Maybe (UserId,ConvId) -> Bool
canRespond' ui (ui',Closed,ci) (Just ((==) ui -> True,_,_)) = True
canRespond' _ _ _ = False

canClose' :: UserId -> (MessageType,UserId,ConvId) -> Maybe (UserId,ConvId) -> Bool
canClose' ui (ui',Closed,ci) (Just ((==) ui -> True,Closed,(==) ci -> True)) = True
canClose' _ _ _ = False

canOpen' :: UserId -> (MessageType,UserId,ConvId) -> Maybe (UserId,ConvId) -> Bool
canOpen' ui ((==) ui,Closed,ci) _ = True
canOpen' _ _ _ = False

canIntervein' :: UserId -> (MessageType,UserId,ConvId) -> Maybe (UserId,ConvId) -> Bool
canIntervein' ui ((==) ui -> False,Passage,ci) _ = True
canIntervein' _ _ _ = False

canPropose' :: UserId -> (MessageType,UserId,ConvId) -> Maybe (UserId,ConvId) -> Bool
canPropose' ui ((==) ui -> False,Passage,ci) _ = True
canPropose' _ _ _ = False

canIVote :: Env -> UserId -> MessageRow -> ConnectionMonad Bool
canIVote e ui (MessageRow mi _ ((/=) ui -> True) ((/=) Passage -> True) _ _ _) = (null :: [(Only UserId)] -> Bool) <$> equery e "select user from voting where user=? and message=?" (ui,mi)
canIVote e ui _ = return False

mkInterface :: Env -> UserId -> MessageRow -> ConnectionMonad Interface
mkInterface e ui mr@(MessageRow mi mt mu mt mmp mc mv) = do
  cv <- canIVote e ui mr
  v <- case mmp of
      Nothing -> return Nothing
      Just mp -> do
            r <- equery e "select user,type,conversation from messages where id=?" (Just mp)
            case r of
                [] -> throwError $ DatabaseError "parent missed"
                [x] -> return $ Just x
  let [cp,ci,cr,cc,co] = map (\f -> f ui (mu,mt,mc) v) [canPropose',canIntervein',canRespond',canClose',canOpen']
  return $ Interface cv cp ci cr cc cc co
      
        
  
data Exposed = Exposed MessageId String Integer Interface  deriving Show


-- messages for me
{-
getClosed :: Env -> Login -> ConnectionMonad [Exposed]
getClosed e l = transactOnLogin e l $ \ui -> equery e "select m1.id,0,m1.message,m1.vote from messages as m1 join messages as m2 on m1.parent = m2.id where m1.type = ? and m2.user = ?" (Closed,ui)

-- messages from me, Open and Closed
getEnvelopes :: Env -> Login -> ConnectionMonad [Exposed]
getEnvelopes e l = transactOnLogin e l $ \ui -> equery e 
        "select id,0,message,vote from messages as m1 where type <> ? and user = ?" (Passage,ui)
-}

mkExposed e ui mr@(MessageRow mi tx _ _ _ _ v) = Exposed mi tx v <$>  mkInterface e ui mr

getPast' :: Env -> UserId -> MessageId -> MessageRow -> Integer -> ConnectionMonad [Exposed]
getPast' e ui mi mr n =  mapM (mkExposed e ui mr) <$>  pastMessages e mi n

getPast :: Env -> Login -> MessageId -> Integer -> ConnectionMonad [Exposed]
getPast e l mi n = transactOnLogin e l $ \ui -> checkingMessage e mi $ \mr -> getPast' e ui mi mr n 

getFuture :: Env -> Login -> MessageId -> ConnectionMonad [Exposed] 
getFuture e l mi =  transactOnLogin e l $ \ui -> checkingMessage e mi $ \mr -> mapM (mkExposed e ui mr) <$> futureMessages e mi
                   
getConversation :: Env -> Login -> MessageId -> ConnectionMonad [Exposed]
getConversation e l 0 = transactOnLogin e l $ \ui -> do
        [(last,n)] <- equery e "select head,count from conversations limit=1" ()
        checkingMessage e last $ \mr -> getPast' e ui last mr n
getConversation e l mi = transactOnLogin e l $ \ui -> checkingMessage e mi $ \(MessageRow _ _ _ _ _ ci _) -> do
        [(last,n)] <- equery e "select head,count from conversations where id=?" (Only ci)
        checkingMessage e last $ \mr -> getPast' e ui last mr n
        
        
        


getOpens :: Env -> Login -> ConnectionMonad [Exposed]
getOpens e l = transactOnLogin e l $ \ui -> do 
        equery e "select m.id,v.user isnull,m.message,m.vote from messages as m left outer join voting as v on v.message = id where type = ? and m.user <> ? and v.user = ?" (Open,ui,ui)

