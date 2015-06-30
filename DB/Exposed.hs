{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Exposed where

import DB0
import DB.Votes
import Database.SQLite.Simple
import Control.Monad.Error

futureMessages :: Env -> Maybe MessageId  -> ConnectionMonad [MessageRow]
futureMessages e (Just mi) = equery e "select id,message,user,type,parent,conversation,vote,data from messages where parent=?" (Only mi)
futureMessages e Nothing = equery e "select id,message,user,type,parent,conversation,vote,data from messages where parent isnull"  ()

data Interface = Interface {
        canVote :: Bool, -- feedback for the message
        canPropose :: Bool, -- open a new conversation on the message
        canIntervein :: Bool, -- open a conversation with the author
        canRespond :: Bool, -- respond to teh message
        canClose :: Bool, -- close the conversation for both
        canOpen :: Bool, -- open a direct message  
        canRetract :: Bool -- retract a closed or open message
        } deriving Show
                               
canRespond' :: UserId -> (UserId,MessageType,ConvId) -> Maybe (UserId,MessageType,ConvId) -> Bool
canRespond' ui (ui',Closed,ci) (Just ((==) ui -> True,_,_)) = True
canRespond' ui ((/=) ui -> True,Open,ci) _ = True
canRespond' _ _ _ = False

canRetract' :: UserId -> (UserId,MessageType,ConvId) -> Maybe (UserId,MessageType,ConvId) -> Bool
canRetract' ui ((==) ui -> True,Closed,ci) _ = True
canRetract' ui ((==) ui -> True,Open,ci) _ = True
canRetract' _ _ _ = False

canClose' :: UserId -> (UserId,MessageType,ConvId) -> Maybe (UserId,MessageType,ConvId) -> Bool
canClose' ui (ui',Closed,ci) (Just ((==) ui -> True,Passage,(==) ci -> True)) = True
canClose' _ _ _ = False

canOpen' :: UserId -> (UserId,MessageType,ConvId) -> Maybe (UserId,MessageType,ConvId) -> Bool
canOpen' ui ((==) ui -> True,Closed,ci) _ = True
canOpen' _ _ _ = False

canIntervein' :: UserId -> (UserId,MessageType,ConvId) -> Maybe (UserId,MessageType,ConvId) -> Bool
canIntervein' ui ((==) ui -> False,Passage,ci) _ = True
canIntervein' _ _ _ = False

canPropose' :: UserId -> (UserId,MessageType,ConvId) -> Maybe (UserId,MessageType,ConvId) -> Bool
canPropose' ui ((==) ui -> False,Passage,ci) _ = True
canPropose' _ _ _ = False

canIVote :: Env -> UserId -> MessageRow -> ConnectionMonad Bool
canIVote e ui (MessageRow mi _ ((/=) ui -> True) ((/=) Passage -> True) _ _ _ _) = (null :: [(Only UserId)] -> Bool) <$> equery e "select user from voting where user=? and message=?" (ui,mi)
canIVote e ui _ = return False

mkInterface :: Env -> UserId -> MessageRow -> ConnectionMonad Interface
mkInterface e ui mr@(MessageRow mi mtx mu mt mmp mc mv _) = do
  cv <- canIVote e ui mr
  v <- case mmp of
      Nothing -> return Nothing
      Just mp -> do
            r <- equery e "select user,type,conversation from messages where id=?" (Only mp)
            case r of
                [] -> throwError $ DatabaseError "parent missed"
                [x] -> return $ Just x
  let [cp,ci,cr,cc,co,cx] = map (\f -> f ui (mu,mt,mc) v) [canPropose',canIntervein',canRespond',canClose',canOpen',canRetract']
  return $ Interface cv cp ci cr cc co cx
      
        

  
data Exposed = Exposed {
    eid :: MessageId,
    edate :: String,
    empid :: Maybe MessageId,
    etext :: String,
    evote ::  Integer,
    ealternative :: [MessageId],
    einterface :: Interface
    }
  deriving Show

mkExposed  e ui mr@(MessageRow mi tx mu mt mmp co v md) = do        
        fs <- case mmp of
            Nothing -> return []
            Just mi' -> map (\(MessageRow mi'' _ _ _ _ co'' _ _) -> mi'') <$> futureMessages e (Just mi') 
        Exposed mi md mmp tx v  fs <$> mkInterface e ui mr

