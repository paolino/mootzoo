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
canClose' ui (ui',Closed,ci) (Just ((==) ui -> True,Closed,(==) ci -> True)) = True
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
canIVote e ui (MessageRow mi _ ((/=) ui -> True) ((/=) Passage -> True) _ _ _) = (null :: [(Only UserId)] -> Bool) <$> equery e "select user from voting where user=? and message=?" (ui,mi)
canIVote e ui _ = return False

mkInterface :: Env -> UserId -> MessageRow -> ConnectionMonad Interface
mkInterface e ui mr@(MessageRow mi mtx mu mt mmp mc mv) = do
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
      
        
  
data Exposed = Exposed MessageId (Maybe MessageId) [Exposed] String Integer Interface  deriving Show


-- messages for me
{-
getClosed :: Env -> Login -> ConnectionMonad [Exposed]
getClosed e l = transactOnLogin e l $ \ui -> equery e "select m1.id,0,m1.message,m1.vote from messages as m1 join messages as m2 on m1.parent = m2.id where m1.type = ? and m2.user = ?" (Closed,ui)

-- messages from me, Open and Closed
getEnvelopes :: Env -> Login -> ConnectionMonad [Exposed]
getEnvelopes e l = transactOnLogin e l $ \ui -> equery e 
        "select id,0,message,vote from messages as m1 where type <> ? and user = ?" (Passage,ui)
-}

mkExposed 0 e ui mr@(MessageRow mi tx mu mt mmp _ v) = Exposed mi mmp [] tx v <$>  mkInterface e ui mr
mkExposed k e ui mr@(MessageRow mi tx mu mt mmp _ v) = do
        fs <-futureMessages e (Just mi) >>=  mapM (mkExposed (k - 1) e ui) 
        Exposed mi mmp fs tx v <$>  mkInterface e ui mr

getPast' :: Env -> UserId -> MessageId -> Integer -> Integer -> ConnectionMonad [Exposed]
getPast' e ui mi n k =  pastMessages e mi n >>= (mapM (mkExposed k e ui) . reverse)

getPast :: Env -> Login -> MessageId -> Integer -> ConnectionMonad [Exposed]
getPast e l mi n = transactOnLogin e l $ \ui -> checkingMessage e mi $ \_ -> getPast' e ui mi  n 1

getFuture :: Env -> Login -> MessageId -> ConnectionMonad [Exposed] 
getFuture e l mi =  transactOnLogin e l $ \ui -> checkingMessage e mi $ \mr ->  futureMessages e (Just mi) >>= mapM (mkExposed 0 e ui) 

getConversation :: Env -> Login -> MessageId -> Integer -> ConnectionMonad [Exposed]
getConversation e l 0 k = transactOnLogin e l $ \ui -> do
        r <- equery e "select head,count from conversations limit 1" ()
        case r of 
            [(last,n)]  -> checkingMessage e last $ \_ -> getPast' e ui last n k
            [] -> throwError UnknownIdMessage
getConversation e l mi k = transactOnLogin e l $ \ui -> checkingMessage e mi $ \(MessageRow _ _ _ _ _ ci _) -> do
        [(last,n)] <- equery e "select head,count from conversations where id=?" (Only ci)
        checkingMessage e last $ \_ -> getPast' e ui last  n k
        
        
        


getRoots :: Env -> Login -> Integer -> ConnectionMonad [Exposed]
getRoots e l k = transactOnLogin e l $ \ui -> futureMessages e Nothing >>= mapM (mkExposed k e ui) 

