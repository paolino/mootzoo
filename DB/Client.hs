{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Client where 

import Prelude hiding (readFile, putStrLn)
import Data.String
import Control.Monad
import Control.Monad.Writer
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Random
import Control.Monad.Error
import Control.Exception
import Data.ByteString (ByteString)
import DB.Operations
import DB0 -- (transactOnLogin, checkingLogin, MessageId, CheckLogin (..), ConnectionMonad, Login,Env)
import DB.Labels


follow' :: Env -> UserId -> MessageRow -> ConnectionMonad ()
follow' e ui mr@(MessageRow mi _ _ _ _ _ _ da) = do
    eexecute e "insert into client (user,message,data) values (?,?,?)" (ui,mi,da)
    ci <- lastRow e
    baseLabel e ci

retractEffect :: Env -> MessageRow -> ConnectionMonad ()
retractEffect e mr@(MessageRow mi _ _ _ Nothing _ _ _) = do
  eexecute e "delete from client where message=?" (Only mi)
retractEffect e mr@(MessageRow mi _ _ _ (Just mp) _ _ _) = do
  eexecute e "update client set message=? where message = ?" (mi,mp)
  
touch :: Env -> UserId -> MessageId -> ConnectionMonad ()
touch e ui mi = eexecute e "update client set data=(datetime('now')) where user=? and message=?" (ui,mi)

data Notification = Notification {
    branches :: [MessageId],
    updates :: Maybe (MessageId,Integer)
    }


data ClientPut
    = Follow Login MessageId
    | Unfollow Login MessageId
    | Store Login MessageId String ByteString

instance Put ClientPut where
  put e (Follow l mi) = transactOnLogin e l $ \ui  -> checkingMessage e mi $ \mr@(MessageRow _ _ _ _ _ _ co _) -> do
    r <- equery e "select conversation from client join messages on messages.id = client.message where client.user = ? and conversation =? " (ui,co)
    case r of
        [Only (_::ConvId)] -> throwError AlreadyFollowing
        _ -> return ()
    follow' e ui mr 
  put e (Unfollow l mi) =  transactOnLogin e l $ \ui  -> eexecute e "delete from client where user=? and message=?" (ui,mi)
  put e (Store l mi int v) = transactOnLogin e l $ \ui  -> do
    r <- equery e "select id from client where user=? and message=?" (ui,mi)
    case r of 
      [] -> throwError NotFollowing
      [Only (i::MessageId)] -> eexecute e "insert or update into blobs client,interface,blob values (?,?,?)" (i,int,v)

data ClientGet a where
  Following :: Login -> ClientGet [MessageId]
  Notificate :: Login -> MessageId -> ClientGet Notification
  Restore :: Login -> MessageId -> String -> ClientGet ByteString

runClientGet :: Env -> ClientGet b -> ConnectionMonad b
runClientGet e (Following l) = checkingLogin e l $ \(CheckLogin ui _ _)  -> map fromOnly <$> equery e "select message from client where user=?" (Only ui)
runClientGet e (Notificate l mi) = checkingLogin e l $ \(CheckLogin ui _ _)  -> checkingMessage e mi $ \mr@(MessageRow mi _ _ _ _ co _ _) -> do
    r1 <- equery e "select m1.id from messages as m1 join messages as m2 on  m1.parent = m2. id where m2.conversation = ? and m1.conversation <> m2.conversation" 
      (Only co)
    da <- equery e "select data from client where user=? and message=?" (ui,mi)
    r2 <- case da of
      [] -> throwError NotFollowing
      [Only (da::String)] -> equery e "select max(id),count(id) from messages where data > ? and conversation = ? group by conversation" (da,co)
    let r2' = case r2 of 
                [(Nothing,0)] -> Nothing
                [(Just x,n)] -> Just (x,n)
                _ -> Nothing
    return  $ Notification (map fromOnly r1) r2'
runClientGet e (Restore l mi int) = checkingLogin e l $ \(CheckLogin ui _ _)  -> do
    r <- equery e "select blob from blobs join client on client=id where user=? and message=? and interface = ? " (ui,mi,int)
    case r of
      [] -> throwError NotFollowing 
      [Only x] -> return x

  

instance Getter ClientGet where
  get e = WGet $ runClientGet e

