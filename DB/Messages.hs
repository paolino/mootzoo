{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module DB.Messages where 

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
import Lib
import DB0
import DB.Client
import DB.Exposed

pastMessages :: Env -> MessageId -> ConnectionMonad [MessageRow]
pastMessages e mi = do
        checkingMessage e mi $ \_ -> return () 
        equery e "with recursive ex(id,parent,message,vote,type,user,conversation,data) as (select id,parent,message,vote,type,user,conversation,data from messages where messages.id=?  union all select messages.id,messages.parent,messages.message,messages.vote,messages.type,messages.user,messages.conversation,messages.data from messages,ex where messages.id=ex.parent) select id,message,user,type,parent,conversation,vote,data from ex" (Only mi)



newConversation :: Env -> MessageId -> ConnectionMonad ConvId
newConversation e t = do
        eexecute e "insert into conversations values (null,?,?,?)" (t,t,1::Integer)
        lastRow e

data Attach 
        = Attach MessageId
        | DontAttach
        | Correct MessageId
        | Pass MessageId
        deriving Read

newMessage ::  Env -> UserId  -> Attach -> String -> ConnectionMonad ()
newMessage e ui DontAttach x = do
        eexecute e "insert into messages (id,message,user,type,parent,conversation) values (null,?,?,?,null,null)" (x,ui,Open) -- public message
        mi <- lastRow e
        tell [EvNewMessage mi]
        ci <- newConversation e mi 
        eexecute e "update messages set conversation = ? where id=?" (ci,mi)
        checkingMessage e mi $ follow' e ui 
        
newMessage e ui (Attach mi) x = do
        let insert  ci = do 
                eexecute e "update messages set type=? where id=?" (Passage,mi) 
                eexecute e "insert into messages (id,message,user,type,parent,conversation)  values (null,?,?,?,?,?)" (x,ui,Closed,mi,ci)
                mi'' <- lastRow e
                tell [EvNewMessage mi'']
                eexecute e "update conversations set head=? , count = count + 1 where id=?" (mi'',ci)

        r <- equery e "select type,parent,user,conversation from messages where id=?" (Only mi)
        case r :: [(MessageType,Maybe UserId,UserId,ConvId)] of
                [(_,_,eq ui -> True,_)] -> throwError Proponent
                [(Closed,Just mi',_,ci)] -> do
                        r' <- equery e "select user from messages where id=?" (Only mi')
                        case r' of
                                [Only (eq ui -> True)] -> insert ci 
                                [_] -> throwError NotOpponent
                [(Open,_,_,ci)] ->  insert ci >> checkingMessage e mi (follow' e ui)
                [(Passage,_,ui',_)] -> do
                                eexecute e "insert into messages (id,message,user,type,parent,conversation)  values (null,?,?,?,?,null)" (x,ui,Closed,mi)
                                mi' <- lastRow e
                                tell [EvNewMessage mi']
                                ci <- newConversation e mi'
                                eexecute e "update messages set conversation = ? where id=?" (ci,mi')
                                checkingMessage e mi $ \mr -> do
                                  follow' e ui mr
                                  follow' e ui' mr
    
                                -- check conversation
                [_] -> throwError NotAttachable
                [] -> throwError UnknownIdMessage

newMessage e ui (Correct mi) x = do
        r <- equery e "select type,user from messages where id=?" (Only mi)
        case r :: [(MessageType,UserId)] of
                [(Passage,_)] -> throwError IsPassage
                [(_, (==ui) -> True)] -> do
                        eexecute e "update messages set message = ? where id=?" (x,mi)
                [_] -> throwError NotProponent
                [] -> throwError UnknownIdMessage

        
retractMessage :: Env -> Login -> MessageId -> ConnectionMonad ()
retractMessage e l mi =  transactOnLogin e l $ \ui -> checkingMessage e mi $ \_ -> do
        r <- equery e "select type,user,parent,conversation from messages where id=?" (Only mi)
        case r :: [(MessageType,UserId,Maybe MessageId,ConvId)] of
                [(Passage,_,_,_)] -> throwError IsPassage
                [(Open, (==ui) -> True,_,ci)] ->  do
                        checkingMessage e mi $ retractEffect e 
                        eexecute e "delete from conversations where id=?" (Only ci)
                        eexecute e "delete from messages where id=?" (Only mi)
                [(Closed, (==ui) -> True,Just mi',ci)] ->  do
                        r <- equery e "select conversation from messages where id=?" (Only mi')
                        case r :: [Only ConvId] of
                                [Only ((== ci) -> False)] -> eexecute e "delete from conversations where id=?" (Only ci)
                                [Only ((== ci) -> True)] -> eexecute e "update conversations set head=? , count = count - 1 where id=?" (mi',ci)
                        checkingMessage e mi $ retractEffect e 
                        eexecute e "delete from messages where id=?" (Only mi) 
                [_] -> throwError NotProponent
                [] -> throwError UnknownIdMessage

data Dispose = Diffuse | Accept deriving Read

disposeMessage ::  Env -> Login -> Dispose -> MessageId -> ConnectionMonad ()
disposeMessage e l Accept mi = transactOnLogin e l $ \ui -> checkingMessage e mi $ \_ -> do
                        r <- equery e "select type,parent from messages where id=?" (Only mi)
                        case r :: [(MessageType,Maybe MessageId)] of
                                [(Closed,Just mi')] ->  do
                                        r' <- equery e "select user from messages where id=?" (Only mi')
                                        case r' of
                                                [Only ((==) ui -> True)] -> eexecute e "update messages set type=? where id=?" (Passage,mi)       
                                                [_] -> throwError NotOpponent
                                                _ -> throwError $ DatabaseError "parent relation failed"
                                [_] -> throwError $ NotDisposable
                                [] -> throwError $ UnknownIdMessage
disposeMessage e l Diffuse mi = transactOnLogin e l $ \ui -> checkingMessage e mi $ \_ -> do
                        r <- equery e "select type,parent,conversation,user from messages where id=?" (Only mi)
                        case r :: [(MessageType,Maybe MessageId,ConvId,UserId)] of
                                [(Closed,Just mi',ci,(==) ui -> True)] ->  do
                                        eexecute e "update messages set type=? where id=?" (Open,mi)
                                        ci' <- newConversation e mi
                                        eexecute e "update messages set conversation = ? where id=?" (ci',mi)
                                        eexecute e "update conversations set head = ?, count = count -1 where id = ?" (mi',ci)
                                [(_,_,_,(==) ui -> false)] -> throwError $ NotProponent
                                [_] -> throwError $ NotClosed
                                [] -> throwError $ UnknownIdMessage
             
data MessagesPut
    = New Login Attach String
    | Retract Login MessageId
    | Leave Login Dispose MessageId

instance Put MessagesPut where

  put e (New l at x) = transactOnLogin e l $ \ui -> newMessage e ui at x
  put e (Retract l mi) = retractMessage e l mi
  put e (Leave l di mi) = disposeMessage e l di mi

data MessageGet a where
        Conversation :: Login -> MessageId -> MessageGet [Exposed]
        Single :: Login -> MessageId -> MessageGet Exposed

runMessageGet :: Env -> MessageGet b -> ConnectionMonad b
runMessageGet e (Conversation l mi) = checkingLogin e l $ \(CheckLogin ui _ _) -> checkingMessage e mi $ \(MessageRow _ _ _ _ _ ci _ _) -> do
        [(last,n::Integer)] <- equery e "select head,count from conversations where id=?" (Only ci)
        checkingMessage e last $ \_ -> getPast' e ui last 
runMessageGet e (Single l mi) = checkingLogin e l $ \(CheckLogin ui _ _) -> checkingMessage e mi $  mkExposed e ui
    
getPast' :: Env -> UserId -> MessageId -> ConnectionMonad [Exposed]
getPast' e ui mi =  pastMessages e mi >>= (mapM (mkExposed e ui) . reverse)

instance Getter MessageGet where
  get e = WGet $ runMessageGet e



 
