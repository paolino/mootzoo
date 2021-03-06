{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module DB.Put where 

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
{-
newMessage e ui (Pass mi) _ = do
        r <- equery e "select type,parent,user,conversation from messages where id=?" (Only mi)
        let insert  ci = do 
                eexecute e "update messages set type=? where id=?" (Passage,mi) 
                eexecute e "insert into messages (id,message,user,type,parent,conversation)  values (null,?,?,?,?,?)" (x,ui,Closed,mi,ci)
                mi'' <- lastRow e
                tell [EvNewMessage mi'']
                eexecute e "update conversations set head=? , count = count + 1 where id=?" (mi'',ci)

        case r :: [(MessageType,Maybe UserId,UserId,ConvId)] of
                [(_,_,eq ui -> True,_)] -> throwError Proponent
                [(Closed,Just mi',_,ci)] -> do
                        r' <- equery e "select user from messages where id=?" (Only mi')
                        case r' of
                                [Only (eq ui -> True)] -> insert ci 
                                [_] -> throwError NotOpponent
                [(Open,_,_,ci)] ->  insert ci 
                [(Passage,_,_,_)] -> do
                                eexecute e "insert into messages (id,message,user,type,parent,conversation)  values (null,?,?,?,?,null)" (x,ui,Closed,mi)
                                mi' <- lastRow e
                                tell [EvNewMessage mi']
                                ci <- newConversation e mi'
                                eexecute e "update messages set conversation = ? where id=?" (ci,mi')
                                -- check conversation
                [_] -> throwError NotAttachable
                [] -> throwError UnknownIdMessage
-}
newMessage e ui (Attach mi) x = do
        r <- equery e "select type,parent,user,conversation from messages where id=?" (Only mi)
        let insert  ci = do 
                eexecute e "update messages set type=? where id=?" (Passage,mi) 
                eexecute e "insert into messages (id,message,user,type,parent,conversation)  values (null,?,?,?,?,?)" (x,ui,Closed,mi,ci)
                mi'' <- lastRow e
                tell [EvNewMessage mi'']
                eexecute e "update conversations set head=? , count = count + 1 where id=?" (mi'',ci)

        case r :: [(MessageType,Maybe UserId,UserId,ConvId)] of
                [(_,_,eq ui -> True,_)] -> throwError Proponent
                [(Closed,Just mi',_,ci)] -> do
                        r' <- equery e "select user from messages where id=?" (Only mi')
                        case r' of
                                [Only (eq ui -> True)] -> insert ci 
                                [_] -> throwError NotOpponent
                [(Open,_,_,ci)] ->  insert ci 
                [(Passage,_,_,_)] -> do
                                eexecute e "insert into messages (id,message,user,type,parent,conversation)  values (null,?,?,?,?,null)" (x,ui,Closed,mi)
                                mi' <- lastRow e
                                tell [EvNewMessage mi']
                                ci <- newConversation e mi'
                                eexecute e "update messages set conversation = ? where id=?" (ci,mi')
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

insertMessage :: Env -> Login -> Attach -> String -> ConnectionMonad ()
insertMessage e l at x = transactOnLogin e l $ \ui -> newMessage e ui at x

        
retractMessage :: Env -> Login -> MessageId -> ConnectionMonad ()
retractMessage e l mi =  transactOnLogin e l $ \ui -> checkingMessage e mi $ \_ -> do
        r <- equery e "select type,user,parent,conversation from messages where id=?" (Only mi)
        case r :: [(MessageType,UserId,Maybe MessageId,ConvId)] of
                [(Passage,_,_,_)] -> throwError IsPassage
                [(Open, (==ui) -> True,_,ci)] ->  do
                        eexecute e "delete from conversations where id=?" (Only ci)
                        eexecute e "delete from messages where id=?" (Only mi)
                [(Closed, (==ui) -> True,Just mi',ci)] ->  do
                        r <- equery e "select conversation from messages where id=?" (Only mi')
                        case r :: [Only ConvId] of
                                [Only ((== ci) -> False)] -> eexecute e "delete from conversations where id=?" (Only ci)
                                [Only ((== ci) -> True)] -> eexecute e "update conversations set head=? , count = count - 1 where id=?" (mi',ci)
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
                  


 
