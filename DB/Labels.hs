

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Labels where 

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
import DB0 
type ClientId = Integer
type LabelId = Integer

baseLabel :: Env -> ClientId -> ConnectionMonad ()
baseLabel e ci = eexecute e "insert into labels (client,label) values (?,?)" (ci,"****"::String)

data LabelPut
        = AddLabel Login MessageId
        | RemoveLabel Login MessageId String
        | UpdateLabel Login MessageId String String

instance Put LabelPut where
  put e (RemoveLabel l mi "****") = throwError $ DatabaseError "cannot remove **** label"
  put e (RemoveLabel l mi s) = transactOnLogin e l $ \ui ->  do
    r <- equery e "select id from client where user = ? and message = ?" (ui,mi)
    case r of
      [] -> throwError NotFollowing
      [Only (ci::ClientId)] -> eexecute e "delete from labels where client = ? and label = ?" (ci,s)
  
  put e (UpdateLabel l mi s1 s) = transactOnLogin e l $ \ui ->  do
    r <- equery e "select id from client where user = ? and message = ?" (ui,mi)
    case r of
      [] -> throwError NotFollowing
      [Only (ci::ClientId)] -> do 
          r <- equery e "select label from labels where client = ? and label = ?" (ci,s)
          case r of 
            [] -> do
                eexecute e "update labels set label = ? where client = ? and label = ?" (s,ci,s1)
                case s1 of
                  "****" -> eexecute e "insert into labels (client,label) values (?,?)" (ci,"****"::String)
                  _ -> return ()
            [Only (l::String)] -> throwError $ DatabaseError "duplicate label"


data LabelNotification = LabelNotification
    {   branchesl :: Integer
    ,   newsl :: Integer
    }

data Label b where
    GetLabels :: Login -> Label [String]
    GetLabelMessages :: Login -> String -> Label [MessageId]
    GetMessageLabels :: Login -> MessageId -> Label [String]
    GetLabelNotification :: Login -> String -> Label LabelNotification

runLabel :: Env -> Label b -> ConnectionMonad b
runLabel e (GetLabels l) = checkingLogin e l $ \(CheckLogin ui _ _) -> map fromOnly <$> equery e
    "select distinct label from labels join client on client = id where user = ?" (Only ui)
runLabel e (GetLabelMessages l la) = checkingLogin e l $ \(CheckLogin ui _ _) -> map fromOnly <$> equery e
    "select distinct message from labels join client on client = id where user = ? and label = ?" (ui,la)

runLabel e (GetMessageLabels l mi) = checkingLogin e l $ \(CheckLogin ui _ _) -> checkingMessage e mi $ \_ -> map fromOnly <$> equery e
    "select label from labels join client on labels.client = id where user = ? and message = ?" (ui,mi)

runLabel e (GetLabelNotification l li) = checkingLogin e l $ \(CheckLogin ui _ _) -> undefined

instance Getter Label where
  get e  = WGet $ runLabel e


