

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

type LabelId = Integer
data LabelPut

instance Putter LabelPut where
  data Put LabelPut 
        = AddLabel Login String
        | RemoveLabel Login LabelId
        | ChangeLabel Login LabelId String
        | LabelOn Login LabelId MessageId
        | LabelOff Login LabelId MessageId
  put e (AddLabel l s) = transactOnLogin e l $ \ui -> undefined
  put e (RemoveLabel l li) = transactOnLogin e l $ \ui -> undefined
  put e (ChangeLabel l li s) = transactOnLogin e l $ \ui -> undefined
  put e (LabelOn l li mi) = transactOnLogin e l $ \ui -> undefined
  put e (LabelOff l li mi) = transactOnLogin e l $ \ui -> undefined

data LabelNotification = LabelNotification
    {   branchesl :: Integer
    ,   newsl :: Integer
    }

data Label b where
    GetLabels :: Login -> Label [LabelId]
    GetLabelMessages :: Login -> LabelId -> Label [MessageId]
    GetLabelNotification :: Login -> LabelId -> Label LabelNotification

runLabel :: Env -> Label b -> ConnectionMonad b
runLabel e (GetLabels l) = checkingLogin e l $ \(CheckLogin ui _ _) -> undefined
runLabel e (GetLabelMessages l li) = checkingLogin e l $ \(CheckLogin ui _ _) -> undefined
runLabel e (GetLabelNotification l li) = checkingLogin e l $ \(CheckLogin ui _ _) -> undefined

instance Getter Label where
  get e  = WGet $ runLabel e


