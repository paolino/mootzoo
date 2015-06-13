{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module DB.Votes where 

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
import Lib (eq)
import DB0



vote :: Env -> Login -> MessageId -> Bool -> ConnectionMonad ()
vote e l mi b = transactOnLogin  e  l $ \ui -> checkingMessage e mi $ \_ ->  do
        r <- equery e "select user from voting where user=? and message=?" (ui,mi)
        case r :: [(Only UserId)] of 
                [] -> do 
                        r <- equery e "select type,user from messages where id = ?" (Only mi)
                        case r of 
                                [(Passage,_)] -> throwError IsPassage
                                [(_,eq ui -> True)] -> throwError Proponent
                                [(_,_)] -> do
                                        eexecute e "insert into voting values (?,?)" (mi,ui)    
                                        eexecute  e "update messages set vote=vote+? where id=?" (if b then 1 else (negate 1) :: Int, mi)
                                _ -> throwError UnknownIdMessage
                _ -> throwError AlreadyVoted
                


