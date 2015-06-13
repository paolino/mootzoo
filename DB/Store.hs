{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module DB.Store where 

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




storeAdd :: Env -> Login -> MessageId -> ConnectionMonad () 
storeAdd e l mi  =   transactOnLogin e l $ \ui -> checkingMessage e mi $ \_ ->  do 
        r <- equery e "select type from messages where id=?" (Only mi)
        case r of
                [Only Closed] -> throwError IsClosed
                [_] -> eexecute e "insert or replace into store values (?,?)" (mi,ui)
                _ -> throwError $ DatabaseError "checkingMessage broken"

storeDel :: Env -> Login -> MessageId -> ConnectionMonad ()
storeDel e l mi = transactOnLogin e l $ \ui ->  eexecute e "delete from store where message=? and user=?" (mi,ui)    

storeSubst :: Env -> Login -> MessageId -> MessageId -> ConnectionMonad ()
storeSubst e l mi mi' = transactOnLogin e l $ \ui ->  checkingMessage e mi $ \_ -> checkingMessage e mi' $ \_ ->  do 
        r <- equery e "select type from messages where id=?" (Only mi')
        case r of
                [Only Closed] -> throwError IsClosed
                [_] -> eexecute e "update store set message= ? where message = ? and user =?"  (mi',mi,ui)

{-
getStore :: Env -> Login -> ConnectionMonad [StoreEl] -- buggy use JOIN (right outer ?)
getStore e l = transactOnLogin e l $ \ui -> do
        rs <- equery e "select id,messages.message,user,type,vote from store join messages on store.message = id where user=?" (Only ui)
        cs <- forM rs $ \(MessageRow mi _ _ _ _) -> do
                        rs <- equery e "select message,user from voting where message= ? and user=?" (mi,ui)
                        case rs :: [(MessageId,UserId)] of 
                                [] -> return False
                                _ -> return True
                        
        return $ zipWith3 (\(Only ci) (co,vo) (Only mi) -> UserConv ci co mi vo) rs cs ms
                
data StoreEl = StoreEl {
        smessage :: MessageRow,
        votable :: Bool
        }

instance FromRow StoreEl where
        fromRow = StoreEl <$> field <*> field
        
getStore :: Env -> Login -> ConnectionMonad [StoreEl] -- buggy use JOIN (right outer ?)
getStore e l = transactOnLogin e l $ \ui -> do
        rs <- equery e "select message from store where user=?" (Only ui)
        cs <- forM rs $ \(Only mi) -> do
                        rs <- equery e "select message,user from voting where message= ? and user=?" (mi,ui)
                        case rs :: [(MessageId,UserId)] of 
                                [] -> return False
                                _ -> return True
                        
        return $ zipWith3 (\(Only ci) (co,vo) (Only mi) -> UserConv ci co mi vo) rs cs ms
                


copyStore :: Env -> UserId -> UserId -> ConnectionMonad ()
copyStore e ui ui' = do
        r <- equery e "select message from store where user=?" (Only ui)
        forM_ r $ \(Only (ci :: ConvId)) -> eexecute e "insert into store values (?,?)" (ci,ui')    
-}
                
                       
 

