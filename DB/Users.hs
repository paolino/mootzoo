{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module DB.Users where 

import Prelude hiding (readFile, putStrLn)
import Data.String
import Control.Monad
import Control.Monad.Writer
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import System.Random
import Control.Monad.Error
import Control.Exception
import DB0
import DB.Operations

-- | compute a new 30 digits login key
mkLogin :: ConnectionMonad Login
mkLogin =  liftIO $ show <$> foldr (\n m -> m *10 + n) 1 <$> forM [0..30] (const $ randomRIO (0,9::Integer))

data UserPut 

instance Putter UserPut  where
  data Put UserPut
      = Boot Mail String
      | Invite Login Mail String 
      | Logout Login String
      | Reminder Mail String
      | Migrate Login Mail String

  put e (Boot m href) = do 
        l <- mkLogin 
        r <- equery e "select id from users" ()
        case (r :: [Only UserId]) of
                [] -> do 
                        eexecute e "insert into users values (null,?,?,null,0)" (m,l) 
                        tell . return $ EvSendMail m (Booting l) href
                _ -> throwError AlreadyBooted
  put e (Reminder m href) =  do 
        r <- equery e "select login from users where email=?" (Only m)
        case (r :: [Only Login]) of 
                [Only l] -> tell . return $ EvSendMail m (Reminding l) href
                _ ->  throwError UnknownUser 

  put e (Logout l href) = checkingLogin e l $ \(CheckLogin ui m _) -> do 
        l' <- mkLogin 
        eexecute e "update users set login=? where id=?" (l',ui) 
        tell . return $ EvSendMail m (LogginOut l') href
  put e (Invite l m' href)  = checkingLogin e l $ \(CheckLogin i m _) -> etransaction e $ do
        l' <- mkLogin
        r <- equery e "select inviter from users where email=?" (Only m')
        case (r :: [Only (Maybe UserId)]) of 
                [] -> do 
                        eexecute e "insert into users values (null,?,?,?,0)" (m',l',i) 
                        tell . return $ EvSendMail m' (Invitation m l') href 
                _ -> throwError AlreadyInvited
  put e (Migrate l m href) = checkingLogin e l $ \(CheckLogin i m' _) -> do
         eexecute e "update users set email=? where id=?" (m,i)
         tell . return $ EvSendMail m (Migration m' l) href

data UserGet a where
  Check :: Login -> UserGet String

runUserGet :: Env -> UserGet a -> ConnectionMonad a
runUserGet e (Check l) = checkingLogin e l $ \(CheckLogin i m _) -> return m

instance Getter UserGet where
  get e = WGet $ runUserGet e
