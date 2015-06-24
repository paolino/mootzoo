{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
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

-- | change the user mail
migrate :: Env -> Login -> Mail -> String -> ConnectionMonad ()
migrate e l m href = checkingLogin e l $ \(CheckLogin i m' _) -> do
         eexecute e "update users set email=? where id=?" (m,i)
         tell . return $ EvSendMail m (Migration m' l) href

-- | compute a new 30 digits login key
mkLogin :: ConnectionMonad Login
mkLogin =  liftIO $ show <$> foldr (\n m -> m *10 + n) 1 <$> forM [0..30] (const $ randomRIO (0,9::Integer))

-- | insert a new user by mail
inviteUser :: Env -> Login -> Mail -> String -> (UserId -> UserId -> ConnectionMonad ()) -> ConnectionMonad ()
inviteUser e l m' href f = checkingLogin e l $ \(CheckLogin i m _) -> etransaction e $ do
        r <- equery e "select inviter from users where email=?" (Only m')
        l' <- mkLogin
        let newuser = do 
                        eexecute e "insert into users values (null,?,?,?,0)" (m',l',i) 
                        r <- equery e "select last_insert_rowid()" ()
                        case (r :: [Only UserId]) of 
                                [Only ui'] -> f i ui'
                                _ -> throwError $ DatabaseError "last rowid lost"
                        tell . return $ EvSendMail m' (Invitation m l') href 

        case (r :: [Only (Maybe UserId)]) of 
                [] -> newuser
                [Only (Just ((==i) -> True))] -> newuser
                [Only (Just ((==i) -> False))] -> throwError UserInvitedBySomeoneElse
                _ ->  throwError $ DatabaseError "user multiple email inconsistence"

-- | change the user login
logout :: Env -> Login -> String -> ConnectionMonad ()
logout e l href = checkingLogin e l $ \(CheckLogin ui m _) -> do 
        l' <- mkLogin 
        eexecute e "update users set login=? where id=?" (l',ui) 
        tell . return $ EvSendMail m (LogginOut l') href

reminder :: Env -> Mail -> String -> ConnectionMonad ()
reminder e m href =  do 
        r <- equery e "select login from users where email=?" (Only m)
        case (r :: [Only Login]) of 
                [Only l] -> tell . return $ EvSendMail m (Reminding l) href
                _ ->  throwError UnknownUser 

getLogins :: Env -> ConnectionMonad [Login]
getLogins e = map fromOnly <$> equery e "select login from users" ()

boot :: Env -> Mail -> String -> ConnectionMonad ()
boot e m href = do 
        l <- mkLogin 
        r <- equery e "select id from users" ()
        case (r :: [Only UserId]) of
                [] -> do 
                        eexecute e "insert into users values (null,?,?,null,0)" (m,l) 
                        tell . return $ EvSendMail m (Booting l) href
                _ -> throwError AlreadyBooted


getLogin :: Env -> Login -> ConnectionMonad String
getLogin e l = checkingLogin e l $ \(CheckLogin i m _) -> return m
