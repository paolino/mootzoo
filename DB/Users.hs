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
migrate :: Env -> Login -> Mail -> ConnectionMonad ()
migrate e l m = checkingLogin e l $ \(CheckLogin i m' _) -> do
         eexecute e "update users set email=? where id=?" (m,i)
         tell . return $ EvSendMail m (Migration m' l)

-- | compute a new 30 digits login key
mkLogin :: ConnectionMonad Login
mkLogin =  liftIO $ show <$> foldr (\n m -> m *10 + n) 1 <$> forM [0..30] (const $ randomRIO (0,9::Integer))

-- | insert a new user by mail
inviteUser :: Env -> Login -> Mail -> (UserId -> UserId -> ConnectionMonad ()) -> ConnectionMonad ()
inviteUser e l m' f = checkingLogin e l $ \(CheckLogin i m _) -> etransaction e $ do
        r <- equery e "select inviter from users where email=?" (Only m')
        l' <- mkLogin
        let newuser = do 
                        eexecute e "insert into users values (null,?,?,?)" (m',l',i) 
                        r <- equery e "select last_insert_rowid()" ()
                        case (r :: [Only UserId]) of 
                                [Only ui'] -> f i ui'
                                _ -> throwError $ DatabaseError "last rowid lost"
                        tell . return $ EvSendMail m' (Invitation m l')

        case (r :: [Only (Maybe UserId)]) of 
                [] -> newuser
                [Only (Just ((==i) -> True))] -> newuser
                [Only (Just ((==i) -> False))] -> throwError UserInvitedBySomeoneElse
                _ ->  throwError $ DatabaseError "user multiple email inconsistence"

-- | change the user login
logout :: Env -> Login -> ConnectionMonad ()
logout e l = checkingLogin e l $ \(CheckLogin ui m _) -> do 
        l' <- mkLogin 
        eexecute e "update users set login=? where id=?" (l',ui) 
        tell . return $ EvSendMail m (LogginOut l')

reminder :: Env -> Mail -> ConnectionMonad ()
reminder e m =  do 
        r <- equery e "select login from users where email=?" (Only m)
        case (r :: [Only Login]) of 
                [Only l] -> tell . return $ EvSendMail m (Reminding l)
                _ ->  throwError UnknownUser 

getLogins :: Env -> ConnectionMonad [Login]
getLogins e = map fromOnly <$> equery e "select login from users" ()

boot :: Env -> Mail -> ConnectionMonad ()
boot e m = do 
        l <- mkLogin 
        r <- equery e "select id from users" ()
        case (r :: [Only UserId]) of
                [] -> do 
                        eexecute e "insert into users values (null,?,?,null)" (m,l) 
                        tell . return $ EvSendMail m (Booting l) 
                _ -> throwError AlreadyBooted


