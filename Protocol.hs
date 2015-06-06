{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Protocol where

import System.Console.Haskeline hiding (catch)
import Control.Applicative
import Data.String
import Control.Monad
import Control.Monad.Writer
import Database.SQLite.Simple
import System.Process
import Database.SQLite.Simple.FromRow
import System.Random
import Data.Typeable
import Control.Exception
import Control.Monad.Error
import Text.Read hiding (lift, get)


import DB1

catchDBException :: IO a -> ConnectionMonad a
catchDBException f = do
        	r <- liftIO $ catch (Right <$> f) (\(e :: SomeException) -> return (Left e)) 
                case r of 
                        Left e -> throwError $ DatabaseError (show e)
                        Right x -> return x
data Put
        = Boot Mail			
        | Invite Login Mail
        | Logout Login
        | Reminds Mail
        | Migrate Login Mail
        | NewMessage Login Attach String
        | RetractMessage Login ConvId
        | LeaveConversation Login ConvId
        | VoteMessage Login MessageId Bool
        | StoreConversation Login ConvId
        | ForgetConversation Login ConvId
        deriving Read

put' :: Env -> Put -> ConnectionMonad ()
put' e (Boot m) = boot e m
put' e (Invite l m) = inviteUser e l m
put' e (Logout l) = logout e l
put' e (Reminds m) = reminder e m
put' e (Migrate l m) = migrate e l m
put' e (NewMessage l at x) = insertMessage e l at x
put' e (RetractMessage l ci) = retractMessage e l ci
put' e (LeaveConversation l ci) = leaveConversation e l ci 
put' e (VoteMessage l mi v) = vote e l mi v
put' e (StoreConversation l ci ) = storeAdd e l ci
put' e (ForgetConversation l ci ) = storeDel e l ci

      
put :: Env -> Put -> WriterT [Event] IO ()  
put e l = do 
        r <- runErrorT (put' e l)
        case r of
                Left s -> liftIO $ print s
                Right () -> return ()
   
data Get 
        = GetMessages MessageId Integer
        | GetStore Login
        -- | GetHints
        deriving Read

get'  :: Env -> Get -> ConnectionMonad String
get' e (GetMessages mi n) = show <$> retrieveMessages e mi n
get' e (GetStore l) = show <$> getStore e l 

get :: Env -> Get -> WriterT [Event] IO (Maybe String)
get e l = do 
        r <- runErrorT (get' e l)
        case r of
                Left s -> liftIO (print s) >> return Nothing
                Right x -> return (Just x)

mkEnv :: Connection -> Env
mkEnv conn = Env 
        (\q r -> catchDBException $ query conn q r) 
        (\q r -> catchDBException $ execute conn q r) (\q -> catchDBException $ execute_ conn q) 
        $ 
        \c -> do
                liftIO $ execute_ conn "begin transaction"
                r <- lift $ runErrorT c
                case r of 
                        Left e -> do
                                liftIO $ execute_ conn "rollback"
                                throwError e
                        Right x -> do
                                liftIO $ execute_ conn "commit transaction"
                                return  x
prepare = do         
        callCommand "rm test.sql"
        callCommand "cat testdef.sql | sqlite3 test.sql"
        conn <- open "test.sql"
        execute_ conn "PRAGMA foreign_keys = ON"
        let     p = put (mkEnv conn)
                g = get (mkEnv conn)
        return (p,g,close conn)

console :: IO ()
console = do
        (p,g,_) <- prepare 
        runInputT defaultSettings $ forever $ do 
                l <- getInputLine "> "
                case fmap readMaybe l of 
                        Nothing -> outputStrLn "should stop here"
                        Just (Just x) -> liftIO $ do 
                                ((),w) <- runWriterT (p x)
                                print w
                        Just Nothing -> case fmap readMaybe l of 
                                Nothing -> outputStrLn "should stop here"
                                Just Nothing -> outputStrLn "no parse"
                                Just (Just x) -> liftIO $ do 
                                        (y,w) <- runWriterT (g x)
                                        case y of 
                                                Just y-> putStrLn y
                                                Nothing -> return ()
                                        print w

