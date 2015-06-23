{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import System.Directory

import DB0
import DB.Users
import DB.Put
import DB.Get
import DB.Votes

data Put
        = Boot Mail			
        | Invite Login Mail
        | Logout Login
        | Reminds Mail
        | Migrate Login Mail
        | New Login Attach String
        | Retract Login MessageId
        | Leave Login Dispose MessageId
        | Vote Login MessageId Bool
        deriving Read

put' :: Env -> Put -> ConnectionMonad ()
put' e (Boot m) = boot e m
put' e (Invite l m) = inviteUser e l m (\_ _ -> return ())
put' e (Logout l) = logout e l
put' e (Reminds m) = reminder e m
put' e (Migrate l m) = migrate e l m
put' e (New l at x) = insertMessage e l at x
put' e (Retract l mi) = retractMessage e l mi
put' e (Leave l d mi) = disposeMessage e l d mi 
put' e (Vote l mi v) = vote e l mi v
      
put :: Env -> Put -> WriterT [Event] IO (Either DBError ())
put e l = runErrorT (put' e l)
   
data Get a where
        Past :: Login -> MessageId -> Get [Exposed]
        Future :: Login -> MessageId -> Get [Exposed]
        Conversation :: Login -> MessageId -> Get [Exposed]
        Roots :: Login -> Get [Exposed]
        Personal :: Login -> Get [Exposed]
        Owned :: Login -> Get [Exposed]
        Opens :: Login -> Get [Exposed]
        Check :: Login -> Get String
        Logins :: Get [Login] -- debugging
        
get'  :: Env -> Get a -> ConnectionMonad a
get' e (Past l mi) = getPast e l mi 
get' e (Future l mi) = getFuture e l mi
get' e (Conversation l mi ) = getConversation e l mi 
get' e (Roots l ) = getRoots e l 
get' e (Personal l ) = getPersonal e l 
get' e (Owned l ) = getOwned e l 
get' e (Opens l ) = getOpen e l 
get' e Logins = getLogins e
get' e (Check l) = getLogin e l

get :: Env -> Get a -> WriterT [Event] IO (Either DBError a)
get e l = runErrorT (get' e l)

clean = callCommand "cat schema.sql | sqlite3 mootzoo.db"

data WGet  = WGet (forall a. Get a ->  WriterT [Event] IO (Either DBError a))

prepare :: IO (Bool,Put -> WriterT [Event] IO (Either DBError ()),WGet)
prepare = do         
        b <- doesFileExist "mootzoo.db"
        when (not b) clean
        conn <- open "mootzoo.db"
        execute_ conn "PRAGMA foreign_keys = ON"
        let     p = put (mkEnv conn)
                g = WGet (get (mkEnv conn))
        return (not b,p,g)
{-

console :: IO ()
console = do
        (t,p,g) <- prepare 
        runInputT defaultSettings $ forever $ do 
                l <- getInputLine "> "
                case fmap readMaybe l of 
                        Nothing -> outputStrLn "should stop here"
                        Just (Just x) -> liftIO $ do 
                                (e,w) <- runWriterT (p x)
                                print (e,w)
                        Just Nothing -> case fmap readMaybe l of 
                                Nothing -> outputStrLn "should stop here"
                                Just Nothing -> outputStrLn "no parse"
                                Just (Just x) -> liftIO $ do 
                                        (y,w) <- runWriterT (g x)
                                        case y of 
                                                Left y -> print y
                                                Right y  -> print y
                                        print w
-}
