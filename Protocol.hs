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
   
data Get 
        = Past Login MessageId Integer
        | Future Login MessageId
        | ForMe Login
        | FromMe Login
        | ForAll Login
        deriving Read

get'  :: Env -> Get -> ConnectionMonad [Exposed]
get' e (Past l mi n) = getPast e l mi n
get' e (Future l mi) = getFuture e l mi
get' e (ForMe mi) = getClosed e mi
get' e (FromMe mi) = getEnvelopes e mi
get' e (ForAll mi) = getOpens e mi

get :: Env -> Get -> WriterT [Event] IO (Either DBError [Exposed])
get e l = runErrorT (get' e l)

clean = callCommand "cat schema.sql | sqlite3 mootzoo.db"
prepare = do         
        b <- doesFileExist "mootzoo.db"
        when (not b) clean
        conn <- open "mootzoo.db"
        execute_ conn "PRAGMA foreign_keys = ON"
        let     p = put (mkEnv conn)
                g = get (mkEnv conn)
        return (not b,p,g)

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

