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
import Text.JSON
import System.Directory

import DB1
import DB0

instance JSON MessageRow where
        showJSON (MessageRow mid mtext mvote mretr muser) = makeObj [
                ("mid",JSRational False $ fromIntegral mid),
                ("txt",JSString $ toJSString mtext),  
                ("vote",JSRational False $ fromIntegral mvote),
                ("retr",JSBool mretr)
                ]
instance JSON UserConv where
        showJSON (UserConv cid ccol cmsg cvo) = makeObj [
                ("cid",JSRational False $ fromIntegral cid), 
                ("color",JSString $ toJSString $ show ccol),
                ("mid",JSRational False $ fromIntegral cmsg),
                ("voted",JSBool cvo)
                ] 
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
        | HintConversation Login
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
put' e (HintConversation l ) = hintStore e l 

      
put :: Env -> Put -> WriterT [Event] IO (Either DBError ())
put e l = runErrorT (put' e l)
   
data Get 
        = GetMessages MessageId Integer
        | GetStore Login
        | GetSearch String
        -- | GetHints
        deriving Read
get'  :: Env -> Get -> ConnectionMonad JSValue
get' e (GetMessages mi n) = showJSON <$> retrieveMessages e mi n
get' e (GetStore l) = showJSON <$> getStore e l 
get' e (GetSearch s) = showJSON <$> searchMessages e s

get :: Env -> Get -> WriterT [Event] IO (Either DBError JSValue)
get e l = runErrorT (get' e l)

clean = callCommand "cat testdef.sql | sqlite3 test.db"
prepare = do         
        b <- doesFileExist "test.db"
        when (not b) clean
        conn <- open "test.db"
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
                                print w
                        Just Nothing -> case fmap readMaybe l of 
                                Nothing -> outputStrLn "should stop here"
                                Just Nothing -> outputStrLn "no parse"
                                Just (Just x) -> liftIO $ do 
                                        (y,w) <- runWriterT (g x)
                                        case y of 
                                                Left y -> print y
                                                Right y  -> print y
                                        print w

