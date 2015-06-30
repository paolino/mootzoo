{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}

module DB0 where 

import Prelude hiding (readFile, putStrLn)
import System.Console.Haskeline hiding (catch)
import System.Directory
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
import Data.Text.Lazy.IO (readFile,putStrLn)
import Data.Text.Lazy (Text,replace,pack)
import qualified Data.Text as S (pack)
import Network.Mail.Client.Gmail
import Network.Mail.Mime (Address (..))
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Ok


type Mail = String
type Login = String
type UserId = Integer
type ConvId = Integer
type MessageId = Integer

data DBError 
	= UnknownKeyUser
        | AlreadyBooted      
        | UnknownIdConversation
        | UnknownIdMessage
        | NotRespondable
        | NotBranchable
        | UnknownUser
        | NotAttachable
        | NotDisposable
        | IsPassage
        | NotClosed
        | IsClosed
        | NotOpen
        | AlreadyInvited
        | NotRetractable
        | NotOpponent
        | NotProponent
        | Proponent
        | Opponent
        | AlreadyVoted
        | AlreadyFollowing
        | NotFollowing
        | DatabaseError String
        deriving Show

data Mailer 
        = Reminding Login
        | Invitation Mail Login
        | LogginOut Login
        | Migration Mail Login
        | Booting Login
        deriving Show
data Event 
        = EvSendMail Mail Mailer String
        | EvNewMessage MessageId
        deriving Show

instance Error DBError where
        strMsg = DatabaseError

type ConnectionMonad = ErrorT DBError (WriterT [Event] IO)

catchDBException :: IO a -> ConnectionMonad a
catchDBException f = do
        	r <- liftIO $ catch (Right <$> f) (\(e :: SomeException) -> return (Left e)) 
                case r of 
                        Left e -> throwError $ DatabaseError (show e)
                        Right x -> return x
data CheckLogin = CheckLogin UserId Mail (Maybe UserId)

instance FromRow CheckLogin where
   fromRow = CheckLogin <$> field <*> field <*> field

-- | wrap an action in a check of the login presence
checkingLogin :: Env -> Login -> (CheckLogin -> ConnectionMonad a) -> ConnectionMonad a
checkingLogin e l f = do
        r <- equery e "select id,email,inviter from users where login=?" (Only l)
        case (r :: [CheckLogin]) of
                [i] -> f i
                _ -> throwError UnknownKeyUser

transactOnLogin :: Env -> Login -> (UserId -> ConnectionMonad a) -> ConnectionMonad a
transactOnLogin e l f = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> f ui

data MessageType 
        = Passage
        | Open
        | Closed 
        | Passed 
        deriving (Eq,Show)

data ParseException = ParseException deriving Show

instance Exception ParseException

instance FromField MessageType where
        fromField (fieldData -> SQLInteger 0) = Ok Passage
        fromField (fieldData -> SQLInteger 1) = Ok Open
        fromField (fieldData -> SQLInteger 2) = Ok Closed
        fromField (fieldData -> SQLInteger 3) = Ok Passed
        fromField _ = Errors [SomeException ParseException]

instance ToField MessageType where
        toField Passage = SQLInteger 0
        toField Open = SQLInteger 1
        toField Closed = SQLInteger 2
        toField Passed = SQLInteger 3

data MessageRow = MessageRow {
        mid :: MessageId,
        mtext :: String,
        muser :: UserId,
        mtype :: MessageType,
        mparent :: Maybe MessageId,
        mconversation ::ConvId,
        mvote :: Integer,
        mdata :: String
        }

instance FromRow MessageRow where
        fromRow = MessageRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

checkingMessage :: Env -> MessageId -> (MessageRow -> ConnectionMonad a) -> ConnectionMonad a
checkingMessage e mi f = do
                        r <- equery e "select * from messages where id=?" (Only mi)
                        case r :: [MessageRow] of
                                [] -> throwError UnknownIdMessage
                                [x] -> f x
                                _ -> throwError $ DatabaseError "multiple message id inconsistence"



lastRow :: Env -> ConnectionMonad Integer
lastRow e = do
        r <- equery e "select last_insert_rowid()" ()
        case (r :: [Only Integer]) of 
                [Only x] -> return x
                _ -> throwError $ DatabaseError "last rowid lost"



data Env = Env {
        equery :: (ToRow q, FromRow r) => Query -> q -> ConnectionMonad [r],
        eexecute :: ToRow q => Query -> q -> ConnectionMonad (),
        eexecute_ :: Query -> ConnectionMonad (),
        etransaction :: forall a. ConnectionMonad a -> ConnectionMonad a
        }
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
class Putter a where
    data Put a 
    put :: Env -> Put a -> ConnectionMonad ()

data WGet a = WGet (forall b. a b ->  ConnectionMonad b)

class Getter a where
    get :: Env -> WGet a

rget :: Getter t => Env -> t b -> ConnectionMonad b
rget e q = let WGet f = get e in f q


clean = callCommand "cat schema.sql | sqlite3 mootzoo.db"

prepare :: IO (Bool,Env,IO Env)
prepare = do         
        b <- doesFileExist "mootzoo.db"
        when (not b) clean
        conn <- open "mootzoo.db"
        execute_ conn "PRAGMA foreign_keys = ON"
        return (not b,mkEnv conn, mkEnv <$> open "mootzoo.db" )
