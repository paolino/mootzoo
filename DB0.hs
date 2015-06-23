{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module DB0 where 

import Prelude hiding (readFile, putStrLn)
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
        | UserInvitedBySomeoneElse
        | NotRetractable
        | NotOpponent
        | NotProponent
        | Proponent
        | Opponent
        | AlreadyVoted
        | AlreadyStored
        | NotStored
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
        = EvSendMail Mail Mailer
        | EvNewMessage MessageId
        deriving Show

instance Error DBError where
        strMsg = DatabaseError

type ConnectionMonad = ErrorT DBError (WriterT [Event] IO)

data Env = Env {
        equery :: (ToRow q, FromRow r) => Query -> q -> ConnectionMonad [r],
        eexecute :: ToRow q => Query -> q -> ConnectionMonad (),
        eexecute_ :: Query -> ConnectionMonad (),
        etransaction :: forall a. ConnectionMonad a -> ConnectionMonad a
        }

catchDBException :: IO a -> ConnectionMonad a
catchDBException f = do
        	r <- liftIO $ catch (Right <$> f) (\(e :: SomeException) -> return (Left e)) 
                case r of 
                        Left e -> throwError $ DatabaseError (show e)
                        Right x -> return x
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
        | Closed deriving (Eq,Show)

data ParseException = ParseException deriving Show

instance Exception ParseException

instance FromField MessageType where
        fromField (fieldData -> SQLInteger 0) = Ok Passage
        fromField (fieldData -> SQLInteger 1) = Ok Open
        fromField (fieldData -> SQLInteger 2) = Ok Closed
        fromField _ = Errors [SomeException ParseException]

instance ToField MessageType where
        toField Passage = SQLInteger 0
        toField Open = SQLInteger 1
        toField Closed = SQLInteger 2

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
        deriving Show

instance FromRow MessageRow where
        fromRow = MessageRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

checkingMessage :: Env -> MessageId -> (MessageRow -> ConnectionMonad a) -> ConnectionMonad a
checkingMessage e mi f = do
                        r <- equery e "select * from messages where id=?" (Only mi)
                        case r :: [MessageRow] of
                                [] -> throwError UnknownIdMessage
                                [x] -> f x
                                _ -> throwError $ DatabaseError "multiple message id inconsistence"

pastMessages :: Env -> MessageId -> ConnectionMonad [MessageRow]
pastMessages e mi = do
        checkingMessage e mi $ \_ -> return () 
        equery e "with recursive ex(id,parent,message,vote,type,user,conversation,data) as (select id,parent,message,vote,type,user,conversation,data from messages where messages.id=?  union all select messages.id,messages.parent,messages.message,messages.vote,messages.type,messages.user,messages.conversation,messages.data from messages,ex where messages.id=ex.parent) select id,message,user,type,parent,conversation,vote,data from ex" (Only mi)


futureMessages :: Env -> Maybe MessageId  -> ConnectionMonad [MessageRow]
futureMessages e (Just mi) = equery e "select id,message,user,type,parent,conversation,vote,data from messages where parent=?" (Only mi)
futureMessages e Nothing = equery e "select id,message,user,type,parent,conversation,vote,data from messages where parent isnull"  ()

personalMessages :: Env -> UserId  -> ConnectionMonad [MessageRow]
personalMessages e ui = equery e ("select m1.id,m1.message,m1.user,m1.type,m1.parent,m1.conversation,m1.vote,m1.data from messages as m1 join messages as m2 on  m1.parent = m2.id where m1.type=? and m2.user=?")  (Closed,ui)


ownedMessages :: Env -> UserId  -> ConnectionMonad [MessageRow]
ownedMessages e ui = equery e "select m1.id,m1.message,m1.user,m1.type,m1.parent,m1.conversation,m1.vote,m1.data from messages as m1 where m1.type<>? and m1.user=?"  (Passage,ui)

openConversations ::  Env -> UserId  -> ConnectionMonad [MessageRow]
openConversations e ui = equery e "select m1.id,m1.message,m1.user,m1.type,m1.parent,m1.conversation,m1.vote,m1.data from messages as m1 where m1.type=? and m1.user<>?"  (Open,ui)

-- run :: (Env -> ConnectionMonad a) -> IO (a,[Event])
run f = do        
        conn <- open "mootzoo.db"
        r <- runWriterT $ do

                r <- runErrorT $ f (mkEnv conn)
                case r of 
                        Left e -> liftIO $ print e
                        Right x -> liftIO $ print x
        print r
        close conn
        --return r
lastRow :: Env -> ConnectionMonad Integer
lastRow e = do
        r <- equery e "select last_insert_rowid()" ()
        case (r :: [Only Integer]) of 
                [Only x] -> return x
                _ -> throwError $ DatabaseError "last rowid lost"

newConversation :: Env -> MessageId -> ConnectionMonad ConvId
newConversation e t = do
        eexecute e "insert into conversations values (null,?,?,?)" (t,t,1::Integer)
        lastRow e



