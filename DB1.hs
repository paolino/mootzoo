{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module DB1 where 

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
	| UserAlreadyInvited
	| UserUnreacheable
        | NotRetractable
        | NotAuthor
        | AlreadyVoted
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
        | EvNewConversation ConvId
        | EvNewMessage MessageId
        | EvUpdateConversation ConvId
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

-- | compute a new 30 digits login key
mkLogin :: ConnectionMonad Login
mkLogin =  liftIO $ show <$> foldr (\n m -> m *10 + n) 1 <$> forM [0..30] (const $ randomRIO (0,9::Integer))

-- | insert a new user by mail
inviteUser :: Env -> Login -> Mail -> ConnectionMonad ()
inviteUser e l m = checkingLogin e l $ \(CheckLogin i m' _) -> etransaction e $ do
        r <- equery e "select id from users where email=?" (Only m)
        l' <- mkLogin
        case (r :: [Only Integer]) of 
                [] ->   do 
                        eexecute e "insert into users values (null,?,?,?)" (m,l',i) 
                        tell . return $ EvSendMail m (Invitation m' l')
                _ ->  throwError UserAlreadyInvited

-- | change the user login
logout :: Env -> Login -> ConnectionMonad ()
logout e l = checkingLogin e l $ \(CheckLogin _ m _) -> do 
        l' <- mkLogin 
        eexecute e "update users set login=? where login=?" (l',l) 
        tell . return $ EvSendMail m (LogginOut l')

reminder :: Env -> Mail -> ConnectionMonad ()
reminder e m =  do 
        r <- equery e "select login from users where email=?" (Only m)
        case (r :: [Only Login]) of 
                [Only l] -> tell . return $ EvSendMail m (Reminding l)
                _ ->  throwError UnknownUser 

boot :: Env -> Mail -> ConnectionMonad ()
boot e m = do 
        l <- mkLogin 
        r <- equery e "select id from users" ()
        case (r :: [Only UserId]) of
                [] -> do 
                        eexecute e "insert into users values (null,?,?,null)" (m,l) 
                        tell . return $ EvSendMail m (Booting l) 
                _ -> throwError AlreadyBooted

checkingConv :: Env -> ConvId -> (MessageId -> ConnectionMonad a) -> ConnectionMonad a
checkingConv e ci f = do
        r <- equery e "select rif from conversations where id=?" (Only ci) 
        case (r :: [Only MessageId]) of 
                [Only mi] -> f mi
                _ ->  throwError UnknownIdConversation

newConv :: Env -> MessageId -> ConnectionMonad ()
newConv e mi = do 
        eexecute e "insert into conversations values (null,?)" (Only mi)
        r <- equery e "select last_insert_rowid()" ()
        case (r :: [Only ConvId]) of 
                [Only ci] -> tell [EvNewConversation ci]
                _ -> throwError $ DatabaseError "last rowid lost"
        


newMessage :: Env -> UserId -> Maybe MessageId -> String -> (MessageId -> ConnectionMonad ()) -> ConnectionMonad ()
newMessage e ui mi x f = do
        case mi of 
                Nothing -> eexecute e "insert into messages values (null,?,?,1,null,0)" (x,ui)
                Just mi -> eexecute e "insert into messages values (null,?,?,1,?,0)" (x,ui,mi)
        r <- equery e "select last_insert_rowid()" ()
        case (r :: [Only MessageId]) of 
                [Only mi'] -> tell [EvNewMessage mi'] >> f mi'
                _ -> throwError $ DatabaseError "last rowid lost"
        
                           
data Attach 
        = AttachMessage MessageId
        | AttachConversation ConvId
        | DontAttach
        deriving Read

insertMessage :: Env -> Login -> Attach -> String -> ConnectionMonad ()
insertMessage e l at x = etransaction e $ checkingLogin e l $ \(CheckLogin ui _ _) -> case at of
                DontAttach -> newMessage e ui Nothing x $ newConv e
                AttachMessage mi -> do
                        r <- equery e "select retractable from messages where id=?" (Only mi)
                        case  (r ::[Only Bool]) of
                                [Only False] -> newMessage e ui (Just mi) x $ newConv e
                                [] -> throwError UnknownIdMessage
                                _ -> throwError NotBranchable
                AttachConversation ci -> checkingConv e ci $ \mi ->  do
                        r <- equery e "select retractable,parent,user from messages where id=?" (Only mi)
                        let update = newMessage e ui (Just mi) x $ \mi' -> do
                                        eexecute e "update conversations set rif=? where id=?" (mi',ci)
                                        tell [EvUpdateConversation ci]
                        case r :: [(Bool, Maybe MessageId,UserId)] of
                                [(True,_,(==ui) -> True)] -> throwError NotRespondable
                                [(False,_,_)] -> update
                                [(True,Nothing,_)] -> update
                                [(True,Just mi',_)] -> do
                                        r <- equery e "select retractable,user from messages where id=?" (Only mi')
                                        case r :: [(Bool,UserId)] of
                                                [(False,_)] -> update 
                                                [(True,ui')] -> if ui == ui' then do
                                                                        update
                                                                        eexecute e "update messages set retractable=0 where id=?" (Only mi')
                                                                else throwError NotRespondable
                                                _ -> throwError $ DatabaseError "parent missing or multiple"
                                _ -> throwError $ DatabaseError "missed rif retractable in conversation"
                        
retractMessage :: Env -> Login -> ConvId -> ConnectionMonad ()
retractMessage e l ci =  etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> checkingConv e ci $ \mi ->  do
                        r <- equery e "select retractable,parent,user from messages where id=?" (Only mi)
                
                        case r :: [(Bool,Maybe MessageId, UserId)] of
                                [(True,mp,(==ui) -> True)] -> do
                                        eexecute e "delete from messages where id=?" (Only mi)
                                        case mp of 
                                                Nothing -> eexecute e "delete from conversations where id=?" (Only ci)
                                                Just mi' -> eexecute e "update conversations set rif=?" (Only mi')
                                [(False,_,_)] -> throwError NotRetractable
                                [(_,_,_)] -> throwError NotAuthor
                                _ -> throwError $ DatabaseError "retractMessage inconsistence"

leaveConversation :: Env -> Login -> ConvId -> ConnectionMonad ()
leaveConversation e l ci = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> checkingConv e ci $ \mi ->  do
                        r <- equery e "select retractable,parent from messages where id=?" (Only mi)
                        case r :: [(Bool,Maybe MessageId)] of
                                [(False,_)] -> throwError NotRetractable
                                [(True,Nothing)] -> throwError NotAuthor
                                [(True,Just mi')] -> do
                                        r <- equery e "select retractable,user from messages where id=?" (Only mi')
                                        case r :: [(Bool,UserId)] of
                                                [(False,_)] -> throwError NotRetractable
                                                [(_,(== ui) -> False)] -> throwError NotAuthor
                                                [(_,_)] -> eexecute e "update messages set retractable=0 where id=?" (Only mi')
                                                _ -> throwError $ DatabaseError "leaveConversation inconsistence"
-- | change the user mail
migrate :: Env -> Login -> Mail -> ConnectionMonad ()
migrate e l m = checkingLogin e l $ \(CheckLogin i m' _) -> do
         eexecute e "update users set email=? where id=?" (m,i)
         tell . return $ EvSendMail m (Migration m' l)

				
catchDBException :: IO a -> ConnectionMonad a
catchDBException f = do
        	r <- liftIO $ catch (Right <$> f) (\(e :: SomeException) -> return (Left e)) 
                case r of 
                        Left e -> throwError $ DatabaseError (show e)
                        Right x -> return x

vote :: Env -> Login -> MessageId -> Bool -> ConnectionMonad ()
vote e l mi b = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> do
        r <- equery e "select message,user from voting where message=? and user=?" (mi,ui)
        case r :: [(MessageId,UserId)] of 
                [] -> do 
                        eexecute e "insert into voting values (?,?)" (mi,ui)    
                        if b then 
                              eexecute e "update messages set vote=vote+1 where id=?" (Only mi)
                        else
                              eexecute e "update messages set vote=vote-1 where id=?" (Only mi)
                _ -> throwError AlreadyVoted
                
		
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

      
put :: Env -> Put -> WriterT [Event] IO ()  
put e l = do 
        r <- runErrorT (put' e l)
        case r of
                Left s -> liftIO $ print s
                Right () -> return ()
   
data Get 
        = GetMessages ConvId 
        | GetStore Login
        deriving Read
 
get'  :: Env -> Get -> ConnectionMonad String
get' e (GetMessages ci) = show <$> retrieveMessages e ci

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


retrieveMessages :: Env -> ConvId -> ConnectionMonad [(MessageId,String,Integer)]
retrieveMessages e ci = checkingConv e ci $ \mi -> do
        r <- equery e "with recursive ex(id,parent,message,vote) as (select id,parent,message,vote from messages where messages.id=?  union all select messages.id,messages.parent,messages.message,messages.vote from messages,ex where messages.id=ex.parent) select id,message,vote from ex" (Only mi)
        return r
