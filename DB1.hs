{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module DB.Users where 

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
import DB0
import Search

getTemplateMail m (Booting l) = do
        x <- readFile "invitation.txt"
        let x' = replace "invitante" "mootzoo service" $ replace "linklogin" (pack $ "http://mootzoo.com/Login/" ++ l) $ x
        return ("Booting a new Mootzoo Conversational System",S.pack m,x')
getTemplateMail m (Invitation m' l) = do
        x <- readFile "invitation.txt"
        let x' = replace "invitante" (pack m') $ replace "linklogin" (pack $ "http://mootzoo.com/Login/" ++ l) $ x
        return ("Invitation to Mootzoo Conversational System",S.pack m,x')
getTemplateMail m (Reminding l) = do
        x <- readFile "reminder.txt"
        let x' = replace "linklogin" (pack $ "http://mootzoo.com/Login/" ++ l) $ x
        return ("Login link reminder from Mootzoo",S.pack m,x')
getTemplateMail m (LogginOut l) = do
        x <- readFile "newlogin.txt"
        let x' = replace "linklogin" (pack $ "http://mootzoo.com/Login/" ++ l) $ x
        return ("New login link from Mootzoo conversational system",S.pack m,x')

sendAMail :: String -> Mail -> Mailer -> IO ()
sendAMail pwd as ty = do
        (t,m,b) <- getTemplateMail as ty
        putStrLn b
        sendGmail "mootzoo.service" (pack pwd) (Address (Just "mootzoo service") "mootzoo.service@gmail.com") [Address (Just m) m] [] [] t b [] 10000000

-- | change the user mail
migrate :: Env -> Login -> Mail -> ConnectionMonad ()
migrate e l m = checkingLogin e l $ \(CheckLogin i m' _) -> do
         eexecute e "update users set email=? where id=?" (m,i)
         tell . return $ EvSendMail m (Migration m' l)

-- | compute a new 30 digits login key
mkLogin :: ConnectionMonad Login
mkLogin =  liftIO $ show <$> foldr (\n m -> m *10 + n) 1 <$> forM [0..30] (const $ randomRIO (0,9::Integer))

-- | insert a new user by mail
inviteUser :: Env -> Login -> Mail -> ConnectionMonad ()
inviteUser e l m' = checkingLogin e l $ \(CheckLogin i m _) -> etransaction e $ do
        r <- equery e "select inviter from users where email=?" (Only m')
        l' <- mkLogin
        let newuser = do 
                        eexecute e "insert into users values (null,?,?,?)" (m',l',i) 
                        r <- equery e "select last_insert_rowid()" ()
                        case (r :: [Only UserId]) of 
                                [Only ui'] -> copyStore e i ui'
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

boot :: Env -> Mail -> ConnectionMonad ()
boot e m = do 
        l <- mkLogin 
        r <- equery e "select id from users" ()
        case (r :: [Only UserId]) of
                [] -> do 
                        eexecute e "insert into users values (null,?,?,null)" (m,l) 
                        tell . return $ EvSendMail m (Booting l) 
                _ -> throwError AlreadyBooted


--------------------------
------ Message operations
--------------------------


data Attach 
        = Attach MessageId
        | DontAttach
        | Correct MessageId
        deriving Read

transactOnLogin :: Env -> Login -> (UserId -> ConnectionMonad ()) -> ConnectionMonad ()
transactOnLogin e l f = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> f ui

newMessage ::  Env -> UserId  -> Attach -> String -> ConnectionMonad ()
newMessage e ui DontAttach x = do
        eexecute e "insert into messages values (null,?,?,?,null,0)" (x,ui,Public) -- public message
newMessage e ui (Attach mi) x = do
        r <- equery e "select type,parent from messages where id=?" (Only mi)
        case r :: [(MessageType,MessageId)] of
                [Private] -> do
                        r' <- equery e "select user from messages where id=?" (Only mi')
                        case r' :: [Only UserId] of
                                [(==ui) -> True] -> do
                                        eexecute e "update messages set type=? where id=?" (Passage,mi) 
                                        eexecute e "insert into messages values (null,?,?,?,?,0)" (x,ui,Private,mi)
                                [_] -> throwError NotOpponent
                [Passage] -> do
                        eexecute e "insert into messages values (null,?,?,?,?,0)" (x,ui,Private,mi)
                [_] -> throwError NotAttachable
                _ -> throwError UnknownIdMessage
newMessage e ui (Correct mi) x = do
        r <- equery e "select type,user from messages where id=?" (Only mi)
        case r :: [(MessageType,UserId)] of
                [(Private, (==ui) -> True] -> do
                        eexecute e "update messages set message = ? where id=?" (x,mi)
                [(Private,_)] -> throwError NotProponent
                [_] -> NotPrivate
                _ -> UnknownIdMessage

insertMessage :: Env -> Login -> Attach -> String -> ConnectionMonad ()
insertMessage e l at x = transactOnLogin e l $ \ui -> newMessage e ui at x

                        
retractMessage :: Env -> Login -> MessageId -> ConnectionMonad ()
retractMessage e l mi =  transactOnLogin e l $ \ui -> checkingMessage e mi $  do
        r <- equery e "select type,user from messages where id=?" (Only mi)
        case r :: [(MessageType,UserId)] of
                [(Passage,_)] -> throwError IsPassage
                [(_, (==ui) -> True)] ->  eexecute e "delete from messages where id=?" (Only mi)
                [_] -> throwError NotProponent
                _ -> UnknownIdMessage

data Dispose = Diffuse | Accept

dispose ::  Env -> Login -> Dispose -> MessageId -> ConnectionMonad ()
dispose e ui dispose mi = transactOnLogin e l $ \ui -> checkingMessage e mi $ do
                        r <- equery e "select type,parent from messages where id=?" (Only mi)
                        case r :: [(MessageType,Maybe MessageId)] of
                                [(Private,Just mi')] -> 
                                        r' <- equery e "select user from messages where id=?" (Only mi')
                                        case r' of
                                                [(==ui) -> True] ->  case dispose of 
                                                        Diffuse -> eexecute e "update messages set type=? where id=?" (Public,mi)
                                                        Accept -> eexecute e "update messages set type=? where id=?" (Passage,mi)       
                                                [_] -> throwError NotOpponent
                                                _ -> throwError $ DatabaseError "parent relation failed"
                                [_] -> throwError $ NotDisposable
                                _ -> throwError $ UnknownIdMessage
                        

{-

vote :: Env -> Login -> MessageId -> Bool -> ConnectionMonad ()
vote e l mi b = transactOnLogin  e  l $ \ui -> checkingMessage e mi $ do
        r <- equery e "select message,user from voting where message=? and user=?" (mi,ui)
        case r :: [(MessageId,UserId)] of 
                [] -> do 
                        eexecute e "insert into voting values (?,?)" (mi,ui)    
                        eexecute e "update messages set vote=vote+? where id=?" (if b then 1 else (-1), mi)
                _ -> throwError AlreadyVoted
                



storeAdd :: Env -> Login -> MessageId -> ConnectionMonad () 
storeAdd e l mi  =   transactOnLogin e l $ \ui -> eexecute e "insert or replace into store values (?,?)" (mi,ui)    

storeDel :: Env -> Login -> MessageId -> ConnectionMonad ()
storeDel e l mi = transactOnLogin e l $ \ui ->  eexecute e "delete from store where message=? and user=?" (ci,ui)    

data StoreEl = StoreEl {
        smessage :: MessageRow,
        votable :: Bool
        }

instance FromRow StoreEl where
        fromRow = StoreEl <$> field <*> field
        
getStore :: Env -> Login -> ConnectionMonad [StoreEl] -- buggy use JOIN (right outer ?)
getStore e l = transactOnLogin e l $ \ui -> do
        rs <- equery e "select message from store where user=?" (Only ui)
        cs <- forM rs $ \(Only mi) -> do
                        rs <- equery e "select message,user from voting where message= ? and user=?" (mi,ui)
                        case rs :: [(MessageId,UserId)] of 
                                [] -> return False
                                _ -> return True
                        
        return $ zipWith3 (\(Only ci) (co,vo) (Only mi) -> UserConv ci co mi vo) rs cs ms
                


copyStore :: Env -> UserId -> UserId -> ConnectionMonad ()
copyStore e ui ui' = do
        r <- equery e "select message from store where user=?" (Only ui)
        forM_ r $ \(Only (ci :: ConvId)) -> eexecute e "insert into store values (?,?)" (ci,ui')    
-}
                
                       
 

