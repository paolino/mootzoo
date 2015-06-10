{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module DB1 where 

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

checkingConv :: Env -> ConvId -> (MessageId -> ConnectionMonad a) -> ConnectionMonad a
checkingConv e ci f = do
        r <- equery e "select rif from conversations where id=?" (Only ci) 
        case (r :: [Only MessageId]) of 
                [Only mi] -> f mi
                _ ->  throwError UnknownIdConversation

newConv :: Env -> MessageId -> (ConvId -> ConnectionMonad ()) -> ConnectionMonad ()
newConv e mi f = do 
        eexecute e "insert into conversations values (null,?)" (Only mi)
        r <- equery e "select last_insert_rowid()" ()
        case (r :: [Only ConvId]) of 
                [Only ci] -> do
                        tell [EvNewConversation ci] >> f ci
                _ -> throwError $ DatabaseError "last rowid lost"
        


newMessage :: Env -> UserId -> Maybe MessageId -> String -> (MessageId -> ConnectionMonad ()) -> ConnectionMonad ()
newMessage e ui mi x f = do
        -- change on parent presence
        case mi of 
                Nothing -> eexecute e "insert into messages values (null,?,?,1,null,0)" (x,ui)
                Just mi -> eexecute e "insert into messages values (null,?,?,1,?,0)" (x,ui,mi)
        r <- equery e "select last_insert_rowid()" ()
        case (r :: [Only MessageId]) of 
                [Only mi'] ->   do
                        tell [EvNewMessage mi'] 
                        f mi'
                _ -> throwError $ DatabaseError "last rowid lost"
        
                           
data Attach 
        = AttachMessage MessageId
        | AttachConversation ConvId
        | DontAttach
        | CorrectConversation ConvId
        deriving Read

insertMessage :: Env -> Login -> Attach -> String -> ConnectionMonad ()
insertMessage e l at x = etransaction e $ checkingLogin e l $ \(CheckLogin ui _ _) -> case at of
                DontAttach -> newMessage e ui Nothing x $ \mi -> do
                                newConv e mi $ \ci -> do
                                        storeAdd' e ui ci  $ return ()
                                        newSearch e ci x
                AttachMessage mi -> do
                        r <- equery e "select retractable from messages where id=?" (Only mi)
                        case  (r ::[Only Bool]) of
                                [Only False] -> do
                                        r <- equery e "select id from conversations where rif=?" (Only mi)
                                        case r :: [Only ConvId] of
                                                [] ->  newMessage e ui (Just mi) x $ \mi -> newConv e mi $ \ci -> do
                                                                storeAdd' e ui ci  $ return ()
                                                                newSearch e ci x
                                                _ -> throwError NotBranchable
                                [] -> throwError UnknownIdMessage
                                _ -> throwError NotBranchable
                CorrectConversation ci ->  checkingConv e ci $ \mi ->  do
                        r <- equery e "select retractable,user from messages where id=?" (Only mi)
                        case r :: [(Bool,UserId)] of
                                [(True,(==ui) -> True)] -> do
                                        eexecute e "update messages set message=? where id=?" (x,mi)
                                        eexecute e "update search set content=? where id=?" (x,mi)
                                        correctSearch e ci x
                                [(False,_)] -> throwError NotRetractable
                                [(_,_)] -> throwError NotProponent
                                _ -> throwError $ DatabaseError "missed rif retractable in conversation"

                AttachConversation ci -> checkingConv e ci $ \mi ->  do
                        r <- equery e "select retractable,parent,user from messages where id=?" (Only mi)
                        let update = newMessage e ui (Just mi) x $ \mi' -> do
                                        eexecute e "update conversations set rif=? where id=?" (mi',ci)
                                        storeAdd' e ui ci  $ return ()
                                        insertSearch e ci x
                                        tell [EvUpdateConversation ci]
                        case r :: [(Bool, Maybe MessageId,UserId)] of
                                [(True,_,(==ui) -> True)] -> throwError Proponent
                                [(False,_,_)] -> update
                                [(True,Nothing,_)] -> update
                                [(True,Just mi',_)] -> do
                                        r <- equery e "select retractable,user from messages where id=?" (Only mi')
                                        case r :: [(Bool,UserId)] of
                                                [(False,_)] -> update 
                                                [(True,ui')] -> if ui == ui' then do
                                                                        update
                                                                        eexecute e "update messages set retractable=0 where id=?" (Only mi')
                                                                        eexecute e "delete from voting where message=? " (Only mi')
                                                                else throwError NotOpponent
                                                _ -> throwError $ DatabaseError "parent missing or multiple"
                                _ -> throwError $ DatabaseError "missed rif retractable in conversation"
                        
retractMessage :: Env -> Login -> ConvId -> ConnectionMonad ()
retractMessage e l ci =  etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> checkingConv e ci $ \mi ->  do
                        r <- equery e "select retractable,parent,user from messages where id=?" (Only mi)
                
                        case r :: [(Bool,Maybe MessageId, UserId)] of
                                [(True,mp,(==ui) -> True)] -> do
                                        eexecute e "delete from voting where message=? " (Only mi)
                                        case mp of 
                                                Nothing -> do 
                                                        deleteSearch e ci
                                                        eexecute e "delete from store where conversation=? " (Only ci)
                                                        eexecute e "delete from conversations where id=?" (Only ci)
                                                Just mi' -> do
                                                        eexecute e "update conversations set rif=? where id=?" (mi',ci)
                                                        retractSearch e ci
                                        eexecute e "delete from messages where id=?" (Only mi)
                                        eexecute e "delete from search where id=?" (Only mi)
                                [(False,_,_)] -> throwError NotRetractable
                                [(_,_,_)] -> throwError NotProponent
                                _ -> throwError $ DatabaseError "retractMessage inconsistence"

leaveConversation :: Env -> Login -> ConvId -> ConnectionMonad ()
leaveConversation e l ci = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> checkingConv e ci $ \mi ->  do
                        r <- equery e "select retractable,parent from messages where id=?" (Only mi)
                        case r :: [(Bool,Maybe MessageId)] of
                                [(False,_)] -> throwError NotRetractable
                                [(True,Nothing)] -> throwError NotOpponent
                                [(True,Just mi')] -> do
                                        r <- equery e "select retractable,user from messages where id=?" (Only mi')
                                        case r :: [(Bool,UserId)] of
                                                [(False,_)] -> throwError NotRetractable
                                                [(_,(== ui) -> False)] -> throwError NotOpponent
                                                [(_,_)] -> do 
                                                        eexecute e "update messages set retractable=0 where id=?" (Only mi')
                                                        eexecute e "delete from voting where message=? " (Only mi')
                                                        leaveSearch e ci
                                                _ -> throwError $ DatabaseError "leaveConversation inconsistence"
-- | change the user mail
migrate :: Env -> Login -> Mail -> ConnectionMonad ()
migrate e l m = checkingLogin e l $ \(CheckLogin i m' _) -> do
         eexecute e "update users set email=? where id=?" (m,i)
         tell . return $ EvSendMail m (Migration m' l)


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
                
		
       
        
data Color = Blank | Green | Blue | Yellow | Azur | Red deriving Show

getColor' e mi ui = do
        ms <- retrieveMessages e mi 2
        case filter isRetractable ms of 
                [] -> return Blank
                [MessageRow _ _ _ _ ((==ui) -> True)] -> return Azur
                [MessageRow _ _ _ _ ((==ui) -> False)] -> return Red
                [MessageRow _ _ _ _ ((==ui) -> False),MessageRow _ _ _ _ ((==ui) -> False)] -> return Green
                [MessageRow _ _ _ _ ((==ui) -> True),MessageRow _ _ _ _ ((==ui) -> False)] -> return Blue
                [MessageRow _ _ _ _ ((==ui) -> False),MessageRow _ _ _ _ ((==ui) -> True)] -> return Yellow
                _ -> throwError $ DatabaseError "retractable inconsistence"
               
isRetractable (MessageRow _ _ _ True _) = True
isRetractable _ = False 
storeAdd' :: Env -> UserId -> ConvId -> ConnectionMonad () -> ConnectionMonad ()
storeAdd' e ui ci  f =   do
        r <- equery e "select user from store where conversation=? and user=?" (ci,ui)
        case r :: [Only UserId] of 
                [] ->  eexecute e "insert into store values (?,?)" (ci,ui)    
                _ -> f

storeAdd e l ci = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> checkingConv e ci $ \_ -> storeAdd' e ui ci $ throwError AlreadyStored

storeDel :: Env -> Login -> ConvId -> ConnectionMonad ()
storeDel e l ci = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> checkingConv e ci $ \mi ->  do
        r <- equery e "select user from store where conversation=? and user=?" (ci,ui)
        case r :: [Only UserId] of 
                [_] -> do 
                        c <- getColor' e mi ui
                        case c of
                                Azur -> throwError Proponent 
                                Blue -> throwError Proponent 
                                Yellow -> throwError Opponent
                                _  -> eexecute e "delete from store where conversation=? and user=?" (ci,ui)    
                _ -> throwError NotStored

data UserConv = UserConv {
        cid :: ConvId,
        ccol :: Color,
        cmsg :: MessageId,
        voted :: Bool
        }
        deriving Show

getStore :: Env -> Login -> ConnectionMonad [UserConv]
getStore e l = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> do
        rs <- equery e "select conversation from store where user=?" (Only ui)
        ms <- forM rs $ \ci -> do
                r <-  equery e "select rif from conversations where id=?" (ci :: Only ConvId)
                case r of
                        [] -> throwError $ DatabaseError "rif of conversation missing"
                        [x] -> return x
                        _ -> throwError $ DatabaseError "multiple messageid"
        cs <- forM ms $ \(Only mi) -> do
                        c <- getColor' e mi ui
                        rs <- equery e "select message,user from voting where message= ? and user=?" (mi,ui)
                        vo <- case rs :: [(MessageId,UserId)] of 
                                [] -> return False
                                _ -> return True
                        return (c,vo)
                        
        return $ zipWith3 (\(Only ci) (co,vo) (Only mi) -> UserConv ci co mi vo) rs cs ms
                


copyStore :: Env -> UserId -> UserId -> ConnectionMonad ()
copyStore e ui ui' = do
        r <- equery e "select conversation from store where user=?" (Only ui)
        forM_ r $ \(Only (ci :: ConvId)) -> eexecute e "insert into store values (?,?)" (ci,ui')    

hintStore :: Env -> Login -> ConnectionMonad ()
hintStore e l = etransaction e $ checkingLogin e l $  \(CheckLogin ui _ _) -> do
        rs <- equery e "SELECT DISTINCT conversations.id FROM (conversations LEFT OUTER JOIN store on conversations.id=store.conversation and store.user=?) WHERE store.conversation IS NULL  order by rif desc limit 1000" (Only ui)
        case rs :: [(Only ConvId)] of
                [] -> return ()
                rs -> do 
                        n <- liftIO $ randomRIO (0,length rs - 1 )
                        let (Only ci) = rs !! n
                        eexecute e "insert or replace into store values (?,?)" (ci,ui)
                
                       
 
searchMessages :: Env -> String -> ConnectionMonad [ConvId]
searchMessages e x = map fromOnly <$> equery e "select id from search where content match ? limit 100" (Only x)
