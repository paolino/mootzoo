{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DB where 

import Control.Applicative
import Data.String
import Control.Monad
import Control.Monad.Reader
import Database.SQLite.Simple
import System.Process
import Database.SQLite.Simple.FromRow
import System.Random
import Data.Typeable
import Control.Exception


type Mail = String
type Login = String


data Env = Env {
        sendLogin :: Login -> ConnectionMonad (),
        equery :: (ToRow q, FromRow r) => Query -> q -> ConnectionMonad [r],
        eexecute :: ToRow q => Query -> q -> ConnectionMonad (),
        eexecute_ :: Query -> ConnectionMonad ()
        etransaction :: ConnectionMonad () -> ConnectionMonad ()
        checkingLogin :: Login -> ConnectionMonad a -> ConnectionMonad a
        }

data DatabaseError = UnknownUser | UnknownMail | 

type ConnectionMonad = ErrorT DatabaseException ReaderT Env IO
-- | compute a new 30 digits login key



mkLogin :: ConnectionMonad Login
mkLogin =  liftIO $ show <$> foldr (\n m -> m *10 + n) 1 <$> forM [0..30] (const $ randomRIO (0,9))

-- | insert a new user by mail
addUser :: Login -> Mail -> ConnectionMonad ()
addUser (User e) = etransaction $ do
        r <- equery "select id from users where email=?" (Only e)
        n <- mkLogin
        case (r :: [Only Integer]) of 
                [] -> eexecute "insert into users values (null,?,?)" (e,n)
                _ ->  eexecute "update users set login=? where email=?" (n,e)

-- | change the user login
logout :: Login -> ConnectionMonad ()
logout c l = mkLogin >>= \l' -> eexecute c "update users set login=? where login=?" (l',e)

-- | change the user mail
migrate :: Connection -> Login -> Mail -> User
migrate c l m = eexecute c "update users set mail=? where login=?" (m,l)




data Message = Message String deriving (Show)
data Conversation

type Id a = Integer


data MootzooException = UnknownUser | UnknownMail
    deriving (Show, Typeable)

instance Exception MootzooException

instance FromRow Message where
   fromRow = Message <$> field 
{-
-- | select the full set of messages of a conversation
selectMessages :: Connection -> Int -> IO [Message]
selectMessages conn conv =  do
        b <- query conn  "SELECT message from messages where conversation= ?" (Only conv)
        l <- query conn  "SELECT message from conversations where id= ?" (Only conv)
        return $ b ++ l
-}




-- | check a login exists and give back the real id of the user
realUser :: Connection -> Login -> IO (Id User) 
realUser conn i = do 
        r <- query conn "select id from users where login=?" (Only i)
        return $ case r of 
                [Only u] -> u :: Integer
                _ -> throw $ UnknownUser

-- | insert a new conversation
newConversation :: Connection -> Login -> Message -> IO ()
newConversation conn l (Message s) = withTransaction conn $ do
        u <- realUser conn l
        execute conn "insert into conversations values (null,?,?,null)" (u,s)


-- | add a message to a conversation
addMessage :: Connection -> Id Conversation -> Login -> Message -> IO ()
addMessage conn c l (Message s) = withTransaction conn $ do
        u <- realUser conn l
        execute conn "insert into newmessage values (?,?,?)" (s,c,u)

-- | value a conversation
interestConversation :: Connection -> Id Conversation -> Login -> Integer -> IO ()
interestConversation conn c l v = withTransaction conn $ do 
        u <- realUser conn l
        execute conn "insert into interests (?,?,?)" (u,c,v)

takeConversation :: Connection -> Id Conversation -> Login -> IO ()
takeConversation conn j i = withTransaction conn $ do 
        u <- realUser conn i
        execute conn "insert into joinconversation values (?,?)" (j,u)

dropConversation :: Connection -> Id Conversation -> Login -> IO ()
dropConversation conn j i = withTransaction conn $ do 
        u <- realUser conn i
        execute conn "insert into dropconversation values (?,?)" (j,u)



				
			
					
