{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DB where 

import Control.Applicative
import Data.String
import Control.Monad
import Database.SQLite.Simple
import System.Process
import Database.SQLite.Simple.FromRow
import System.Random
import Data.Typeable
import Control.Exception

data Message = Message String deriving (Show)

data User = User String deriving (Show)

data Conversation

type Id a = Integer

type Login = String

data MootzooException = UnknownUser | UnknownMail
    deriving (Show, Typeable)

instance Exception MootzooException

instance FromRow Message where
   fromRow = Message <$> field 

-- | select the full set of messages of a conversation
selectMessages :: Connection -> Int -> IO [Message]
selectMessages conn conv =  do
        b <- query conn  "SELECT message from messages where conversation= ?" (Only conv)
        l <- query conn  "SELECT message from conversations where id= ?" (Only conv)
        return $ b ++ l



-- | insert a new user by mail
addUser :: Connection -> User -> IO String
addUser conn (User e) = withTransaction conn $ do
        r <- query conn "select id from users where email=?" (Only e)
        (n :: Integer) <- foldr (\n m -> m *10 + n) 1 <$> forM [0..30] (const $ randomRIO (0,9))
        case (r :: [Only Integer]) of 
                [] -> execute conn "insert into users values (null,?,?)" (e,show n)
                _ ->  execute conn "update users set login=? where email=?" (show n,e)
        return $ show n



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



				
			
					
