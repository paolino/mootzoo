{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Writer
import Data.Char
import System.IO
import Network
import Data.Time.LocalTime
import Text.Read hiding (get) 
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.URL as URL
import Network.URI (URI (..), URIAuth (..))
import Text.JSON
import Text.JSON.String(runGetJSON)
import Codec.Binary.UTF8.String
import Control.Exception(try,SomeException)
import System.FilePath
import Data.List(isPrefixOf)
import Data.List.Split
import Data.String.Utils (replace)
import Control.Concurrent
import Data.ByteString (pack)
import Data.ByteString.Internal (c2w)
import System.Environment
import System.Time
import Control.Monad.Error
import DB0
import DB.Users
import DB.Labels
import DB.Client
import DB.Messages
import DB.Votes
import Mailer
import JSON

sendText       :: StatusCode -> String -> Response String
sendText s v    = insertHeader HdrContentLength (show (length txt))
                $ insertHeader HdrContentEncoding "UTF-8"
                $ insertHeader HdrContentEncoding "text/plain"
                $ (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v

sendJSON       :: StatusCode -> JSValue -> Response String
sendJSON s v    = insertHeader HdrContentType "application/json"
                $ sendText s (showJSValue v "")

sendHTML       :: StatusCode -> String -> Response String
sendHTML s v    = insertHeader HdrContentType "text/html"

                $ sendText s v

jsError :: String -> JSValue
jsError x = makeObj [("error",showJSON x)]
jsDBError x  = makeObj [("dberror",showJSON $ show x)]
jsCompund x y = makeObj [("result",showJSON x),("events", showJSON y)]

sendGetter :: (JSON a,Getter b) => Env -> Maybe (b a) -> IO (Response String)
sendGetter env v = case v of 
        Nothing -> return $ sendJSON BadRequest $ jsError "Not parsed"
        Just v -> do
                (x,w) <- runWriterT . runErrorT $ rget env v
                z <- case x of 
                        Left x -> return $ sendJSON BadRequest $ jsDBError $ x
                        Right x -> return $ sendJSON OK $ jsCompund x w
                return z

checkUserId :: Env -> String -> String -> String -> IO (Response String)
checkUserId env sl s loc = do
                (c,_) <- runWriterT . runErrorT $ rget env (Check sl)
                return $ case c of Left x -> insertHeaders [Header HdrLocation loc] $  sendHTML Found $ "<h1>Unidentified</h1>"
                                   Right e -> let (name,_)= break (=='@') e in sendHTML OK $  replace "userkey=" ("userkey='"++sl++"'") 
                                                        $ replace "username=" ("username='"++name++"'") $  s

sendPut :: Put a => String -> String -> IO Env -> Maybe a -> IO (Response String)
sendPut mail pwd mkEnv v = case v of 
        Nothing -> return $ sendJSON BadRequest $ jsError "Not parsed"
        Just v -> do
                (x,w) <- mkEnv >>= \e -> runWriterT . runErrorT $ put e v
                forM_ w $ \y ->
                        case y of
                                EvSendMail s m h -> do
                                        void $ forkIO $ sendAMail mail pwd s m h
                                _ -> return ()
                case x of 
                        Left x -> return $ sendJSON BadRequest $ jsDBError $ x
                        Right () -> return $ sendJSON OK $ jsCompund JSNull w
main :: IO ()
main = do
        [mail,pwd,mailbooter,reloc] <- getArgs
        (t,g,p) <- prepare
          
        let   sp :: Put a => Maybe a -> IO (Response String) 
              sp = sendPut mail pwd p
              sg :: (JSON a,Getter b) => Maybe (b a) -> IO (Response String)
              sg = sendGetter g
        putStrLn "running"
        when t $ void $ sp $ Just $ Boot mailbooter reloc 
        serverWith defaultConfig{ srvLog = stdLogger, srvPort = 8888 }
                $ \_ url request -> do
                          let   URI a (Just (URIAuth _ b _)) _ _ _  = rqURI request
                                href = reloc
                          case rqMethod request of
                            POST ->  do 
                                let msg = decodeString (rqBody request)
                                case splitOn "/" $ url_path url of
                                        ["Invite",sl] -> sp . Just $ Invite sl msg href 
                                        ["Migrate",sl] -> sp . Just $ Migrate sl msg href
                                        ["Reminds"] -> sp . Just $ Reminder msg href
                                        ["New",sl,"DontAttach"] ->  sp . Just $ New sl DontAttach msg
                                        ["New",sl,"Attach",sci] ->  sp $ do
                                                        ci <- readMaybe sci
                                                        return $ New sl (Attach ci) msg
                                        ["New",sl,"Correct",sci] ->  sp $ do
                                                        ci <- readMaybe sci
                                                        return $ New sl (Correct ci) msg
                                        ["Store",sl,sci,k,v] -> sp $ do
                                                        ci <- readMaybe sci
                                                        return $ Store sl ci k $ pack $map c2w v
                                        ["RemoveLabel",sl,smi] -> sp $ do
                                                      mi <- readMaybe smi
                                                      return $ RemoveLabel sl mi msg
                                        ["UpdateLabel",sl,smi,s1] -> sp $ do
                                                      mi <- readMaybe smi
                                                      return $ UpdateLabel sl mi s1 msg
                                        _ -> return $ sendJSON BadRequest $ JSNull
                            PUT -> do 
                                 case splitOn "/" $ url_path url of
                                        ["Vote",sl,smi,sb] -> sp $ do
                                                        mi <- readMaybe smi
                                                        b <- readMaybe sb
                                                        return $ Vote sl mi b
                                        ["Logout",sl] -> sp . Just $ Logout sl href
                                        ["Retract",sl,sci] -> sp $ do
                                                        ci <- readMaybe sci
                                                        return $ Retract sl ci
                                        ["Open",sl,sci] -> sp $ do
                                                        ci <- readMaybe sci
                                                        return $ Leave sl Diffuse ci
                                        ["Close",sl,sci] -> sp $ do
                                                        ci <- readMaybe sci
                                                        return $ Leave sl Accept ci
                                        ["Follow",sl,sci] -> sp $ do
                                                        ci <- readMaybe sci
                                                        return $ Follow sl ci
                                        ["Unfollow",sl,sci] -> sp $ do
                                                        ci <- readMaybe sci
                                                        return $ Unfollow sl ci
                                        _ -> return $ sendJSON BadRequest $ JSNull
                            GET -> do 
                                case splitOn "/" $ url_path url of
                                        ["Login",sl] -> do
                                                s <- readFile "login.html"
                                                checkUserId g sl s href
                                                
                                        ["Conversation",sl,sn] -> sg $ do
                                                        n <- readMaybe sn
                                                        return $ Conversation sl n 
                                        ["Following",sl]-> sg . Just $ Following sl
                                        ["Restore",sl,smi,k]-> sg $ do
                                                        mi <- readMaybe smi
                                                        return $ Restore sl mi k
                                        ["Notificate",sl,smi] -> sg $ do
                                                        mi <- readMaybe smi
                                                        return $ Notificate sl mi 
                                        ["Single",sl,smi] -> sg $ do
                                                        mi <- readMaybe smi
                                                        return $ Single sl mi 
                                        ["Labels",sl] -> sg . Just $ GetLabels sl 
                                        ["MessageLabels",sl,smi] -> sg $ do
                                                        mi <- readMaybe smi
                                                        return $ GetMessageLabels sl mi 
                                        ["LabelMessages",sl,l] -> sg $ do
                                                        return $ GetLabelMessages sl l
                                        ["Notificate",sl,"Label",smi] -> sg $ do
                                                        mi <- readMaybe smi
                                                        return $ GetLabelNotification sl mi 
                                        _ -> return $ sendJSON BadRequest $ JSNull
