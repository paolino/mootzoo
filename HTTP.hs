{-# LANGUAGE RankNTypes #-}

import Control.Monad
import Control.Monad.Writer
import Data.Char
import System.IO
import Network
import Data.Time.LocalTime
import Protocol
import DB.Users
import DB.Get
import Mailer
import JSON
import DB.Put
import DB0
import Text.Read 
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

import System.Environment

jsError x = makeObj [("error",JSString $ toJSString x)]
jsDBError x  = makeObj [("dberror",JSString $ toJSString $ show x)]
jsCompund x y = makeObj [("result",showJSON x),("events", JSArray $ map showJSON y)]

sendResponse
  :: JSON a => WGet -> Maybe (Get a) -> IO (Response String)
sendResponse g v = case v of 
        Nothing -> return $ sendJSON BadRequest $ jsError "Not parsed"
        Just v -> do
                let (WGet g') = g
                (x,w) <- runWriterT $ g' v
                case x of 
                        Left x -> return $ sendJSON BadRequest $ jsDBError $ x
                        Right x -> return $ sendJSON OK $ jsCompund x w
checkUserId g sl s = do
                let (WGet g') = g
                (c,_) <- runWriterT $ g' (Check sl)
                return $ case c of Left x -> sendHTML OK s
                                   Right e -> sendHTML OK $  replace "userkey=" ("userkey='"++sl++"'") 
                                                        $ replace "username=" ("username='"++e++"'") $  s
sendResponseP pwd p v = case v of 
        Nothing -> return $ sendJSON BadRequest $ jsError "Not parsed"
        Just v -> do
                (x,w) <- runWriterT $ p v
                forM_ w $ \y ->
                        case y of
                                EvSendMail s m h -> do
                                        void $ forkIO $ sendAMail pwd s h m
                                _ -> return ()
                case x of 
                        Left x -> return $ sendJSON BadRequest $ jsDBError $ x
                        Right () -> return $ sendJSON OK $ jsCompund JSNull w

main :: IO ()
main = do
        [pwd,mailbooter,reloc] <- getArgs
        (t,p,g) <- prepare
        let responseP = sendResponseP pwd p
        putStrLn "running"
        when t $ void $ responseP $ Just $ Boot mailbooter "reloc" 
        serverWith defaultConfig { srvLog = quietLogger, srvPort = 8888 }
                $ \_ url request -> do
                          let   URI a (Just (URIAuth _ b _)) _ _ _  = rqURI request
                                href = reloc
                          case rqMethod request of
                            POST -> do 
                                let msg = decodeString (rqBody request)
                                case splitOn "/" $ url_path url of
                                        ["Invite",sl] -> responseP $ do
                                                        return $ Invite sl msg href 
                                        ["Migrate",sl] -> responseP $ do
                                                        return $ Migrate sl msg href
                                        ["Reminds"] -> responseP $ do
                                                        return $ Reminds msg href
                                        ["New",sl,"DontAttach"] ->  responseP $  Just $ New sl DontAttach msg
                                        ["New",sl,"Attach",sci] ->  responseP $ do
                                                        ci <- readMaybe sci
                                                        return $ New sl (Attach ci) msg
                                        ["New",sl,"Correct",sci] ->  responseP $ do
                                                        ci <- readMaybe sci
                                                        return $ New sl (Correct ci) msg
                                        _ -> return $ sendJSON BadRequest $ JSNull
                            PUT -> do 
                                 case splitOn "/" $ url_path url of
                                        ["Vote",sl,smi,sb] -> responseP $ do
                                                        mi <- readMaybe smi
                                                        b <- readMaybe sb
                                                        return $ Vote sl mi b
                                        ["Logout",sl] -> responseP $ do
                                                        return $ Logout sl href
                                        ["Retract",sl,sci] -> responseP $ do
                                                        ci <- readMaybe sci
                                                        return $ Retract sl ci
                                        ["Open",sl,sci] -> responseP $ do
                                                        ci <- readMaybe sci
                                                        return $ Leave sl Diffuse ci
                                        ["Close",sl,sci] -> responseP $ do
                                                        ci <- readMaybe sci
                                                        return $ Leave sl Accept ci
                                        _ -> return $ sendJSON BadRequest $ JSNull
                            GET -> do 
                                case splitOn "/" $ url_path url of
                                        ["Login",sl] -> do
                                                s <- readFile "login.html"
                                                checkUserId g sl s
                                                
                                        ["Past",sl,sci] -> sendResponse g $ do
                                                        ci <- readMaybe sci
                                                        return $ Past sl ci 
                                        ["Conversation",sl,sn] -> sendResponse g $ do
                                                        n <- readMaybe sn
                                                        return $ Conversation sl n 
                                        ["Future",sl,sn] -> sendResponse g $ do
                                                        n <- readMaybe sn
                                                        return $ Future sl n
                                        ["Roots",sl]-> sendResponse g $ do
                                                        return $ Roots sl 
                                        ["Personal",sl]-> sendResponse g $ do
                                                        return $ Personal sl 
                                        ["Owned",sl]-> sendResponse g $ do
                                                        return $ Owned sl 
                                        ["Opens",sl]-> sendResponse g $ do
                                                        return $ Opens sl 
                                        _ -> return $ sendJSON BadRequest $ JSNull

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


