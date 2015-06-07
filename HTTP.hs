import Control.Monad
import Control.Monad.Writer
import Data.Char
import System.IO
import Network
import Data.Time.LocalTime
import Protocol
import DB1
import Text.Read 
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Text.JSON
import Text.JSON.String(runGetJSON)
import Text.XHtml
import Codec.Binary.UTF8.String
import Control.Exception(try,SomeException)
import System.FilePath
import Data.List(isPrefixOf)
import Data.List.Split
import Data.String.Utils (replace)

jsError x = makeObj [("error",JSString $ toJSString x)]
jsDBError x  = makeObj [("dberror",JSString $ toJSString $ show x)]
jsCompund x y = makeObj [("result",x),("events", JSArray $ map (JSString . toJSString . show) y)]

sendResponse g v = case v of 
        Nothing -> return $ sendJSON BadRequest $ jsError "Not parsed"
        Just v -> do
                (x,w) <- runWriterT $ g v
                case x of 
                        Left x -> return $ sendJSON BadRequest $ jsDBError $ x
                        Right x -> return $ sendJSON OK $ jsCompund x w
sendResponseP p v = case v of 
        Nothing -> return $ sendJSON BadRequest $ jsError "Not parsed"
        Just v -> do
                (x,w) <- runWriterT $ p v
                case x of 
                        Left x -> return $ sendJSON BadRequest $ jsDBError $ x
                        Right x -> return $ sendJSON OK $ jsCompund JSNull w

main :: IO ()
main = do 
        (p,g,_) <- prepare
        serverWith defaultConfig { srvLog = stdLogger, srvPort = 8888 }
                $ \_ url request -> do
                          print url
                          case rqMethod request of
                            POST -> do 
                                let msg = decodeString (rqBody request)
                                case splitOn "/" $ url_path url of
                                        ["Invite",sl] ->sendResponseP p $ do
                                                        return $ Invite sl msg
                                        ["Migrate",sl] ->sendResponseP p $ do
                                                        return $ Migrate sl msg
                                        ["Reminds"] ->sendResponseP p $ do
                                                        return $ Reminds msg
                                        ["NewMessage",sl,"DontAttach"] -> sendResponseP p $  Just $ NewMessage sl DontAttach msg
                                        ["NewMessage",sl,"AttachConversation",sci] -> sendResponseP p $ do
                                                        ci <- readMaybe sci
                                                        return $ NewMessage sl (AttachConversation ci) msg
                                        ["NewMessage",sl,"AttachMessage",smi] ->sendResponseP p $ do
                                                        mi <- readMaybe smi
                                                        return $ NewMessage sl (AttachMessage mi) msg
                                        _ -> return $ sendJSON BadRequest $ JSNull
                            PUT -> do 
                                 case splitOn "/" $ url_path url of
                                        ["VoteMessage",sl,smi,sb] ->sendResponseP p $ do
                                                        mi <- readMaybe smi
                                                        b <- readMaybe sb
                                                        return $ VoteMessage sl mi b
                                        ["StoreConversation",sl,sci] ->sendResponseP p $ do
                                                        ci <- readMaybe sci
                                                        return $ StoreConversation sl ci
                                        ["ForgetConversation",sl,sci] ->sendResponseP p $ do
                                                        ci <- readMaybe sci
                                                        return $ ForgetConversation sl ci
                                        ["HintConversation",sl] ->sendResponseP p $ do
                                                        return $ HintConversation sl 
                                        ["Logout",sl] ->sendResponseP p $ do
                                                        return $ Logout sl
                                        ["RetractMessage",sl,sci] ->sendResponseP p $ do
                                                        ci <- readMaybe sci
                                                        return $ RetractMessage sl ci
                                        ["LeaveConversation",sl,sci] ->sendResponseP p $ do
                                                        ci <- readMaybe sci
                                                        return $ LeaveConversation sl ci
                                        _ -> return $ sendJSON BadRequest $ JSNull
                            GET -> do 
                                case splitOn "/" $ url_path url of
                                        ["Login",sci] -> do
                                                s <- readFile "login.html"
                                                return $ sendHTML OK $ replace "userkey=" ("userkey='"++sci++"'") s
                                                
                                        ["GetMessages",sci,sn] -> sendResponse g $ do
                                                        ci <- readMaybe sci
                                                        n <- readMaybe sn
                                                        return $ GetMessages ci n
                                        ["GetStore",sl] ->  do
                                                        print "get store"
                                                        sendResponse g $Just $ GetStore sl 
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


