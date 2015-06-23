{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Mailer where 

import Prelude hiding (readFile, putStrLn)
import Control.Monad
import Data.Text.Lazy.IO (readFile,putStrLn)
import Data.Text.Lazy (Text,replace,pack)
import qualified Data.Text as S (pack)
import Network.Mail.Client.Gmail
import Network.Mail.Mime (Address (..))
import DB0

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
        sendGmail "mootzoo.service" (pack pwd) (Address (Just "mootzoo service") "mootzoo.service@gmail.com") [Address (Just m) m] [] [] t b [] 50000000

