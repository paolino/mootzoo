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

getTemplateMail m (Booting l) href = do
        x <- readFile "invitation.txt"
        let x' = replace "invitante" "mootzoo service" $ replace "linklogin" (pack $ href ++"/Login/" ++ l)$ x
        return ("Booting a new MootZoo instance",S.pack m,x')
getTemplateMail m (Invitation m' l) href = do
        x <- readFile "invitation.txt"
        let x' = replace "invitante" (pack m') $ replace "linklogin" (pack $ href ++"/Login/" ++ l) $ x
        return ("Invito ad accedere a MootZoo, la foresta di discussioni",S.pack m,x')
getTemplateMail m (Reminding l) href = do
        x <- readFile "reminder.txt"
        let x' = replace "linklogin" (pack $ href ++"/Login/" ++ l) $ x
        return ("Il tuo login link per accedere a MootZoo",S.pack m,x')
getTemplateMail m (LogginOut l) href = do
        x <- readFile "newlogin.txt"
        let x' = replace "linklogin" (pack $ href ++"/Login/" ++ l) $ x
        return ("Il tuo nuovo login link per accedere a MootZoo",S.pack m,x')

sendAMail :: String -> String -> Mail -> String -> Mailer -> IO ()
sendAMail mail pwd as href ty = do
        (t,m,b) <- getTemplateMail as ty href 
        sendGmail (pack mail) (pack pwd) (Address (Just "mootzoo service") $ S.pack $ mail ++ "@gmail.com") [Address (Just m) m] [] [] t b [] 50000000

