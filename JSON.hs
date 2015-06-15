module JSON where

import Text.JSON
import DB0

import DB.Get

instance JSON MessageType where
        showJSON Closed = JSString $ toJSString "Closed"
        showJSON Open = JSString $ toJSString "Open"
        showJSON Passage = JSString $ toJSString "Passage"

instance JSON Exposed where
        showJSON (Exposed mid mvotable mtext mvote) = makeObj [
                ("id",JSRational False $ fromIntegral mid),
                ("votable",JSBool $ mvotable),
                ("text",JSString $ toJSString mtext),  
                ("vote",JSRational False $ fromIntegral mvote)
                ]

