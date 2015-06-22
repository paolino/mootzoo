module JSON where

import Text.JSON
import DB0

import DB.Get
import DB0

instance JSON Event where
  showJSON (EvNewMessage mid) = makeObj [("newmessage",JSRational False $ fromIntegral mid)]
  showJSON _ = JSNull

instance JSON Exposed where
        showJSON (Exposed mid mdate mpid mtext mvote fs (Interface cv cp ci cr cc co cx)) = makeObj $ [
                ("id",JSRational False . fromIntegral $ mid),
                ("date",JSString $ toJSString $ mdate),
                ("parent",maybe JSNull (JSRational False . fromIntegral) mpid),
                ("text",JSString $ toJSString mtext),  
                ("vote",JSRational False $ fromIntegral mvote),
                ("alter", JSArray $ map showJSON  fs), 
                ("canVote",JSBool cv),
                ("canPropose",JSBool cp),
                ("canIntervein",JSBool ci),
                ("canRespond",JSBool cr),
                ("canClose",JSBool cc),
                ("canOpen",JSBool co), 
                ("canRetract",JSBool cx) 
                ]


