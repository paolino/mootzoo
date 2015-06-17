module JSON where

import Text.JSON
import DB0

import DB.Get

instance JSON Exposed where
        showJSON (Exposed mid mmp fs mtext mvote (Interface cv cp ci cr cc co cx)) = makeObj [
                ("id",JSRational False $ fromIntegral mid),
                ("parent",maybe JSNull (JSRational False . fromIntegral) mmp),
                ("future", JSArray $ map (JSRational False . fromIntegral) fs),
                ("text",JSString $ toJSString mtext),  
                ("vote",JSRational False $ fromIntegral mvote),
                ("canVote",JSBool cv),
                ("canPropose",JSBool cp),
                ("canIntervein",JSBool ci),
                ("canRespond",JSBool cr),
                ("canClose",JSBool cc),
                ("canOpen",JSBool co), 
                ("canRetract",JSBool cx) 
                ]

