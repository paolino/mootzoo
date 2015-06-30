module JSON where

import Text.JSON

import DB.Messages
import DB.Exposed
import DB.Client
import DB.Labels
import DB0


instance JSON LabelNotification where
  showJSON (LabelNotification i j) = makeObj [("nbranches",showJSON i),("nmessages",showJSON j)]
instance JSON Event where
  showJSON (EvNewMessage mid) = makeObj [("newmessage",JSRational False $ fromIntegral mid)]
  showJSON _ = JSNull

instance JSON Notification where
  showJSON (Notification mis mh) = makeObj [
    ("branches",showJSON mis),
    ("followup",maybe JSNull (\(mi',n) -> makeObj [("head",showJSON mi'),("count",showJSON n)]) mh )
    ]
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


