type Message = String

type User = String

data Partecipation 
        = None
        | One User 
        | Two User User

data Color 
        = Blank
        | Orphan
        | Personal
        | Waiting
        | Complete
        | Conversata
        deriving Show

get :: User -> Partecipation -> Color

get u None          = Blank
get u (One u') 
        | u == u'   = Personal
        | u /= u'   = Orphan
get u (Two u' u'') 
        | u == u'   = Conversata
        | u == u''  = Waiting
        | True      = Complete

put :: User -> Color -> Partecipation -> Partecipation


put u  Personal   None                     = One u
put u  Blank      o@(One u')      |u == u' = None
                                  |True    = o
put u  Conversata o@(One u')      |u /= u' = Two u' u
                                  |True    = o
put u  Orphan     t@(Two u' u'')  |u == u''= One u'
                                  |u == u' = One u''
                                  |True    = t
put _ _ x = x
