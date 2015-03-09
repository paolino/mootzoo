{-# LANGUAGE ViewPatterns #-}

import Data.List (find, partition)
import Data.Maybe (catMaybes,isJust,fromJust)
import Control.Monad (forM_)
import Control.Arrow ((***))
import Logic
import Browser

-- int indexing
type Idx a = [(Int,a)]

aId ::  Eq a => a -> Idx a -> Maybe (Int,a)
aId a = find ((==a).snd)

idA :: Int -> Idx a -> Maybe (Int,a)
idA i = find ((==i).fst)



type Links = Idx Int


fromUser' :: Int -> Links  -> [Int]
fromUser' u = map snd . filter ((==u) . fst) 

fromUser :: Idx User -> User -> Links  -> Maybe [Int]
fromUser iu  u ls = do
	(u',_) <-  aId u iu 
	return $ fromUser' u' ls
	
	
fromConversation' :: Int -> Links -> [Int]
fromConversation' c = map fst . filter ((==c) . snd)

fromConversation :: Idx User  -> Int -> Links  -> Maybe [User]
fromConversation iu c ls = do
	let cs = fromConversation' c ls
	return $ fmap snd $ catMaybes (map (flip idA iu) cs)


data Authored a = Authored a Int Int

data DB = DB 
	{	users :: Idx User
	,	conversations :: Idx Conversation
	,	links :: Links
	}

waiting :: User -> [Conversation] -> ([Conversation],[Conversation])
waiting u = partition (isJust . insert (Message u ""))

userspace :: Int -> User -> DB -> Maybe ([Browser],[Browser])
userspace (browser -> b) u (DB us cs ls) = do
	csi <- fromUser us u ls 
	let 	(c1,c2) = waiting u cs'
		cs' = map snd . catMaybes . map (flip idA cs) $ csi
	return (catMaybes $ map b c1, catMaybes $ map b c2) 


booting = DB [(0,"paolino"),(1,"tazzo")] [(0,fromJust $ construct [Message "paolino" "siamo online",Message "tazzo" "era l'ora"])] [(0,0),(1,0)]

us0 = Just (1,0) == (userspace 4 "paolino" booting >>= return . (length *** length))
us1 = Just (0,1) == (userspace 4 "tazzo" booting >>= return . (length *** length))


main = do 
	putStr "user:"
	u <- getLine
	case userspace 1 u booting of 
		Nothing -> putStrLn "unknown"
		Just (b1,b2) -> do
			let loop b action = do
					mb <- action b 
					case mb of
						Nothing -> return ()
						Just b' -> loop b' action 
			let action conv b = do
					let ls = now b
					forM_ ls $ \(Message u m) -> putStrLn $ u ++ ":\"" ++ m ++ "\""
					case down b of
						Left c -> conv c
						Right b' -> do
							return $ Just b'
			forM_ b1 $ \b -> do
				loop b . action $ \_ -> do
					putStr ">?"
					new <- getLine
					case new of 
						[]  -> putStrLn "---------------" >> return Nothing
						mess -> putStrLn "adding message not implemented" >> return Nothing
					
			forM_ b2 $ \b -> do
				loop b . action $ \_ -> do
					putStrLn "---------------"
					return Nothing

				
			
					
