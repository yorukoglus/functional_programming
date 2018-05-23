-------------------------
--   SAMET YÖRÜKOĞLU   --
--     150140125       --
-- Trie Implementation --
--    Term Project     --
-------------------------
import qualified Data.Map as M 
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)
data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
            deriving (Show,Read)
type Word = String

empty :: Trie
empty = Trie {end = False, children = M.empty}

insert :: Word -> Trie -> Trie
insert []     t = t {end = True}
insert (c:cs) t = case M.lookup c ts of
                   Just t' -> t { end = False , children = M.insert c (insert cs t') ts }
                   Nothing -> t { children = M.insert c (insert cs child_node) (M.insert c child_node ts)}
                   where
                    ts = children t
                    child_node = empty

insertList :: [Word] -> Trie
insertList ws = foldr insert empty ws

search :: Word -> Trie -> Bool
search [c]    t = case M.lookup c ts of
                   Nothing -> False
                   Just t' -> (if (end t'==True) then True else False)
                   where
                    ts = children t
search (c:cs) t = case M.lookup c ts of
                   Nothing -> False
                   Just t' -> search cs t'
                   where
                    ts = children t


getWords :: Trie -> [Word]
getWords t =(getIter t "" [])
  
getIter :: Trie -> Word -> [Word] -> [Word]
getIter t w ws = case end t of 
 True  -> if ((length list) >0) then (get__it list w (w:ws)) else (w:ws) 
 False -> get__it list w ws
 where
   ts = children t
   list = M.toList ts

get__it :: [(Char,Trie)] -> Word -> [Word] -> [Word]
get__it []     _ _  = []
get__it (x:xs) w ws = (getIter (snd x) (w++[fst(x)]) ws) ++ get__it xs w ws

prefix :: Word -> Trie -> Maybe [Word]
prefix [c]    t = case M.lookup c ts of
                   Nothing -> Nothing
                   Just t' -> Just $ getWords t'
                   where
                    ts = children t
prefix (c:cs) t = case M.lookup c ts of
                   Nothing -> Nothing
                   Just t' -> prefix cs t'
                   where
                    ts = children t

add_prefix :: [Word] -> Word -> [Word]
add_prefix [] _ = []
add_prefix (w:ws) str = [(concat [str,w])]++(add_prefix ws str)

main :: IO ()
main = do 
    args <- getArgs
    let file_name = head args
    handle <- openFile file_name ReadMode
    contents <- hGetContents handle
    let t = insertList $ lines contents
    mainloop t
    hClose handle


mainloop :: Trie -> IO ()
mainloop in_t = 
 do 
    putStrLn "a) Add Word"
    putStrLn "s) Search Word"
    putStrLn "f) Find words with prefix"
    putStrLn "p) Print all words"
    putStrLn "e) Exit\nEnter the action:"
    c <- getLine
    case c of
     "e" -> return ()
     "s" -> do
         putStrLn "Enter word/prefix:"
         str <- getLine
         if search str in_t then do 
          putStrLn "Exists in dictionary!\n------------------------------"
          mainloop in_t
         else do 
          putStrLn "NOT exist!\n------------------------------"
          mainloop in_t
     "a" -> do 
         putStrLn "Enter word/prefix:"
         str <- getLine
         let new_t = insert str in_t
         putStrLn "New word is added!\n------------------------------"
         mainloop new_t
     "f" -> do
         putStrLn "Enter word/prefix:"
         str <- getLine
         let Just a = prefix str in_t
         let b = add_prefix a str
         print $ b
         putStrLn "------------------------------"
         mainloop in_t
     "p" -> do 
         mapM_ print $ getWords in_t
         putStrLn "------------------------------"
         mainloop in_t
     otherwise -> do 
         putStrLn "Input is incorrect!\n------------------------------"
         mainloop in_t
