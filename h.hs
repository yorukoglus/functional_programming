import qualified Data.Map as M --(can be shortened)
import qualified Data.Foldable as F
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


keyList :: [(k,l)] -> [k]
keyList ts = fst((unzip ts))


getWords :: Trie -> Word
getWords t = concat $ getIter t []

  
getIter :: Trie -> [Word] -> [Word]
getIter t ws = case M.lookup c ts of 
 Just t' -> if (end t'==True) then [[c]]++ws else (getIter t' [[c]]++ws)
 Nothing -> ws
 where
  ts = children t
  l@(c:cs)=keyList (M.toList ts)

find_prefix = undefined
  
print_trie :: Trie -> IO ()
print_trie t = print $ M.showTree (children t)

getInput :: IO String
getInput = do inp <- getLine
              return inp

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
     "f" -> find_prefix
     "p" -> print_trie in_t
     otherwise -> do 
         putStrLn "Input is incorrect!\n------------------------------"
         mainloop in_t
    