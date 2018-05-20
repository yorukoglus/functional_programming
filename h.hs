import qualified Data.Map as M --(can be shortened)
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)
data Trie = Trie {end :: Bool, children :: M.Map Char Trie}
            deriving (Show)
type Word = String

empty :: Trie
empty = Trie {end = False, children = M.empty}

insert :: Word -> Trie -> Trie
insert []     t = t {end = True}
insert (c:cs) t = in case M.lookup c ts of
                          Nothing -> t { children = M.insert c (insert cs child_node) new_children }
						  Just t' -> t { children = M.insert c (insert cs t') ts }
						    where
							  ts = children t
							  child_node = 



insertList :: [Word] -> Trie
insertList = undefined
search :: Word -> Trie -> Bool
search = undefined
getWords :: Trie -> [Word]
getWords = undefined
prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

getTrie :: a [(Trie a)]
getTrie :: M.Map a (Trie a)

