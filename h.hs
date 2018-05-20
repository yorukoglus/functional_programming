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
insert (c:cs) t = case M.lookup c ts of
                   Just t' -> t { children = M.insert c (insert cs t') ts }
                   Nothing -> t { children = M.insert c (insert cs child_node) (M.insert c child_node ts) }
                   where
                    ts = children t
                    child_node = empty

insertList :: [Word] -> Trie
insertList = undefined
search :: Word -> Trie -> Bool
search = undefined
getWords :: Trie -> [Word]
getWords = undefined
prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined


