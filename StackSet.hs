-----------------------------------------------------------------------------
-- |
-- Module      :  StackSet
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  portable, needs GHC 6.6
--
-----------------------------------------------------------------------------
--
-- The 'StackSet' data type encodes a set of stacks. A given stack in the
-- set is always current. Elements may appear only once in the entire
-- stack set.
--
-- A StackSet provides a nice data structure for multiscreen
-- window managers, where each screen has a stack of windows, and a window
-- may be on only 1 screen at any given time.
--

module StackSet where

import Data.Maybe
import qualified Data.List     as L (delete)
import qualified Data.Map      as M

------------------------------------------------------------------------

--
-- N.B we probably want to think about strict 'adjust' and inserts on
-- these data structures in the long run.
--

-- | The StackSet data structure. A table of stacks, with a current pointer
data StackSet a =
    StackSet
        { current  :: !Int              -- ^ the currently visible stack
        , ws2screen:: !(M.Map Int Int)  -- ^ workspace -> screen map
        , screen2ws:: !(M.Map Int Int)  -- ^ screen -> workspace
        , stacks   :: !(M.Map Int [a])  -- ^ the separate stacks
        , focus    :: !(M.Map Int a)    -- ^ the window focused in each stack
        , cache    :: !(M.Map a Int)    -- ^ a cache of windows back to their stacks
        } deriving Eq

instance Show a => Show (StackSet a) where
    showsPrec p s r = showsPrec p (show . toList $ s) r

-- Ord a constraint on 'a' as we use it as a key.
--
-- The cache is used to check on insertion that we don't already have
-- this window managed on another stack

------------------------------------------------------------------------

-- | /O(n)/. Create a new empty stacks of size 'n', indexed from 0, with 'm'
-- screens. (also indexed from 0) The 0-indexed stack will be current.
empty :: Int -> Int -> StackSet a
empty n m = StackSet { current   = 0
                     , ws2screen = wsScreenAssn
                     , screen2ws = wsScreenAssn
                     , stacks    = M.fromList (zip [0..n-1] (repeat []))
                     , focus     = M.empty
                     , cache     = M.empty }
    where wsScreenAssn = M.fromList $ map (\x -> (x,x)) [0..m-1]
  
-- | /O(log w)/. True if x is somewhere in the StackSet
member :: Ord a => a -> StackSet a -> Bool
member a w = M.member a (cache w)

-- | /O(log n)/. Looks up the stack that x is in, if it is in the StackSet
lookup :: (Monad m, Ord a) => a -> StackSet a -> m Int
lookup x w = M.lookup x (cache w)

-- | /O(n)/. Number of stacks
size :: StackSet a -> Int
size = M.size . stacks

------------------------------------------------------------------------

-- | fromList. Build a new StackSet from a list of list of elements
-- If there are duplicates in the list, the last occurence wins.
-- FIXME: This always makes a StackSet with 1 screen.
fromList :: Ord a => (Int,[[a]]) -> StackSet a
fromList (_,[]) = error "Cannot build a StackSet from an empty list"

fromList (n,xs) | n < 0 || n >= length xs
                = error $ "Cursor index is out of range: " ++ show (n, length xs)

fromList (o,xs) = view o $ foldr (\(i,ys) s ->
                                foldr (\a t -> insert a i t) s ys)
                                    (empty (length xs) 1) (zip [0..] xs)

-- | toList. Flatten a stackset to a list of lists
toList  :: StackSet a -> (Int,[[a]])
toList x = (current x, map snd $ M.toList (stacks x))

-- | Push. Insert an element onto the top of the current stack.
-- If the element is already in the current stack, it is moved to the top.
-- If the element is managed on another stack, it is removed from that
-- stack first.
push :: Ord a => a -> StackSet a -> StackSet a
push k w = insert k (current w) w

-- | /O(log s)/. Extract the element on the top of the current stack. If no such
-- element exists, Nothing is returned.
peek :: StackSet a -> Maybe a
peek w = peekStack (current w) w

-- | /O(log s)/. Extract the element on the top of the given stack. If no such
-- element exists, Nothing is returned.
peekStack :: Int -> StackSet a -> Maybe a
peekStack n w = M.lookup n (focus w)

-- | /O(log s)/. Index. Extract the stack at index 'n'.
-- If the index is invalid, an exception is thrown.
index :: Int -> StackSet a -> [a]
index k w = fromJust (M.lookup k (stacks w))

-- | view. Set the stack specified by the Int argument as being visible and the
-- current StackSet. If the stack wasn't previously visible, it will become
-- visible on the current screen. If the index is out of range an exception is
-- thrown.
view :: Int -> StackSet a -> StackSet a
view n w | n >= 0 && n < M.size (stacks w) = if M.member n (ws2screen w)
                                                 then w { current = n }
                                                 else tweak (fromJust $ screen (current w) w)
         | otherwise                       = error $ "view: index out of bounds: " ++ show n
  where
    tweak sc = w { screen2ws = M.insert sc n (screen2ws w)
                 , ws2screen = M.insert n sc (M.filter (/=sc) (ws2screen w))
                 , current = n
                 }

-- | That screen that workspace 'n' is visible on, if any.
screen :: Int -> StackSet a -> Maybe Int
screen n w = M.lookup n (ws2screen w)

-- | The workspace visible on screen 'sc'. Nothing if screen is out of bounds.
workspace :: Int -> StackSet a -> Maybe Int
workspace sc w = M.lookup sc $ ws2screen w

-- | A list of the currently visible workspaces.
visibleWorkspaces :: StackSet a -> [Int]
visibleWorkspaces = M.keys . ws2screen

--
-- | /O(log n)/. rotate. cycle the current window list up or down.
--
--  rotate EQ   -->  [5,6,7,8,1,2,3,4]
--  rotate GT   -->  [6,7,8,1,2,3,4,5]
--  rotate LT   -->  [4,5,6,7,8,1,2,3]
--
--  where xs = [5..8] ++ [1..4]
--
rotate :: Eq a => Ordering -> StackSet a -> StackSet a
rotate o w = maybe w id $ do
    f <- M.lookup (current w) (focus w)
    s <- M.lookup (current w) (stacks w)
    ea <- case o of
            EQ -> Nothing
            GT -> elemAfter f s
            LT -> elemAfter f (reverse s)
    return (w { focus = M.insert (current w) ea (focus w) })

-- | /O(log n)/. shift. move the client on top of the current stack to
-- the top of stack 'n'. If the stack to move to is not valid, and
-- exception is thrown.
--
shift :: Ord a => Int -> StackSet a -> StackSet a
shift n w = maybe w (\k -> insert k n (delete k w)) (peek w)

-- | /O(log n)/. Insert an element onto the top of stack 'n'.
-- If the element is already in the stack 'n', it is moved to the top.
-- If the element exists on another stack, it is removed from that stack.
-- If the index is wrong an exception is thrown.
--
insert :: Ord a => a -> Int -> StackSet a -> StackSet a
insert k n old = new { cache  = M.insert k n (cache new)
                     , stacks = M.adjust (k:) n (stacks new)
                     , focus  = M.insert n k (focus new) }
    where new = delete k old

-- | /O(log n)/. Delete an element entirely from from the StackSet.
-- This can be used to ensure that a given element is not managed elsewhere.
-- If the element doesn't exist, the original StackSet is returned unmodified.
delete :: Ord a => a -> StackSet a -> StackSet a
delete k w = maybe w tweak (M.lookup k (cache w))
  where
    tweak i = w { cache  = M.delete k (cache w)
                , stacks = M.adjust (L.delete k) i (stacks w)
                , focus  = M.update (\k' -> if k == k' then elemAfter k (stacks w M.! i)
                                                       else Just k') i
                                    (focus w)
                }

-- | /O(log n)/. If the given window is contained in a workspace, make it the
-- focused window of that workspace, and make that workspace the current one.
raiseFocus :: Ord a => a -> StackSet a -> StackSet a
raiseFocus k w = case M.lookup k (cache w) of
    Nothing -> w
    Just i  -> (view i w) { focus = M.insert i k (focus w) }

-- | Move a window to the top of its workspace.
promote :: Ord a => a -> StackSet a -> StackSet a
promote k w = case M.lookup k (cache w) of
    Nothing -> w
    Just i  -> w { stacks = M.adjust (\ks -> k : filter (/= k) ks) i (stacks w) }

-- |
elemAfter :: Eq a => a -> [a] -> Maybe a
elemAfter w ws = listToMaybe . filter (/= w) . dropWhile (/= w) $ ws ++ ws
