-----------------------------------------------------------------------------
-- |
-- Operations over Self Balancing Trees using the AVL algorithm.
-- Useful to demonstrate recursive data structures.
--
-----------------------------------------------------------------------------

module Types.Tree where

import qualified Data.Foldable as F

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq)

instance (Show a) => Show (Tree a) where
    show EmptyTree = ""
    show (Node a EmptyTree EmptyTree) = show a
    show (Node a left right) = show a ++ "(" ++ show left ++ "|" ++ show right ++ ")"

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node a left right) =
        F.foldMap f left `mappend` f a `mappend` F.foldMap f right

leaf :: a -> Tree a
leaf x = Node x EmptyTree EmptyTree

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = leaf x
insert x (Node a left right)
    | x < a = balance $ Node a (insert x left) right
    | x > a = balance $ Node a left (insert x right)
    | otherwise = balance $ Node x left right

delete :: (Ord a) => a -> Tree a -> Tree a
delete element EmptyTree = EmptyTree
delete element tree@(Node a EmptyTree EmptyTree)
    | element == a = EmptyTree
    | otherwise = tree
delete element tree@(Node a EmptyTree right)
    | element == a = right
delete element tree@(Node a left EmptyTree)
    | element == a = left
delete element tree@(Node a left right)
    | element < a = balance $ Node a (delete element left) right
    | element > a = balance $ Node a left (delete element right)
    | element == a = balance $ Node rightMin left (delete rightMin right)
    | otherwise = tree
    where
        rightMin = treeMin right

balance :: Tree a -> Tree a
balance EmptyTree = EmptyTree
balance tree@(Node a left right)
    | treeBalanceFactor < -1 && balanceFactor left <= 0 = rotateToRight tree
    | treeBalanceFactor < -1 = rotateToRight (Node a (rotateToLeft left) right)
    | treeBalanceFactor > 1 && balanceFactor right >=0 = rotateToLeft tree
    | treeBalanceFactor > 1 = rotateToLeft (Node a left (rotateToRight right))
    | otherwise = tree
    where
        treeBalanceFactor = balanceFactor tree
        balanceFactor (Node _ left right) = height right - height left
        rotateToRight (Node b (Node a aLeft aRight) bRight) = Node a aLeft (Node b aRight bRight)
        rotateToLeft (Node a aLeft (Node b bLeft bRight)) = Node b (Node a aLeft bLeft) bRight
        height EmptyTree = 0
        height (Node _ left right) = 1 + max (height left) (height right)

fromList :: (Ord a) => [a] -> Tree a
fromList list = foldr insert EmptyTree $ reverse list

deleteList :: (Ord a) => [a] -> Tree a -> Tree a
deleteList [] tree = tree
deleteList list tree = foldr delete tree list

-- Does the element occur in the structure?
element :: (Ord a) => a -> Tree a -> Bool
element _ EmptyTree = False
element x (Node a left right)
    | x == a = True
    | x < a = element x left
    | x > a = element x right

levels :: (Eq a) => Tree a -> [[a]]
levels tree =
    map value
    <$> filter (/= EmptyTree)
    <$> (takeWhile (not . null) $ iterate (concatMap subtrees) [tree])
  where
    value (Node a _ _) = a
    subtrees EmptyTree = []
    subtrees (Node _ EmptyTree EmptyTree) = []
    subtrees (Node _ left right) = [left, right]

flatten :: Tree a -> [a]
flatten = inorder

inorder :: Tree a -> [a]
inorder EmptyTree = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

preorder :: Tree Char -> [Char]
preorder EmptyTree = []
preorder (Node a left right) = [a] ++ preorder left ++ preorder right

postorder :: Tree a -> [a]
postorder EmptyTree = []
postorder (Node a left right) = postorder left ++ postorder right ++ [a]

-- Empty trees not supported for the same reasons Data.List.head doesn't support empty lists
-- Further explanations at: https://stackoverflow.com/questions/6364409/why-does-haskells-head-crash-on-an-empty-list-or-why-doesnt-it-return-an/6364990
treeMin :: Tree a -> a
treeMin EmptyTree = error "No minimum for empty trees"
treeMin (Node a EmptyTree _) = a
treeMin (Node _ left _) = treeMin left

-- Empty trees not supported for the same reasons Data.List.head doesn't support empty lists
-- Further explanations at: https://stackoverflow.com/questions/6364409/why-does-haskells-head-crash-on-an-empty-list-or-why-doesnt-it-return-an/6364990
treeMax :: Tree a -> a
treeMax EmptyTree = error "No maximum for empty trees"
treeMax (Node a _ EmptyTree) = a
treeMax (Node _ _ right) = treeMax right

drawLevels :: (Eq a, Show a) => Tree a -> String
drawLevels tree = unlines $ map (concatMap show) (levels tree)

draw :: (Show a) => Tree a -> String
draw EmptyTree = []
draw (Node a left right) = show a ++ drawSubtree right "" ++ drawSubtree left ""
  where
    drawSubtree EmptyTree _ = []
    drawSubtree (Node a left right) prefix =
        drawEmptyLine
        ++ drawValueLine
        ++ drawSubtree right (prefix ++ "| ")
        ++ drawSubtree left (prefix ++ "| ")
      where
        drawEmptyLine = "\n" ++ prefix ++ "|"
        drawValueLine = "\n" ++ prefix ++ "+" ++ show a

-- >>> putStr $ draw $ (subtract 1) <$> (*2) <$> (deleteList [9] $ fromList [1..2^4-1])
-- 15
-- |
-- +23
-- | |
-- | +27
-- | | |
-- | | +29
-- | | |
-- | | +25
-- | |
-- | +19
-- | | |
-- | | +21
-- |
-- +7
-- | |
-- | +11
-- | | |
-- | | +13
-- | | |
-- | | +9
-- | |
-- | +3
-- | | |
-- | | +5
-- | | |
-- | | +1
--
