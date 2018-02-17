import System.Random
import Control.Applicative
import Control.Monad.State

data Tree a = Node (Tree a) a (Tree a) | Empty
  deriving Show

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node ln v rn)
  | x < v = Node (insert x ln) v rn
  | otherwise = Node ln v (insert x rn)

remove :: (Ord a) => a -> Tree a -> Tree a
remove x (Node ln v rn)
  | x == v = remove_aux (Node ln v rn)
  | x < v = (Node (remove x ln) v rn)
  | x > v = (Node ln v (remove x rn))

remove_aux (Node Empty _ rn) = rn
remove_aux (Node ln _ Empty) = ln
remove_aux (Node ln _ rn) = let leftmost_value = leftmost rn in
                              (Node ln leftmost_value (remove leftmost_value rn))

leftmost :: Tree a -> a
leftmost Empty = error "empty tree"
leftmost (Node Empty v _) = v
leftmost (Node ln _ _) = leftmost ln

extract :: Tree a -> Maybe a
extract Empty = Nothing
extract (Node _ v _) = Just v

main = undefined

example = insert (-3) $ insert 3 $ insert (-2) $ insert (-5) $ insert 12 $ insert 23 Empty
