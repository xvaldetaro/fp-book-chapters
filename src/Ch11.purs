module Ch11 where

import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List (List(..), (:))
import Data.NonEmpty (NonEmpty(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Monoid, class Semiring, type (~>), Unit, discard, mempty, negate, show, zero, ($), (+), (-))

fold1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
fold1 fn (NonEmpty first l) = foldl fn first l

sum :: ∀ a f. Semiring a => Foldable f => f a -> a
sum = foldr (+) zero

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)

toListRight :: ∀ a. Tree a -> List a
toListRight tree = go Nil tree
  where
  go list (Leaf x) = x : list
  go list (Node tl tr) = go (go list tl) tr

toListLeft :: ∀ a. Tree a -> List a
toListLeft tree = go Nil tree
  where
  go list (Leaf x) = x : list
  go list (Node tl tr) = go (go list tr) tl

instance foldableTree :: Foldable Tree where
  foldr f acc (Leaf x) = f x acc
  foldr f acc (Node tl tr) = foldr f rightResult tl
    where
    rightResult = foldr f acc tr

  foldl f x tree = foldl f x (toListLeft tree)

  foldMap = mempty

test :: Effect Unit
test = do
  log $ show $ foldr (-) 0 (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
  log $ show $ sum (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))
