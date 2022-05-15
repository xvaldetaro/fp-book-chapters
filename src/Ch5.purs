module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (+), (-), (/=), (<), (==), (>), (>=))

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f a b = f b a

const :: ∀ a b. a -> b -> a
const a _ = a

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped x f = f x

infixl 1 applyFlipped as #

singleton :: ∀ a . a -> List a
singleton x = Cons x Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc (x : xs) y = x : (snoc xs y)
snoc Nil y = singleton y

length :: ∀ a. List a -> Int
length (_ : xs) = length xs + 1
length Nil = 0

head :: ∀ a. List a -> Maybe a
head (x : _) = Just x
head Nil = Nothing

last :: ∀ a. List a -> Maybe a
last (x : Nil) = Just x
last (_ : xs) = last xs
last Nil = Nothing

tail :: ∀ a. List a -> Maybe (List a)
tail (_ : xs) = Just xs
tail Nil = Nothing

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init (_ : Nil) = Just Nil
init (x : xs) = case init xs of
  Nothing -> Nothing
  Just y -> Just (x : y)

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (_ : Nil) = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex predicate list = go list 0 where
  go :: List a -> Int -> Maybe Int
  go Nil _ = Nothing
  go (x : xs) i = if predicate x == true then Just i else go xs (i + 1)

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex predicate list = go Nothing list 0 where
  go :: Maybe Int -> List a -> Int -> Maybe Int
  go fi Nil _ = fi
  go fi (x : xs) i = go (if predicate x == true then Just i else fi) xs (i + 1)

reverse :: List ~> List
reverse l = go Nil l where
  go newList Nil = newList
  go newList (x : xs) = go (x : newList) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xs) = concat xs
concat ((y : ys) : xs) = y : (concat (ys : xs))

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter pred l = reverse $ go Nil l where
-- filter pred (x : xs) = if pred x == false then filter pred xs else x : filter pred xs
  go newList Nil = newList
  go newList (x : xs) = go (if pred x then x : newList else newList) xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

range :: Int -> Int -> List Int
range start end = go Nil end
  where
    go rl end'
      | end' == start = end' : rl
      | otherwise     = go (end' : rl) (end' + incr)
    incr = if start > end then 1 else (-1)

take :: ∀ a. Int -> List a -> List a
take amount l = reverse $ go Nil l amount
  where
    go rl Nil _ = rl
    go rl _ 0 = rl
    go rl (x : xs) amount' = go (x : rl) xs (amount' - 1)

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd amount l' = snd $ go l'
  where
    go Nil = Tuple amount Nil
    go (x : xs) =
      let Tuple amount' l = go xs in
      if amount' == 0 then Tuple 0 l else Tuple (amount' - 1) (x : l)

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd amount l = snd $ go l
  where
    go Nil = Tuple 0 Nil
    go (x : xs) = go xs # \(Tuple remainder rl) -> Tuple (remainder + 1) (if remainder < amount then Nil else x : rl)


takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile pred l = reverse $ go Nil l
  where
    go rl Nil = rl
    go rl (x : xs)
      | pred x = go (x : rl) xs
      | otherwise = rl

drop :: ∀ a. Int -> List a -> List a
drop 0 l = l
drop _ Nil = Nil
drop amount' (_ : xs) = drop (amount' - 1) xs

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs)
  | pred x = dropWhile pred xs
  | otherwise = l

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip ((Tuple xa xb) : xs) = unzip xs # \(Tuple xsa xsb) -> Tuple (xa : xsa) (xb : xsb)

test :: Effect Unit
test = do
  log $ show $ unzip $ zip (1 : 2 : 3 : Nil) ("a" : "b" : "c" : "d" : "e" : Nil)
  log $ show $ unzip $ zip ("a" : "b" : "c" : "d" : "e" : Nil) (1 : 2 : 3 : Nil)
  log $ show $ unzip $ zip (Nil :: List Unit) (1 : 2 : Nil)
  -- log $ show $ range 9 (-3)
  -- log $ show $ init (Cons 1 Nil)