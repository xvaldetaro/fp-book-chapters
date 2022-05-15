module Ch7a where

import Data.Eq (class Eq)
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude (class Ord, class Show, Ordering(..), Unit, compare, discard, flip, show, ($), (==), (||), (<>))

data Maybe a = Nothing | Just a

derive instance eqMaybe :: Eq a => Eq (Maybe a)
  -- eq (Just x) (Just y) = x == y
  -- eq Nothing Nothing = true
  -- eq _ _ = false

derive instance ordMaybe :: Ord a => Ord (Maybe a)

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show x = genericShow x

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq x y = comparison == GT || comparison == EQ
  where
    comparison = (x `compare` y)

infixl 4 greaterThanOrEq as >=

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a,  Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a,  Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show x = genericShow x


test :: Effect Unit
test = do
  log $ show $ "aaa"
  -- log $ show $ Left "left"
  -- log $ show $ Right (Just 3)