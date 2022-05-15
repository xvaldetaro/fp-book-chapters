module Ch13 where

import Control.Plus (class Alt, class Plus)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Monoid, class Show, Unit, discard, mempty, show, ($), (+), (/), (<<<), (==))

data Either a b
  = Left a
  | Right b

data Maybe a
  = Nothing
  | Just a

data Tuple a b = Tuple a b

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show x = genericShow x

derive instance genericMaybe :: Generic (Maybe a) _
instance showMaybe :: Show a => Show (Maybe a) where
  show x = genericShow x

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq (Just x) (Just y) = x == y
  eq Nothing Nothing = true
  eq _ _ = false

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right x) = Right $ f x

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x

-- class Contravariant f where
--   cmap :: ∀ a b. (b -> a) -> f a -> f b

newtype F4 a = F4 ((a -> Int) -> Int)

-- instance conF4 :: Contravariant F4 where

cmap :: ∀ a b. (a -> b) -> F4 a -> F4 b
cmap f (F4 g) = F4 go
  where
  go :: (b -> Int) -> Int
  go bToInt = g (\x -> bToInt (f x))

test :: Effect Unit
test = do
  -- log $ show $ Just 10
  log $ show $ ((f2 <<< f1) <$> j2) == ((f2 <$> _) <<< (f1 <$> _)) j2
  log $ show $ ((f2 <<< f1) <$> n) == ((map f2) <<< (map f1)) n
    where
    f1 = (_ / 2)
    f2 = (_ + 1)
    j2 = Just 2
    n = (Nothing :: Maybe Int)
  -- log $ show $ (_ / 2) <$> Left "asdf"
