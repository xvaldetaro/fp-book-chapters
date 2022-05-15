module Ch9 where

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (+), (==))

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a

data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _
instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _  = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

data Mod4 = Zero | One | Two | Three

newtype First a = First (Maybe a)
newtype Last a = Last (Maybe a)

instance semigroupFirst :: Semigroup (First a) where
  append (First (Just x)) _ = First $ Just x
  append _ (First (Just x)) = First $ Just x
  append (First Nothing) (First Nothing) = First Nothing

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

instance semigroupLast :: Semigroup (Last a) where
  append first (Last Nothing) = first
  append _ last = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing

length :: ∀ a. List a -> Int
length l = foldl (\acc _ -> acc + 1) 0 l

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "verifying AndBool Semigroup Laws (1)"
  log $ show $ ATrue <> (ATrue <> AFalse) == (ATrue <> ATrue) <> AFalse

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "verifying AndBool Monoid Laws (1)"
  log $ show $ ATrue <> mempty == ATrue
  log $ show $ AFalse <> mempty == AFalse

test :: Effect Unit
test = do
  log $ show $ length $ (1 : 2 : 3 : 4 : Nil)