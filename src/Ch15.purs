module Ch15 where

import Prelude

import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Semiring (class Semiring)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate p) = p

instance contravariantPredicate :: Contravariant Predicate where
  cmap fntion (Predicate p) = Predicate (p <<< fntion)

data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
  -- dimap :: ∀ a b c d. (b -> a) -> (c -> d) -> p a c -> p b d
  dimap f g (Moore s output step) = Moore s (g <<< output) (\x y -> step x (f y))

addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

sizer :: Moore Int String String
sizer = dimap length (\x -> "Size is " <> (show x)) addr

runFoldL :: ∀ s a b foldable. Foldable foldable => Moore s a b -> foldable a -> b
runFoldL (Moore s0 output transition) smr = output $ foldl transition s0 smr

class Foo f where
  foo :: ∀ a. f a

data Bar a b
  = Beft a
  | Bight b

instance functorBar :: Functor (Bar a) where
  map _ (Beft x) = Beft x
  map f (Bight x) = Bight $ f x

instance fooBar :: Monoid a => Foo (Bar a) where
  foo = Beft mempty

tes :: Bar String String
tes = foo

sh :: Bar String String -> String
sh (Beft a) = a <> "a"
sh _ = "right"

test :: Effect Unit
test = do
  log "------------------------------------"
  log $ sh tes