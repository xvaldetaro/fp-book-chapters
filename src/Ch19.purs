module Ch19 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a
derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map f (Just x) = Just $ f x
  map _ Nothing = Nothing

instance applyMaybe :: Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) mx = f <$> mx

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind Maybe where
  bind (Just x) f = f x
  bind Nothing _ = Nothing

instance monadMaybe :: Monad Maybe

data Either a b = Left a | Right b
derive instance functorEither :: Functor (Either a)
derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance applyEither :: Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) mx = f <$> mx

instance applicativeEither :: Applicative (Either a) where
  pure = Right

instance bindEither :: Bind (Either a) where
  bind (Left x) _ = Left x
  bind (Right x) f = f x

instance monadEither :: Monad (Either a)

newtype Reader r a = Reader (r -> a)
newtype Writer w a = Writer (Tuple a w)
newtype State s a = State (s -> Tuple a s)

type RWSResult r w s = { r :: r, w :: w, s :: s }

newtype RWS r w s a = RWS (RWSResult r w s -> Tuple a (RWSResult r w s))

runRWS :: ∀ r w s a. RWS r w s a -> (RWSResult r w s -> Tuple a (RWSResult r w s))
runRWS (RWS f) = f

instance functorRWS :: Functor (RWS r w s) where
  map f m = RWS $ \rws -> runRWS m rws # \(Tuple x {r, w, s}) -> Tuple (f x) {r, w, s}

instance applyRWS :: Monoid w => Apply (RWS r w s) where
  apply = ap

instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
  pure x = RWS \{r, s} -> Tuple x {r, w: mempty, s}

instance bindRWS :: Monoid w => Bind (RWS r w s) where
  bind m fm =
    RWS \rws -> runRWS m rws #
      \(Tuple x rws'@{ w }) -> runRWS (fm x) rws' #
          \(Tuple x' rws''@{ w: w' }) -> Tuple x' rws'' { w = w <> w' }

instance monadRWS :: Monoid w => Monad (RWS r w s)

tell :: ∀ r w s. w -> RWS r w s Unit
tell w = RWS $ \{r, s} -> Tuple unit {r, w, s}

ask :: ∀ r w s. Monoid w => RWS r w s r
ask = RWS $ \{r, s} -> Tuple r {r, w: mempty, s}

get :: ∀ r w s. Monoid w => RWS r w s s
get = RWS $ \{r, s} -> Tuple s {r, w: mempty, s}

put :: ∀ r w s. Monoid w => s -> RWS r w s Unit
put s = RWS $ \{r} -> Tuple unit {r, w: mempty, s}

type Config = { debugModeOn :: Boolean }
type Counter = Int

rwsTest :: RWS Config (Array String) Counter Unit
rwsTest = do
  tell ["test the log"]
  tell ["test the log2", "test the log3"]
  config <- ask
  tell ["the config is " <> show config]
  counter <- get
  tell ["old counter is " <> show counter]
  put $ counter + 1
  newCounter <- get
  tell ["new counter is " <> show newCounter]
  pure unit

test :: Effect Unit
test = do
  log $ show $ runRWS rwsTest { r: {debugModeOn: true}, w: mempty, s: 0}