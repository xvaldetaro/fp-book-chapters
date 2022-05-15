module Summary where

import Prelude

import Data.Tuple (Tuple)

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

class (Functor f) <= Apply f where
  apply :: ∀ a b. f (a -> b) -> f a -> f b

class (Apply m) <= Bind m where
  bind :: ∀ a b. m a -> (a -> m b) -> m b

class (Apply f) <= Applicative f where
  pure :: ∀ a. a -> f a

class (Applicative m, Bind m) <= Monad m

class Functor f <= Alt f where
  alt :: ∀ a. f a -> f a -> f a

infixl 3 alt as <|>

class Monad m <= MonadAsk r m | m -> r where
  ask :: m r

class Monad m <= MonadState s m | m -> s where
  state :: ∀ a. (s -> Tuple a s) -> m a

class Monad m <= MonadTell w m | m -> w where
  tell :: w -> m Unit

class MonadTrans t where
  lift :: ∀ m a. Monad m => m a -> t m a

class Monad m <= MonadThrow e m | m -> e where
  throwError :: ∀ a. e -> m a

class MonadThrow e m <= MonadError e m | m -> e where
  catchError :: ∀ a. m a -> (e -> m a) -> m a

data StateT s m a = StateT (s -> m (Tuple a s))
