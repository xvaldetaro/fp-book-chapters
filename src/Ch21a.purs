module Ch21a where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, ask)
import Control.Monad.State (class MonadState, get, put, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Control.Monad.Writer.Trans (WriterT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Debug (spy)
import Effect (Effect)
import Effect.Console as Console
import Undefined (undefined)

data StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: ∀ s m a. StateT s m a -> (s -> m (Tuple a s))
runStateT (StateT f) = f

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT mg) = StateT $ \s -> mg s <#> \(Tuple x s') -> Tuple (f x) s'

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply (StateT ff) (StateT fx) = StateT $ \s -> do
    Tuple f s' <- ff s
    Tuple x s'' <- fx s'
    pure $ Tuple (f x) s''

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind (StateT fmx) gm = StateT $ \s -> do
    Tuple x s' <- fmx s
    runStateT (gm x) s'

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  -- ask = StateT $ \s -> ask <#> \x -> Tuple x s
  ask = lift ask

instance monadTellStateT :: MonadTell w m => MonadTell w (StateT s m) where
  -- tell w = StateT $ \s -> (tell w) <#> \_ -> Tuple unit s
  tell = lift <<< tell

instance monadTransStateT :: MonadTrans (StateT s) where
  lift m = StateT $ \s -> m <#> \x -> Tuple x s

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
  catchError :: ∀ a. StateT s m a -> (e -> StateT s m a) -> StateT s m a
  catchError (StateT fmx) f = StateT \s -> catchError (fmx s) \e -> runStateT (f e) s

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a
type AppM = StateT Int (ExceptT String (WriterT String Effect)) Unit
type StackResult = Tuple (Tuple (Either String Unit) String) Int
type AppEffects = {
  log :: String,
  state :: Int,
  result :: (Maybe Unit)
}
type AppResult = Tuple (Maybe String) AppEffects

app :: AppM
app = do
  log "Starting app..."
  n <- get
  catchError (validate n) (\err -> do
    s <- get
    let x = spy "s in error handler" s
    log $ "We encountered an error: (" <> err <> ")"
    put 100
  )
  s <- get
  let x = spy "s in app" s
  put (n + 1)
  log "Incremented state"
  pure unit

validate :: Int -> AppM
validate n = do
  s <- get
  let x = spy "s in validate" s
  log "HEY!!!!!!!!!!!!!!!!!!!" -- This will NOT be lost on the error case
  put 10 -- This will be lost on the error case but kept on success
  when (n == 0) $ void $ throwError "WE CANNOT HAVE A 0 STATE!"

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) {log: l, state: s, result: Nothing}
results (Tuple (Tuple (Right result) l) s) = Tuple Nothing {log: l, state: s, result: (Just result)}

runApp :: Int
  -> AppM
  -> Effect (Tuple (Either String (Tuple Unit Int)) String)
runApp s = runWriterT <<< runExceptT <<< flip runStateT s

log :: ∀ m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

test :: Effect Unit
test = do
  x1 <- runApp 0 app
  Console.log $ show $ x1
  x2 <- runApp 99 app
  Console.log $ show $ x2