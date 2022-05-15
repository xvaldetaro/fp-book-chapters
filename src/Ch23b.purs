module Ch23b where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReader, runReaderT)
import Control.Monad.Reader as ReaderT
import Control.Monad.Reader as StateT
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (StateT, get, modify_, put, runState, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, forkAff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Bus (Bus, BusRW)
import Effect.Aff.Bus as Bus
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (random)

randomAff :: Aff Number
randomAff = liftEffect random

type Config = { bus :: BusRW String }
type State = { count :: Int }

type FiberM a = ReaderT Config (StateT State Aff) a

lift' :: ∀ a. Aff a -> FiberM a
lift' = lift <<< lift

delayRandom :: Aff Number
delayRandom = delay (Milliseconds 1000.0) *> randomAff

randomGenerator :: (Number -> Boolean) -> FiberM Unit
randomGenerator pred = do
  { count } <- get
  { bus } <- ask
  if count <= 0 then do
    lift' $ Bus.write ("Count reached 0") bus
  else do
    n <- lift' delayRandom
    if pred n then
      lift' $ Bus.write ("Published " <> show n) bus
    else do
      modify_ \state -> state { count = count - 1 }
      randomGenerator pred

logger :: FiberM Unit
logger = do
  { bus } <- ask
  lift' $ go bus
  where
    go bus = forever do
      text <- Bus.read bus
      log $ text


runFiberM :: BusRW String -> FiberM Unit -> Aff Unit
runFiberM bus = void <<< forkAff <<< (flip runStateT { count: 3 }) <<< (flip runReaderT { bus })

test :: Effect Unit
test = launchAff_ do
  bus <- Bus.make
  let forkFiberM = runFiberM bus
  forkFiberM logger
  forkFiberM $ randomGenerator (_ > 0.5)
  forkFiberM $ randomGenerator (_ < 0.5)
  forkFiberM $ randomGenerator (_ < 0.1)
