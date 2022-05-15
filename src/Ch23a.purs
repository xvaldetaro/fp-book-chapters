module Ch23a where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_, makeAff, nonCanceler)
import Effect.Aff.AVar (AVar, read)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)
import Effect.Random (random)

data TikTok = Tik | Tok
derive instance genericTikTok :: Generic TikTok _
instance showTikTok :: Show TikTok where
  show = genericShow
derive instance eqTikTok :: Eq TikTok

signal :: TikTok -> TikTok -> AVar TikTok -> Aff Unit
signal inT outT aVar = do
  delay (Milliseconds 100.0)
  currentT <- AVar.take aVar
  didPut <- AVar.tryPut inT aVar
  signal outT inT aVar

kill :: ∀ a. Fiber a -> Aff Unit
kill = killFiber (error "Killing you softly...")

data BombState = WaitingTick | WaitingTock

bomb :: AVar TikTok -> Int -> Aff Unit
bomb ttAVar detinationCount = go 0 WaitingTick where
  go :: Int -> BombState -> Aff Unit
  go count state = do
    if count == detinationCount then log "BOOM!!"
    else do
      delay (Milliseconds 50.0)
      tt <- AVar.read ttAVar
      case state of
        WaitingTick ->
          if tt == Tik then log "Tick" *> go count WaitingTock
          else go count state
        WaitingTock ->
          if tt == Tok then log "Tock" *> go (count + 1) WaitingTick
          else go count state

randomAff :: Aff Number
randomAff = makeAff $ \f -> do
  x <- random
  void $ f $ Right x
  pure nonCanceler

test :: Effect Unit
test = launchAff_ do
  aVar <- AVar.new Tik
  tikFiber <- forkAff $ signal Tik Tok aVar
  bombFiber <- forkAff $ bomb aVar 3
  log $ "blocked on joinFiber"
  joinFiber bombFiber
  log $ "joinFiber continued"
  kill tikFiber
