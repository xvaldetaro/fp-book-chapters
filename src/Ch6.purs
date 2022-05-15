module Ch6 where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)

type Address =
  { street1 :: String , street2 :: String , city :: String
  , state :: String
  , zip :: String
  }

class HasAddress a where
  getAddress :: a -> Address

newtype Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

-- Deriving newtype chapter
newtype Ceo = Ceo Person

derive instance newTypeCeo :: Newtype Ceo _
derive newtype instance hasAddressCeo :: HasAddress Ceo

-- test :: Effect Unit
-- test = do
--   log $ show $ getAddress $ Ceo $ Person { name: "asdf", age: 1, address: {street1: "", street2: "", city:"", state: "", zip: ""} }


test :: Effect Unit
test = do
  log $ show $ getAddress $ Ceo $ Person { name: "asdf", age: 1, address: {street1: "", street2: "", city:"", state: "", zip: ""} }