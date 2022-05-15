module Ch25a where

import Prelude

import Affjax as Ajax
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Generic (class Decode, class Encode, F, decodeJSON, defaultOptions, encodeJSON, genericDecode, genericEncode)
import Type.Proxy (Proxy)

newtype Centimeters = Centimeters Number
derive instance genericCentimeters :: Generic Centimeters _
derive newtype instance decodeCentimeters :: Decode Centimeters
derive newtype instance encodeCentimeters :: Encode Centimeters
instance showCentimeters :: Show Centimeters where
  show = genericShow

newtype Kilograms = Kilograms Number
derive instance genericKilograms :: Generic Kilograms _
derive newtype instance decodeKilograms :: Decode Kilograms
derive newtype instance encodeKilograms :: Encode Kilograms
instance showKilograms :: Show Kilograms where
  show = genericShow

newtype Years = Years Int
derive instance genericYears :: Generic Years _
derive newtype instance decodeYears :: Decode Years
derive newtype instance encodeYears :: Encode Years
instance showYears :: Show Years where
  show = genericShow

type Personal = { height :: Centimeters , weight :: Kilograms , age :: Years }

newtype GPA = GPA Number
derive instance genericGPA :: Generic GPA _
derive newtype instance decodeGPA :: Decode GPA
derive newtype instance encodeGPA :: Encode GPA
instance showGPA :: Show GPA where
  show = genericShow

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int
derive instance genericGrade :: Generic Grade _
instance decodeGrade :: Decode Grade where
  decode = genericDecode defaultOptions
instance encodeGrade :: Encode Grade where
  encode = genericEncode defaultOptions
instance showGrade :: Show Grade where
  show = genericShow

type Student =
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }

data TeachingStatus = Student | Probationary | NonTenured | Tenured
derive instance genericTeachingStatus :: Generic TeachingStatus _
instance decodeTeachingStatus :: Decode TeachingStatus where
  decode = genericDecode defaultOptions
instance encodeTeachingStatus :: Encode TeachingStatus where
  encode = genericEncode defaultOptions
instance showTeachingStatus :: Show TeachingStatus where
  show = genericShow

type Teacher =
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

processAjaxResult ::
  ∀ m a. MonadEffect m
  => Show a
  => Decode a
  => Proxy a
  -> Either Ajax.Error (Ajax.Response String)
  -> m Unit
processAjaxResult _ = case _ of
  Left err -> log $ "ERROR: " <> Ajax.printError err
  Right { body } -> case runExcept (decodeJSON body :: F a) of
    Left err -> log $ "ERROR: " <> show err <> " (" <> body <> ")"
    Right v -> log $ show v

-- TEST code
teacher :: Teacher
teacher =
  { grades: [ Preschool, Kindergarten, Grade 1 ]
  , numberOfStudents: 23
  , personal: {
    height: Centimeters 162.56
    , weight: Kilograms 63.5
    , age: Years 31
  }
  , status: NonTenured
  }

test :: Effect Unit
test = launchAff_ do
  result <- Ajax.post ResponseFormat.string "http://localhost:3000/" $ Just $ RequestBody.string $ encodeJSON teacher
  log $ case result of
    Left err -> "Error: " <> Ajax.printError err
    Right v -> show v
