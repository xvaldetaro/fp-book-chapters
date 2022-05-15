module Ch7b where

import Prelude
import Data.Array (length)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)

newtype CSV
  = CSV String

derive instance newTypeCSV :: Newtype CSV _

derive newtype instance showCSV :: Show CSV

derive newtype instance eqCSV :: Eq CSV

class ToCSV a where
  toCSV :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

newtype FullName
  = FullName String

newtype Age
  = Age Int

data Occupation
  = Doctor
  | Dentist
  | Lawyer
  | Unemployed

data Person
  = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }

derive instance newTypeFullName :: Newtype FullName _

instance showFullName :: Show FullName where
  show (FullName x) = x

derive instance newTypeAge :: Newtype Age _

derive newtype instance showAge :: Show Age

derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show x = genericShow x

derive newtype instance eqFullName :: Eq FullName

derive instance eqOccupation :: Eq Occupation

derive newtype instance eqAge :: Eq Age

derive instance eqPerson :: Eq Person

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ (show name) <> "," <> (show age) <> "," <> (show occupation)

fromStringOccupation :: String -> Maybe Occupation
fromStringOccupation "Doctor" = Just Doctor

fromStringOccupation "Dentist" = Just Dentist

fromStringOccupation "Lawyer" = Just Lawyer

fromStringOccupation "Unemployed" = Just Unemployed

fromStringOccupation _ = Nothing

stringOrDefault :: String -> Maybe String -> String
stringOrDefault default maybeString = case maybeString of
  Nothing -> default
  Just x -> x

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV string) = case split (Pattern ",") string of
    [ nameString, ageString, occString ] -> case fromString ageString of
      Nothing -> Nothing
      Just (age :: Int) -> case fromStringOccupation occString of
        Nothing -> Nothing
        Just occupation -> Just (Person { name: (FullName nameString), age: (Age age), occupation: occupation })
    _ -> Nothing

test :: Effect Unit
test = do
  log $ show
    $ case (fromCSV (CSV "Sue Smith,23,Doctor") :: Maybe Person) of
        Nothing -> false
        Just person' -> person' == person
          where
          person = Person { name: (FullName "Sue Smith"), age: (Age 23), occupation: Doctor }

-- log $ show $ toCSV $ fromCSV "Sue Smith,23,Doctor"
