module Ch17
( Age (..)
, Either (..)
, FamilyAges (..)
, FamilyAgesRow
, Validation (..)
, createFamilyAges
, test
) where

import Prelude

import Data.Array ((:))
import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prim.TypeError (class Fail)

data Maybe a
  = Nothing
  | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply (Just f) (Just x) = Just (f x)
  apply _ _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure x = Just x

derive instance eqMaybe :: Eq a => Eq (Maybe a)

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance functorEither :: Functor (Either a)

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right x) = Right $ g x

instance applyEither :: Apply (Either a) where
  apply (Right f) x = f <$> x
  apply (Left x) _ = Left x

instance applicativeEither :: Applicative (Either a) where
  pure x = Right x

newtype Validation err result = Validation (Either err result)

derive instance newtypeValidation :: Newtype (Validation a b) _
derive newtype instance functorValidation :: Functor (Validation a)
derive newtype instance bifunctorValidation :: Bifunctor Validation
derive newtype instance eqValidation :: (Eq a, Eq b) => Eq (Validation a b)
derive newtype instance ordValidation :: (Ord a, Ord b) => Ord (Validation a b)
derive instance genericValidation :: Generic (Validation a b) _
instance showValidation :: (Show a, Show b) => Show (Validation a b) where
  show = genericShow

instance applyValidation :: (Semigroup err) => Apply (Validation err) where
  -- apply (Validation (Right f)) x = f <$> x
  apply (Validation (Left x)) (Validation (Left y)) = Validation $ Left $ x <> y
  apply (Validation x) (Validation y) = Validation $ x <*> y
  -- apply (Validation (Left x)) _ = Validation $ Left x

instance applicativeValidation :: (Semigroup err) => Applicative (Validation err) where
  pure = Validation <<< Right

newtype Age = Age Int
instance showAge :: Show Age where
  show (Age x) = show x

newtype FullName = FullName String
instance showFullName :: Show FullName where
  show (FullName x) = show x

type FamilyAgesRow r = (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)
type FamilyNamesRow r = (fatherName :: FullName, motherName :: FullName, childName :: FullName | r)

newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }
derive instance genericFamily :: Generic Family _
instance showFamily :: Show Family where
  show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }
derive instance genericFamilyAges :: Generic FamilyAges _
instance showFamilyAges :: Show FamilyAges where
  show = genericShow

newtype UpperAge = UpperAge Int
newtype LowerAge = LowerAge Int

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge lower) (UpperAge upper) (Age age) attribution
  | age < lower = Validation $ Left $ [attribution <> " age is lower"]
  | age > upper = Validation $ Left $ [attribution <> " age is higher"]
  | otherwise = Validation $ Right $ Age age

createFamilyAges :: { | FamilyAgesRow() } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } =
  FamilyAges <$> ({ fatherAge: _, motherAge: _, childAge: _}
    <$> (validateAgeWithBounds fatherAge "father")
    <*> (validateAgeWithBounds motherAge "mother")
    <*> (validateAgeWithBounds childAge "child"))
  where
    validateAgeWithBounds = validateAge (LowerAge 10) (UpperAge 50)



test :: Effect Unit
test = do
  log $ show $ createFamilyAges { fatherAge: (Age 60), motherAge: (Age 60), childAge: (Age 9) }