module Parser where

import Prelude

import Control.Lazy (class Lazy, defer)
import Control.Plus (class Alt, (<|>))
import Data.Array (cons, (:))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..), note)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, singleton, uncons)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

type ParserState a = Tuple String a
class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

type ParseFunction e a = ParserError e => String -> Either e (ParserState a)
newtype Parser e a = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map :: ∀ a b. (a -> b) -> Parser e a -> Parser e b
  map f p = Parser \s -> (map f) <$> (parse p s)

flatten :: ∀ a b. (Either a (Either a b)) -> Either a b
flatten (Right (Right x)) = Right x
flatten (Right (Left x)) = Left x
flatten (Left x) = Left x

instance applyParser :: Apply (Parser e) where
  apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
  apply p1 p2 = Parser \s -> do
    Tuple s' f <- parse p1 s
    Tuple s'' x <- parse p2 s'
    pure $ Tuple s'' (f x)

  -- apply p1 p2 = Parser \s -> case parse p1 s of
  --   (Left x) -> Left x
  --   (Right (Tuple remains mapperFunction)) -> case parse p2 remains of
  --     (Left x) -> Left x
  --     (Right (Tuple remains' value)) -> Right $ Tuple remains' (mapperFunction value)

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser $ \s -> pure (Tuple s x)

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

data PError = EOF | InvalidChar String

derive instance genericPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

instance parserErrorPError :: ParserError PError where
  eof = EOF
  invalidChar = InvalidChar

instance bindParser :: Bind (Parser e) where
  bind p f = Parser $ \s -> do
    Tuple s' x <- parse p s
    parse (f x) s'

instance altParser :: Alt (Parser e) where
  alt fx fy = Parser $ \s -> case parse fx s of
    Right x -> Right x
    Left _ -> parse fy s

instance lazyParser :: Lazy (Parser e a) where
  defer :: (Unit -> Parser e a) -> Parser e a
  defer f = Parser \s -> parse (f unit) s

-- Parsers
fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ \_ -> Left e

char :: ∀ e. Parser e Char
char = Parser $ \s -> case uncons s of
  Nothing -> Left eof
  Just { head, tail } -> Right $ Tuple tail head

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = do
  c <- char
  c' <- char
  pure $ Tuple c c'

threeCharsA :: ∀ e. Parser e String
threeCharsA = go <$> char <*> char <*> char
  where
    go a b c = fromCharArray [a, b, c]

threeCharsB :: ∀ e. Parser e String
threeCharsB = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [c1, c2, c3]

count :: ∀ e a f. Unfoldable f => Traversable f => Int -> Parser e a -> Parser e (f a)
count num p
  | num <= 0 = pure none
  | otherwise = sequence $ replicate num p

countA :: ∀ e a.Int -> Parser e a -> Parser e (Array a)
countA = count

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' num p = fromCharArray <$> count num p

atMostG :: ∀ e a f. Unfoldable f => Traversable f => (a -> f a -> f a) -> Int -> Parser e a -> Parser e (f a)
atMostG f n p
  | n <= 0 = pure none
  | otherwise = optional none $ p >>= \c -> (f c) <$> (atMostG f (n - 1) p)

atMostG' :: ∀ e a f. Monoid (f a) => Applicative f => Int -> Parser e a -> Parser e (f a)
atMostG' n p
  | n <= 0 = pure mempty
  | otherwise = optional mempty $ p >>= \c -> (append (pure c)) <$> (atMostG' (n - 1) p)

atMost :: ∀ e a. Int -> Parser e a -> Parser e (Array a)
atMost = atMostG'

atMost' :: ∀ e. Int -> Parser e Char -> Parser e String
atMost' n p = fromCharArray <$> atMostG cons n p

range :: ∀ e a f. Monoid (f a) => Applicative f => Unfoldable f => Traversable f => Int -> Int -> Parser e a -> Parser e (f a)
range min max p
  | min <= 0
    || max <= 0
    || max < min = pure none
  | otherwise = do
    firstPart <- count min p
    secondPart <- atMostG' (max - min) p
    pure $ firstPart <> secondPart

rangeA :: ∀ e a. Int -> Int -> Parser e a -> Parser e (Array a)
rangeA = range

range' :: ∀ e. Int -> Int -> Parser e Char -> Parser e String
range' min max p = fromCharArray <$> rangeA min max p

optional :: ∀ e a. a -> Parser e a -> Parser e a
optional default p = p <|> pure default

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = do
  c <- char
  if (pred c) then pure c else fail $ invalidChar expected

digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = digit <|> letter <|> fail (invalidChar "alphaNum")

newtype Year = Year Int
derive newtype instance showYear :: Show Year
newtype Month = Month Int
derive newtype instance showMonth :: Show Month
newtype Day = Day Int
derive newtype instance showDay :: Show Day

data DateFormat = YearFirst | MonthFirst
derive instance genericFormat :: Generic DateFormat _
instance showDateFormat :: Show DateFormat where
  show = genericShow

type DateParts = { year :: Year , month :: Month , day :: Day , format :: DateFormat }

toInt :: String -> Either String Int
toInt = (note "") <<< fromString

digitsToNum :: String -> Int
digitsToNum = (fromMaybe 0) <<< fromString

constChar :: ∀ e. ParserError e => Char -> Parser e Unit
constChar c = void $ satisfy (singleton c) (_ == c)

yearFirst :: ∀ e. ParserError e => Parser e DateParts
-- yearFirst = do
  -- y <- count' 4 digit
  -- void $ satisfy "date dash" (_ == '-')
  -- m <- range' 1 2 digit
  -- void $ satisfy "date dash" (_ == '-')
  -- d <- range' 1 2 digit
  -- case traverse fromString [y, m, d] of
  --   Just [yInt, mInt, dInt] -> pure $ { year: (Year yInt), month: (Month mInt), day: (Day dInt), format: YearFirst}
  --   _ -> fail $ invalidChar "toInt failed"
yearFirst = do
  y <- Year <<< digitsToNum <$> count' 4 digit
  constChar '-'
  m <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '-'
  d <- Day <<< digitsToNum <$> range' 1 2 digit
  pure $ { year: y, month: m, day: d, format: YearFirst}

monthFirst :: ∀ e. ParserError e => Parser e DateParts
monthFirst = do
  m <- Month <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  d <- Day <<< digitsToNum <$> range' 1 2 digit
  constChar '/'
  y <- Year <<< digitsToNum <$> count' 4 digit
  pure $ { year: y, month: m, day: d, format: MonthFirst}

date :: ∀ e. ParserError e => Parser e DateParts
date = monthFirst <|> yearFirst

some :: ∀ a f m
  . Unfoldable f => Alt m => Applicative m => Lazy (m (f a))
  => (a -> f a -> f a)
  -> m a
  -> m (NonEmpty f a)
some cons p = (:|) <$> p <*> defer \_ -> many cons p

many :: ∀ a f m
  . Unfoldable f => Alt m => Applicative m => Lazy (m (f a))
  => (a -> f a -> f a)
  -> m a
  -> m (f a)
many cons p = fromNonEmpty cons <$> some cons p <|> pure none

some' :: ∀ e. Parser e Char -> Parser e String
some' p = fromCharArray <<< fromNonEmpty (:) <$> some (:) p

many' :: ∀ e. Parser e Char -> Parser e String
many' p = fromCharArray <$> many (:) p

digits :: ∀ e. ParserError e => Parser e String
digits = some' digit

letterOrSpace :: ∀ e. ParserError e => Parser e Char
letterOrSpace = letter <|> satisfy "space" (_ == ' ')

ugly :: ∀ e. ParserError e => Parser e (Array String)
ugly = do
  x <- range' 1 4 digit
  void $ constChar ','
  void $ constChar ' '
  x1 <- some' letterOrSpace
  x2 <- many' digit
  pure $ [x, x1, x2]

test :: Effect Unit
test = do
  log $ show $ parse' ugly "123, asdf asdf end909009"
  log $ show $ parse' ugly "12, asdf end"
  log $ show $ parse' ugly "12, "