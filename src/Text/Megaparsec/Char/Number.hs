{-# LANGUAGE FlexibleContexts #-}

module Text.Megaparsec.Char.Number where

import Control.Monad (ap)
import Data.Char (digitToInt)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

-- * floats

-- | parse a decimal unsigned floating point number containing a dot, e or E
floating :: (Floating f) => Parser f
floating = do
  n <- decimal
  fractExponent n

-- | parse a floating point number possibly containing a decimal dot, e or E
floating2 :: (Floating f) => Bool -> Parser f
floating2 = fmap (either fromInteger id) . decFloat

-- | parse a floating point number possibly starting with a decimal dot.
-- Note, that a single decimal point or a number starting with @.E@ is illegal.
floating3 :: (Floating f) => Bool -> Parser f
floating3 b = genFractAndExp 0 (fraction True) exponentFactor <|> floating2 b

-- | same as 'floating' but returns a non-negative integral wrapped by Left if
-- a fractional part and exponent is missing
decimalFloat :: (Integral i, Floating f) => Parser (Either i f)
decimalFloat = decFloat True

-- | same as 'floating' but returns a non-negative integral wrapped by Left if
-- a fractional part and exponent is missing
decFloat ::
  (Integral i, Floating f) =>
  Bool ->
  Parser (Either i f)
decFloat b = do
  n <- decimal
  option (Left n) $ Right <$> fractExp (toInteger n) b

-- | parse a hexadecimal floating point number
hexFloat :: (Integral i, Floating f) => Bool -> Parser (Either i f)
hexFloat b = do
  n <- hexnum
  option (Left n) $ Right <$> hexFractExp (toInteger n) b

octFloat :: (Integral i, Floating f) => Bool -> Parser (Either i f)
octFloat b = do
  n <- L.octal
  option (Left n) $ Right <$> octFractExp (toInteger n) b

-- | parse a binary floating point number
binFloat ::
  (Integral i, Floating f) =>
  Bool ->
  Parser (Either i f)
binFloat b = do
  n <- binary
  option (Left n) $ Right <$> binFractExp (toInteger n) b

-- | parse hexadecimal, octal or decimal integrals or 'floating'
natFloat :: (Integral i, Floating f) => Parser (Either i f)
natFloat = (char '0' >> zeroNumFloat) <|> decimalFloat

-- ** float parts

-- | parse any hexadecimal, octal, decimal or floating point number following
-- a zero
zeroNumFloat ::
  (Integral i, Floating f) =>
  Parser (Either i f)
zeroNumFloat =
  Left <$> hexOrOct
    <|> decimalFloat
    <|> Right <$> fractExponent 0
    <|> return (Left 0)

-- | parse a floating point number given the number before a dot, e or E
fractExponent :: (Floating f) => Integer -> Parser f
fractExponent i = fractExp i True

-- | parse a hex floating point number given the number before a dot, p or P
hexFractExp :: (Floating f) => Integer -> Bool -> Parser f
hexFractExp i b = genFractExp i (hexFraction b) hexExponentFactor

octFractExp :: Floating f => Integer -> Bool -> Parser f
octFractExp i b = genFractExp i (octFraction b) hexExponentFactor

-- | parse a binary floating point number given the number before a dot, p or P
binFractExp ::
  (Floating f) =>
  Integer ->
  Bool ->
  Parser f
binFractExp i b = genFractExp i (binFraction b) hexExponentFactor

-- | parse a floating point number given the number before a dot, e or E
fractExp ::
  (Floating f) =>
  Integer ->
  Bool ->
  Parser f
fractExp i b = genFractExp i (fraction b) exponentFactor

-- | parse a floating point number given the number before the fraction and
-- exponent
genFractExp ::
  (Floating f) =>
  Integer ->
  Parser f ->
  Parser (f -> f) ->
  Parser f
genFractExp i frac expo = case fromInteger i of
  f -> genFractAndExp f frac expo <|> fmap ($ f) expo

-- | parse a floating point number given the number before the fraction and
-- exponent that must follow the fraction
genFractAndExp ::
  (Floating f) =>
  f ->
  Parser f ->
  Parser (f -> f) ->
  Parser f
genFractAndExp f frac = ap (fmap (flip id . (f +)) frac) . option id

-- | parse a floating point exponent starting with e or E
exponentFactor :: (Floating f) => Parser (f -> f)
exponentFactor = oneOf "eE" >> extExponentFactor 10 <?> "exponent"

-- | parse a hexadecimal floating point starting with p (IEEE 754)
hexExponentFactor :: (Floating f) => Parser (f -> f)
hexExponentFactor = oneOf "pP" >> extExponentFactor 2 <?> "hex-exponent"

-- | parse a signed decimal and compute the exponent factor given a base.
-- For hexadecimal exponential notation (IEEE 754) the base is 2 and the
-- leading character a p.
extExponentFactor ::
  (Floating f) =>
  Int ->
  Parser (f -> f)
extExponentFactor base =
  fmap (flip (*) . exponentValue base) (ap sign (decimal <?> "exponent"))

-- | compute the factor given by the number following e or E. This
-- implementation uses @**@ rather than @^@ for more efficiency for large
-- integers.
exponentValue :: Floating f => Int -> Integer -> f
exponentValue base = (fromIntegral base **) . fromInteger

-- * fractional numbers (with just a decimal point between digits)

-- | parse a fractional number containing a decimal dot
fractional :: (Fractional f) => Parser f
fractional = do
  n <- decimal
  fractFract n True

-- | parse a fractional number possibly containing a decimal dot
fractional2 :: (Fractional f) => Bool -> Parser f
fractional2 = fmap (either fromInteger id) . decFract

-- | parse a fractional number possibly starting with a decimal dot
fractional3 :: (Fractional f) => Bool -> Parser f
fractional3 b = fractFract 0 True <|> fractional2 b

-- | a decimal fractional
decFract ::
  (Integral i, Fractional f) =>
  Bool ->
  Parser (Either i f)
decFract b = do
  n <- decimal
  option (Left n) $ Right <$> fractFract (toInteger n) b

-- | a hexadecimal fractional
hexFract ::
  (Integral i, Fractional f) =>
  Bool ->
  Parser (Either i f)
hexFract b = do
  n <- hexnum
  option (Left n) $ fmap Right $ genFractFract (toInteger n) $ hexFraction b

-- | a binary fractional
binFract ::
  (Integral i, Fractional f) =>
  Bool ->
  Parser (Either i f)
binFract b = do
  n <- binary
  option (Left n) $ fmap Right $ genFractFract (toInteger n) $ binFraction b

-- | same as 'fractional' but returns a non-negative integral wrapped by Left if
-- a fractional part is missing
decimalFract ::
  (Integral i, Fractional f) =>
  Parser (Either i f)
decimalFract = decFract True

-- | parse hexadecimal, octal or decimal integrals or 'fractional'
natFract ::
  (Integral i, Fractional f) =>
  Parser (Either i f)
natFract = (char '0' >> zeroNumFract) <|> decimalFract

-- | parse any hexadecimal, octal, decimal or fractional number following
-- a zero
zeroNumFract ::
  (Integral i, Fractional f) =>
  Parser (Either i f)
zeroNumFract =
  fmap Left hexOrOct
    <|> decimalFract
    <|> fmap Right (fractFract 0 True)
    <|> return (Left 0)

-- ** fractional parts

-- | parse a fractional number given the number before the dot
fractFract ::
  (Fractional f) =>
  Integer ->
  Bool ->
  Parser f
fractFract i = genFractFract i . fraction

-- | combine the given number before the dot with a parser for the fractional
-- part
genFractFract ::
  (Fractional f) =>
  Integer ->
  Parser f ->
  Parser f
genFractFract i = fmap (fromInteger i +)

-- | parse a dot followed by decimal digits as fractional part
fraction :: (Fractional f) => Bool -> Parser f
fraction b = baseFraction b 10 digitChar

-- | parse a dot followed by hexadecimal digits as fractional part
hexFraction :: (Fractional f) => Bool -> Parser f
hexFraction b = baseFraction b 16 hexDigitChar

-- | parse a dot followed by binary digits as fractional part
binFraction :: (Fractional f) => Bool -> Parser f
binFraction b = baseFraction b 2 binDigit

octFraction :: (Fractional f) => Bool -> Parser f
octFraction b = baseFraction b 8 octDigitChar

-- | parse a dot followed by base dependent digits as fractional part
baseFraction ::
  (Fractional f) =>
  Bool ->
  Int ->
  Parser Char ->
  Parser f
baseFraction requireDigit base baseDigit =
  char '.'
    >> fmap
      (fractionValue base)
      ((if requireDigit then some else many) baseDigit <?> "fraction")
    <?> "fraction"

-- | compute the fraction given by a sequence of digits following the dot.
-- Only one division is performed and trailing zeros are ignored.
fractionValue :: Fractional f => Int -> String -> f
fractionValue base =
  uncurry (/)
    . foldl
      ( \(s, p) d ->
          (p * fromIntegral (digitToInt d) + s, p * fromIntegral base)
      )
      (0, 1)
    . dropWhile (== '0')
    . reverse

-- * integers and naturals

-- | parse an optional 'sign' immediately followed by a 'nat'. Note, that in
-- Daan Leijen's code the sign was wrapped as lexeme in order to skip comments
-- and spaces in between.
int :: (Integral i) => Parser i
int = ap sign nat

-- | parse an optional plus or minus sign, returning 'negate' or 'id'
sign :: (Num a) => Parser (a -> a)
sign = (char '-' >> return negate) <|> (optional (char '+') >> return id)

-- | parse plain non-negative decimal numbers given by a non-empty sequence
-- of digits
decimal :: (Integral i) => Parser i
decimal = number 10 digitChar

-- | parse 0 or 1
binDigit :: Parser Char
binDigit = oneOf "01"

-- | parse a binary number
binary :: (Integral i) => Parser i
binary = number 2 binDigit

-- | parse non-negative hexadecimal, octal or decimal numbers
nat :: (Integral i) => Parser i
nat = zeroNumber <|> decimal

-- ** natural parts

-- | parse a 'nat' syntactically starting with a zero
zeroNumber :: (Integral i) => Parser i
zeroNumber =
  char '0' >> (hexOrOct <|> decimal <|> return 0) <?> ""

-- | hexadecimal or octal number
hexOrOct :: (Integral i) => Parser i
hexOrOct = hexadecimal <|> octal

-- | parse a hexadecimal number preceded by an x or X character
hexadecimal :: (Integral i) => Parser i
hexadecimal = oneOf "xX" >> hexnum

-- | parse a hexadecimal number
hexnum :: (Integral i) => Parser i
hexnum = number 16 hexDigitChar

-- | parse an octal number preceded by an o or O character
octal :: (Integral i) => Parser i
octal = oneOf "oO" >> number 8 octDigitChar

-- | parse a non-negative number given a base and a parser for the digits
number ::
  (Integral i) =>
  Int ->
  Parser Char ->
  Parser i
number base baseDigit = do
  n <- fmap (numberValue base) (some baseDigit)
  seq n (return n)

-- | compute the value from a string of digits using a base
numberValue :: Integral i => Int -> String -> i
numberValue base =
  foldl (\x -> ((fromIntegral base * x) +) . fromIntegral . digitToInt) 0
