module RandomChars where

import Prelude

import Data.Array (range)
import Data.Char (fromCharCode)
import Data.NonEmpty ((:|))
import Data.String (fromCharArray)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (elements, arrayOf)

newtype LowercaseChar = LowercaseChar Char

derive instance ordLowercaseChar :: Ord LowercaseChar
derive instance eqLowercaseChar :: Eq LowercaseChar

instance showLowercaseChar :: Show LowercaseChar where
  show lca = fromCharArray [lowercaseChar lca]

{--instance arbLowercaseChar :: Arbitrary LowercaseChar where
  arbitrary = LowercaseChar <<< fromCharCode <<< arbToLowercase <$> arbitrary
              where
                arbToLowercase = floor <<< ((+) 97.0) <<< ((*) 26.0)

instance arbLowercaseChar :: Arbitrary LowercaseChar where
  arbitrary = LowercaseChar <<< fromCharCode <<< arbToLowercaseChar <$> arbitrary
    where
      arbToLowercaseChar arb = arb `mod` 26 + 97
--}

lowercaseChars :: Array Int
lowercaseChars = range 98 122

instance arbLowercaseChar :: Arbitrary LowercaseChar where
  arbitrary = LowercaseChar <<< fromCharCode <$> (elements $ 97 :| lowercaseChars)

-- sample (mkSeed 12345) 10 (char <$> arbitrary)
-- sample (mkSeed 12345) 10 (chars <$> arrayOf arbitrary)

lowercaseChar :: LowercaseChar -> Char
lowercaseChar (LowercaseChar c) = c

char :: LowercaseChar -> LowercaseChar
char = id

chars :: Array LowercaseChar -> Array LowercaseChar
chars = id

newtype LowercaseString = LowercaseString String

strings :: LowercaseString -> LowercaseString
strings = id

instance arbLowercaseString :: Arbitrary LowercaseString where
  arbitrary =  lowerCharArrayToLowerString <$> (chars <$> arrayOf arbitrary)

-- sample (mkSeed 12345) 10 (strings <$> arbitrary)

instance showLowercaseString :: Show LowercaseString where
  show (LowercaseString s) = s

lowerCharArrayToLowerString :: Array LowercaseChar -> LowercaseString
lowerCharArrayToLowerString = LowercaseString <<< fromCharArray <<< map lowercaseChar

lowercaseCharArrayToString :: Array LowercaseChar -> String
lowercaseCharArrayToString = show <<< lowerCharArrayToLowerString
