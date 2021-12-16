{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Aeson (FromJSON (..), Result, ToJSON (..), Value (..), eitherDecodeFileStrict, object, (.:), (.=))
import Data.Aeson.Types (fromJSON, prependFailure, typeMismatch)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time (UniversalTime, defaultTimeLocale, parseTimeM)

data Bio = Bio
  { bioGender :: Text,
    bioBirthday :: Text
  }
  deriving (Show, Eq, Ord)

data Person = Person
  { personName :: Name,
    personBio :: Bio,
    personTerms :: [Terms]
  }
  deriving (Show, Eq, Ord)

data Name = Name
  { nameLast :: Text,
    nameFirst :: Text
  }
  deriving (Show, Eq, Ord)

data Terms = Terms
  { termsEnd :: Maybe UniversalTime,
    termsStart :: Maybe UniversalTime,
    termsType :: Text,
    termsParty :: Text,
    termsState :: Text
  }
  deriving (Show, Eq, Ord)

instance FromJSON Bio where
  parseJSON (Object v) = do
    bioGender <- v .: "gender"
    bioBirthday <- v .: "birthday"
    pure $ Bio {..}
  parseJSON invalid = do
    prependFailure "parsing Bio failed, " (typeMismatch "Object" invalid)

instance FromJSON Person where
  parseJSON (Object v) = do
    personName <- v .: "name"
    personBio <- v .: "bio"
    personTerms <- v .: "terms"
    pure $ Person {..}
  parseJSON invalid = do
    prependFailure "parsing Person failed, " (typeMismatch "Object" invalid)

instance FromJSON Name where
  parseJSON (Object v) = do
    nameLast <- v .: "last"
    nameFirst <- v .: "first"
    pure $ Name {..}
  parseJSON invalid = do
    prependFailure "parsing Name failed, " (typeMismatch "Object" invalid)

instance FromJSON Terms where
  parseJSON (Object v) = do
    termsEnd <- parseTimeM False defaultTimeLocale "%Y-%m-%d" <$> v .: "end"
    termsStart <- parseTimeM False defaultTimeLocale "%Y-%m-%d" <$> v .: "start"
    termsType <- v .: "type"
    termsParty <- v .: "party"
    termsState <- v .: "state"
    pure $ Terms {..}
  parseJSON invalid = do
    prependFailure "parsing Terms failed, " (typeMismatch "Object" invalid)

query :: [Person] -> [Terms]
query =
  filter (("rep" ==) . termsType)
    . concatMap personTerms
    . filter (("F" ==) . bioGender . personBio)

main :: IO ()
main = do
  contents <- eitherDecodeFileStrict "db.json"
  case contents of
    Left err -> putStrLn err
    Right people -> print (query people)
