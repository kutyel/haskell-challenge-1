{-# language DuplicateRecordFields #-}
{-# language RecordWildCards #-}
{-# language OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Data.Time (UniversalTime, defaultTimeLocale, parseTimeM)
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), Result, (.:), (.=), object, eitherDecodeFileStrict)
import Data.Aeson.Types (prependFailure, typeMismatch, fromJSON)
import Data.Text (Text)

data Bio = Bio
  { bioGender :: Text
  , bioBirthday :: Text
  } deriving (Show, Eq, Ord)

data Person = Person
  { personName :: Name
  , personBio :: Bio
  , persionTerms :: [Terms]
  } deriving (Show, Eq, Ord)

data Name = Name
  { nameLast :: Text
  , nameFirst :: Text
  } deriving (Show, Eq, Ord)

{- FIXME: use this to parse the dates...
> parseTimeM False defaultTimeLocale "%Y-%m-%d" "2016-10-20" :: Maybe UniversalTime
> Just 2016-10-20 00:00:00
-}

data Terms = Terms
  { termsEnd :: Text -- Maybe UniversalTime
  , termsType :: Text
  , termsParty :: Text
  , termsState :: Text
  , termsStart :: Text -- Maybe UniversalTime
  } deriving (Show, Eq, Ord)

instance FromJSON Bio where
  parseJSON (Object v) = do
    bioGender <- v .: "gender"
    bioBirthday <- v .: "birthday"
    pure $ Bio{..}
  parseJSON invalid = do
    prependFailure "parsing Bio failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Person where
  parseJSON (Object v) = do
    personName <- v .: "name"
    personBio <- v .: "bio"
    persionTerms <- v .: "terms"
    pure $ Person {..}
  parseJSON invalid = do
    prependFailure "parsing Person failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Name where
  parseJSON (Object v) = do
    nameLast <- v .: "last"
    nameFirst <- v .: "first"
    pure $ Name {..}
  parseJSON invalid = do
    prependFailure "parsing Name failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Terms where
  parseJSON (Object v) = do
    termsEnd <- v .: "end"
    termsType <- v .: "type"
    termsParty <- v .: "party"
    termsState <- v .: "state"
    termsStart <- v .: "start"
    pure $ Terms {..}
  parseJSON invalid = do
    prependFailure "parsing Terms failed, "
      (typeMismatch "Object" invalid)

query :: [Person] -> [Person]
query = filter (("F" ==) . bioGender. personBio)

main :: IO ()
main = do
    contents <- eitherDecodeFileStrict "db.json"
    case contents of
        Left err -> putStrLn err
        Right people -> print (query people)
