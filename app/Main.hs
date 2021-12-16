{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative (liftA2)
import Data.Aeson (FromJSON (..), Result, ToJSON (..), Value (..), eitherDecodeFileStrict, object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.List (group, sort)
import Data.Text (Text)
import Data.Time.Calendar (Day, toGregorian)

data Bio = Bio
  { bioGender :: Text,
    bioBirthday :: Text
  }
  deriving (Show, Eq)

data Person = Person
  { personName :: Name,
    personBio :: Bio,
    personTerms :: [Terms]
  }
  deriving (Show, Eq)

data Name = Name
  { nameLast :: Text,
    nameFirst :: Text
  }
  deriving (Show, Eq)

data Terms = Terms
  { termsEnd :: Day,
    termsStart :: Day,
    termsType :: Text,
    termsParty :: Text,
    termsState :: Text
  }
  deriving (Show, Eq)

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
    termsEnd <- v .: "end"
    termsStart <- v .: "start"
    termsType <- v .: "type"
    termsParty <- v .: "party"
    termsState <- v .: "state"
    pure $ Terms {..}
  parseJSON invalid = do
    prependFailure "parsing Terms failed, " (typeMismatch "Object" invalid)

getYear :: Day -> Integer
getYear day =
  let (year, _, _) = toGregorian day
   in year

query :: [Person] -> [(Integer, Int)]
query =
  map (liftA2 (,) head length)
    . group
    . sort
    . concatMap
      ( \term ->
          [ (getYear (termsStart term))
            .. (getYear (termsEnd term) - 1)
          ]
      )
    . filter (("rep" ==) . termsType)
    . concatMap personTerms
    . filter (("F" ==) . bioGender . personBio)

main :: IO ()
main = do
  contents <- eitherDecodeFileStrict "db.json"
  case contents of
    Left err -> putStrLn err
    Right people ->
      mapM_
        (\(year, num) -> putStrLn $ show year ++ ": " ++ (take num $ repeat '#'))
        (query people)
