{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict)
import Data.List (group, sort)
import Data.Text (Text)
import Data.Time.Calendar (Day, toGregorian)
import Data.Tuple.Extra (fst3)
import GHC.Generics (Generic)
import Generics.Generic.Aeson (Settings, defaultSettings, gparseJson, gparseJsonWithSettings, stripPrefix)

data Bio = Bio
  { gender :: Text,
    birthday :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Bio where parseJSON = gparseJson

data Person = Person
  { name :: Name,
    bio :: Bio,
    terms :: [Terms]
  }
  deriving (Generic, Show, Eq)

instance FromJSON Person where parseJSON = gparseJson

data Name = Name
  { last :: Text,
    first :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Name where parseJSON = gparseJson

data Terms = Terms
  { termsEnd :: Day,
    termsStart :: Day,
    termsType :: Text,
    termsParty :: Text,
    termsState :: Text
  }
  deriving (Generic, Show, Eq)

stripSettings :: Settings
stripSettings = defaultSettings {stripPrefix = Just "terms"}

instance FromJSON Terms where parseJSON = gparseJsonWithSettings stripSettings

getYear :: Day -> Integer
getYear = fst3 . toGregorian

query :: [Person] -> [(Integer, Int)]
query =
  map ((,) <$> head <*> length)
    . group
    . sort
    . concatMap
      ( \term ->
          [ (getYear (termsStart term))
            .. (getYear (termsEnd term) - 1)
          ]
      )
    . filter (("rep" ==) . termsType)
    . concatMap terms
    . filter (("F" ==) . gender . bio)

main :: IO ()
main = do
  contents <- eitherDecodeFileStrict "db.json"
  case contents of
    Left err -> putStrLn err
    Right people ->
      mapM_
        (\(year, num) -> putStrLn $ show year ++ ": " ++ (take num $ repeat '#'))
        (query people)
