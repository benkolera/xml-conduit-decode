{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Text.XML.Decode.Parsers
Description :  Helper functions from Text -> Either Text a
Copyright   :  (c) Ben Kolera
License     :  MIT

Maintainer  :  Ben Kolera
Stability   :  experimental

-}
module Text.XML.Decode.Parsers
 ( parseText
 , parseMaybe
 , parseInt
 , parseInteger
 , parseDouble
 , parseBool
 , parseXmlTime
 , parseIsoUtcTime
 , parseIsoDay
 ) where

import           Data.Foldable        (fold)
import           Data.Text            (Text, toLower, unpack)
import           Data.Time            (ParseTime, parseTimeM, defaultTimeLocale)
import           Text.Read            (readMaybe)

import           Text.Read            (readMaybe)
import           Text.XML.Decode.Time

parseText :: Text -> Either Text Text
parseText = Right

parseMaybe :: Text -> (Text -> Maybe a) -> Text -> Either Text a
parseMaybe desc f t = maybe (Left (fold ["'",t, "' was not ",desc])) Right (f t)

parseInt :: Text -> Either Text Int
parseInt = parseMaybe "an int" (readMaybe . unpack)

parseInteger :: Text -> Either Text Integer
parseInteger = parseMaybe "an integer" (readMaybe . unpack)

parseDouble :: Text -> Either Text Double
parseDouble = parseMaybe "a double" (readMaybe . unpack)

parseBool :: Text -> Either Text Bool
parseBool = parseMaybe "a bool" parseBool' . toLower
  where
    parseBool' "true"  = Just True
    parseBool' "false" = Just False
    parseBool' _       = Nothing


parseXmlTime :: ParseTime a => Text -> String -> Text -> Either Text a
parseXmlTime desc format =
  parseMaybe desc (parseTimeM True defaultTimeLocale format . unpack)

parseIsoUtcTime :: Text -> Either Text IsoUTCTime
parseIsoUtcTime =
  fmap IsoUTCTime . parseXmlTime "UTCTime" isoUTCTimeFormat

parseIsoDay :: Text -> Either Text IsoDay
parseIsoDay = fmap IsoDay . parseXmlTime "UTCDay" isoDayFormat
