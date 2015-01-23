{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Decode.Parsers where

import BasePrelude hiding (toLower)
import Prelude     ()

import Data.Text     (Text, toLower, unpack)
import Data.Time     (ParseTime, parseTime)
import System.Locale (defaultTimeLocale)

import Text.XML.Decode.Time

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
  parseMaybe desc (parseTime defaultTimeLocale format . unpack)

parseIsoUtcTime :: Text -> Either Text IsoUTCTime
parseIsoUtcTime =
  fmap IsoUTCTime . parseXmlTime "UTCTime" isoUTCTimeFormat

parseIsoDay :: Text -> Either Text IsoDay
parseIsoDay = fmap IsoDay . parseXmlTime "UTCDay" isoDayFormat
