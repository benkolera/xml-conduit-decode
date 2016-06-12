{- |
Module      :  Text.XML.Decode.Time
Description :  Newtypes for Data.Time parsing
Copyright   :  (c) Ben Kolera
License     :  MIT

Maintainer  :  Ben Kolera
Stability   :  experimental

Some newtypes that denote Days and UTCTimes to be parsed in ISO8601 format

-}
module Text.XML.Decode.Time
  ( IsoUTCTime(..)
  , IsoDay(..)
  , isoUTCTimeFormat
  , isoDayFormat
  ) where

import           Data.Time (Day, UTCTime)

newtype IsoUTCTime = IsoUTCTime { toUtcT :: UTCTime }
newtype IsoDay     = IsoDay     { toDay :: Day }

isoUTCTimeFormat,isoDayFormat :: String
isoUTCTimeFormat = "%FT%TZ"
isoDayFormat     = "%F"
