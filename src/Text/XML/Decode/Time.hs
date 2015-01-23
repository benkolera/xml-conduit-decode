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
