module Text.XML.Decode.Time where

import Data.Time (Day, UTCTime)

newtype IsoUTCTime = IsoUTCTime { toUtcT :: UTCTime }
newtype IsoDay     = IsoDay     { toDay :: Day }

isoUTCTimeFormat,isoDayFormat :: String
isoUTCTimeFormat = "%FT%T%z"
isoDayFormat     = "%F"
