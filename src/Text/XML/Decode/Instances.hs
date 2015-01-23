{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.XML.Decode.Instances where

import           BasePrelude
import           Prelude                      ()

import           Data.Text                    (Text)

import           Text.XML.Decode.DecodeCursor
import           Text.XML.Decode.Parsers
import           Text.XML.Decode.Time

instance DecodeCursor Text where decode = fmap fold . cursorContents
instance DecodeCursor Int where decode = parseCursor parseInt
instance DecodeCursor Integer where decode = parseCursor parseInteger
instance DecodeCursor Double where decode = parseCursor parseDouble
instance DecodeCursor Bool where decode = parseCursor parseBool
instance DecodeCursor IsoUTCTime where decode = parseCursor parseIsoUtcTime
instance DecodeCursor IsoDay where decode = parseCursor parseIsoDay
