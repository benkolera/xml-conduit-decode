{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{- |
Module      :  Text.XML.Decode.DecodeCursor
Description :  Decode combinators to parse the contents of a HCursor
Copyright   :  (c) Ben Kolera
License     :  MIT

Maintainer  :  Ben Kolera
Stability   :  experimental

These functions allow you to pull haskell values out of a HCursor.

The idea is that you use the HCursor combinators to get where you need to in
the XML, and something in this file allows you to parse the XML into values.

Because of how the underlaying Text.XML.Cursors work, these functions have the
following oddities:

  * The cursor could actually be at 0 or many elements, so the 'decodeMay',
    'decodeSingle','decodeMany','decodeNel' functions allow you to express how many
    elements you want to decode. 'DecodeCursor' powers these functions.
  * The cursor has no concept of being at an attribute, so we need the hack
    of 'decodeAttrMay','decodeAttr' to pull out a named attribute from the positions
    that the cursor is in.
  * Choices are weird and our only spot where we have multiple different elements
    at a Cursor and need to disambiguate them by the element name. 'DecodeChoice'
    pairs element names with a decoder that will decode the element into a sum
    type constructor. See the 'decodeChoice' function and the 'choice' constructor.
-}
module Text.XML.Decode.DecodeCursor
  ( DecodeResult
  , DecodeCursor
  , decode
  , decodeMay
  , decodeSingle
  , decodeDefault
  , decodeMany
  , decodeNel
  , decodeDocument
  , decodeAttr
  , ChoiceDecoder
  , choice
  , decodeChoice
  , decodeAttrMay
  , parseCursor
  , cursorContents
  ) where

import           Control.Lens
import           Data.Bifunctor          (first)
import           Data.Foldable           (find, fold)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NEL
import           Data.Maybe              (fromMaybe)
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Text.XML                (Document)
import qualified Text.XML.Cursor         as C

import           Text.XML.Decode.Parsers
import           Text.XML.Decode.Time
import           Text.XML.Decode.HCursor

-- | Gives you a the result of the decoding or a text description of the error
--   and a history of cursor movements prior to the error.
type DecodeResult a = Either (Text,CursorHistory) a

nelCursor :: HCursor -> DecodeResult (NonEmpty HCursor)
nelCursor = foldCursor f w
  where
    f      = Left . ("Tried to convert failed cursor to NEL",)
    w cs h = Right $ fmap (\ c -> HCursor [c] h) cs

cursorAttribute :: Text -> HCursor -> CursorResult Text
cursorAttribute  n = foldCursor f w
  where
    f _      = Right $ "" :| []
    w cs   _ = Right $ fmap (fold . C.laxAttribute n) cs

-- | Grabs the concatenated text of all elements of a cursor.
cursorContents
  :: HCursor            -- ^ The cursor to extract all text nodes from
  -> CursorResult Text  -- ^ Left if the cursor was failed, else all text nodes concatenated into a single text
cursorContents = foldCursor f w
  where
    f h      = Left ("Tried to decode a failed cursor",h)
    w cs   _ = Right . fmap (T.concat . (C.$// C.content)) $ cs

-- | DecodeCursor is the crux of pulling things out of XML in a reusable way.
--   You'll want to implement it for any data type that you wish to construct
--   out of a XML element.
class DecodeCursor a where
  -- | You wont call this outside of here. Call 'decodeSingle' instead
  decode :: HCursor -> DecodeResult a

-- | Decodes zero or one results from the cursor.
decodeMay :: DecodeCursor a => HCursor -> DecodeResult (Maybe a)
decodeMay = foldCursor (const (Right Nothing)) w
  where
    w cs h = Just <$> decode (HCursor [NEL.head cs] h)

-- | Decodes a single result from the Cursor. Errors if the cursor is empty.
decodeSingle :: DecodeCursor a => HCursor -> DecodeResult a
decodeSingle = (decode . NEL.head =<<) . nelCursor

-- | Decodes a result from the cursor, or provides the default if the cursor is empty.
decodeDefault :: DecodeCursor a => a -> HCursor -> DecodeResult a
decodeDefault a = fmap (fromMaybe a) . decodeMay

-- | Decodes 0 or more results from the cursor.
decodeMany :: DecodeCursor a => HCursor -> DecodeResult [a]
decodeMany = foldCursor (const $ return []) w
  where
    w cs h = NEL.toList <$> traverse (\ c -> decode $ HCursor [c] h) cs

-- | Decodes 1 or more results. Fails if the cursor is empty.
decodeNel :: DecodeCursor a => HCursor -> DecodeResult (NonEmpty a)
decodeNel hc = nelCursor hc >>= traverse decode

-- | Takes an entire document, an a cursor shift to shift from the top of the document
--   to where you need to start parsing.
decodeDocument :: DecodeCursor a
  => (HCursor -> HCursor)
  -> Document
  -> Either (Text,CursorHistory,Document) a
decodeDocument s d = first (\ (t,h) -> (t,h,d)) . decode . s . fromDocument $ d

-- | Grab an attribute from the element focused by the cursor
decodeAttr
  :: Text                    -- ^ The attribute name
  -> (Text -> Either Text a) -- ^ A parser from Text to either an error or the result
  -> HCursor                 -- ^ The cursor to parse from
  -> DecodeResult a
decodeAttr n f hc =
  (first ((,hc ^. history) . errorMessage) . f . NEL.head =<<)
  . cursorAttribute n
  $ hc
  where
    errorMessage pe = T.concat ["Failed to get attr (",n,"): ",pe]

-- | Optionally grab an attribute from the cursor.
decodeAttrMay :: Text -> (Text -> Either Text a) -> HCursor -> DecodeResult (Maybe a)
decodeAttrMay n f = decodeAttr n parse
  where
    parse "" = Right Nothing
    parse t  = Just <$> f t

-- | Describes how to navigate to the choice element and then decode it.
data ChoiceDecoder a = ChoiceDecoder
  { _choiceDecoderShift  :: Shift
  , _choiceDecoderDecode :: HCursor -> DecodeResult a
  }
makeLenses ''ChoiceDecoder

-- | Constructs a ChoiceDecoder
choice
  :: Shift -- ^ Given a shift to the element (e.g. laxElement "foo")
  -> (HCursor -> DecodeResult a) -- ^ And a parser
  -> ChoiceDecoder a
choice = ChoiceDecoder

-- | Given a choice of elements, decode the first where the shift succeeds.
--
-- Using it usually takes this shape:
--
-- @
--   instance DecodeCursor LibrarySection where
--     decode = decodeChoice
--       [ choice (laxElement "fiction") decodeFiction
--       , choice (laxElement "non_fiction") decodeNonFiction
--       ]
--       where
--         decodeFiction  c   = Fiction <$> parseCursor parseText c
--         decodeNonFiction c = NonFiction <$> parseCursor parseDouble c
-- @
--
decodeChoice :: [ChoiceDecoder a] -> HCursor -> DecodeResult a
decodeChoice cds (HCursor c h) =
  withResHistory (h++)
  . maybe noMatch doDecode
  . find matched
  $ shifted
  where
    noHistory        = HCursor c []
    shifted          = fmap (\cd -> (cd,noHistory %/ (cd^.choiceDecoderShift))) cds
    matched          = successfulCursor . snd
    unMatched        = fmap (^._2.history) . filter (not . matched) $ shifted
    noMatch          = Left ("Choices Exhausted",thisOp Nothing)
    doDecode (cd,bh) = withResHistory (thisOp . Just) . (cd^.choiceDecoderDecode) $ bh
    thisOp hh        = [Choice unMatched hh]
    withResHistory f = first (& over _2 f)

-- | Helper function for parsing the text of the cursor
parseCursor :: (Text -> Either Text a) -> HCursor -> DecodeResult a
parseCursor f hc  = (first (,hc ^. history) . f . fold =<<) . cursorContents $ hc

instance DecodeCursor Text where decode = fmap fold . cursorContents
instance DecodeCursor Int where decode = parseCursor parseInt
instance DecodeCursor Integer where decode = parseCursor parseInteger
instance DecodeCursor Double where decode = parseCursor parseDouble
instance DecodeCursor Bool where decode = parseCursor parseBool
instance DecodeCursor IsoUTCTime where decode = parseCursor parseIsoUtcTime
instance DecodeCursor IsoDay where decode = parseCursor parseIsoDay
