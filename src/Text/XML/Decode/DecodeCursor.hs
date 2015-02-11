{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Text.XML.Decode.DecodeCursor
  ( DecodeCursor
  , DecodeResult
  , decode
  , decodeDocument
  , decodeSingle
  , decodeDefault
  , decodeMay
  , decodeMany
  , decodeNel
  , decodeAttr
  , decodeAttrMay
  , parseCursor
  , cursorContents
  ) where

import           BasePrelude             hiding (first)
import           Prelude                 ()

import           Control.Lens            ((^.))
import           Data.Bifunctor          (first)
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.List.NonEmpty      as NEL
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Text.XML                (Document)
import qualified Text.XML.Cursor         as C

import           Text.XML.Decode.HCursor

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

cursorContents :: HCursor -> CursorResult Text
cursorContents = foldCursor f w
  where
    f h      = Left ("Tried to decode a failed cursor",h)
    w cs   _ = Right . fmap (T.concat . (C.$// C.content)) $ cs

class DecodeCursor a where
  decode :: HCursor -> DecodeResult a

decodeDocument :: DecodeCursor a
  => (HCursor -> HCursor)
  -> Document
  -> Either (Text,CursorHistory,Document) a
decodeDocument s d = first (\ (t,h) -> (t,h,d)) . decode . s . fromDocument $ d

decodeSingle :: DecodeCursor a => HCursor -> DecodeResult a
decodeSingle = (decode . NEL.head =<<) . nelCursor

decodeDefault :: DecodeCursor a => a -> HCursor -> DecodeResult a
decodeDefault a = fmap (fromMaybe a) . decodeMay

decodeMay :: DecodeCursor a => HCursor -> DecodeResult (Maybe a)
decodeMay = foldCursor (const (Right Nothing)) w
  where
    w cs h = Just <$> decode (HCursor [NEL.head cs] h)

decodeMany :: DecodeCursor a => HCursor -> DecodeResult [a]
decodeMany = foldCursor (const $ return []) w
  where
    w cs h = NEL.toList <$> traverse (\ c -> decode $ HCursor [c] h) cs

decodeNel :: DecodeCursor a => HCursor -> DecodeResult (NonEmpty a)
decodeNel hc = nelCursor hc >>= traverse decode

decodeAttr :: Text -> (Text -> Either Text a) -> HCursor -> DecodeResult a
decodeAttr n f hc =
  (first ((,hc ^. history) . errorMessage) . f . NEL.head =<<)
  . cursorAttribute n
  $ hc
  where
    errorMessage pe = T.concat ["Failed to get attr (",n,"): ",pe]

decodeAttrMay :: Text -> (Text -> Either Text a) -> HCursor -> DecodeResult (Maybe a)
decodeAttrMay n f = decodeAttr n parse
  where
    parse "" = Right Nothing
    parse t  = Just <$> f t

data ChoiceDecoder a = ChoiceDecoder
  { choiceDecoderShift  :: Shift
  , choiceDecoderDecode :: (HCursor -> DecodeResult a)
  }

choice :: Shift -> (HCursor -> DecodeResult a) -> ChoiceDecoder a
choice = ChoiceDecoder

decodeChoice :: [ChoiceDecoder a] -> HCursor -> DecodeResult a
decodeChoice cds c =
  -- TODO: This isn't right. We want to fold through the choices
  -- and collect up ChoiceSwitches as we fail to match the initial
  -- shift of the choice. ||| isn't going to help us, but we can do
  -- something very similiar.
  maybe noMatch doDecode . find choiceSuccess . fmap shiftChoice $ cds
  where
    shiftChoice cd      = (cd,c %/ (choiceDecoderShift cd))
    choiceSuccess (_,c) = successfulCursor c
    noMatch             = _
    doDecode            = _

parseCursor :: (Text -> Either Text a) -> HCursor -> DecodeResult a
parseCursor f hc  = (first (,hc ^. history) . f . fold =<<) . cursorContents $ hc
