{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           Prelude                   (Double, Eq, IO, Integer, Show)

import           Control.Applicative       ((<$>), (<*>))
import           Control.Category          ((.))
import           Control.Lens

import           Data.Default              (def)
import           Data.Either               (Either (..))
import           Data.Function             (($))
import           Data.Functor              (fmap)
import           Data.Maybe                (Maybe (..))
import           Data.Monoid               ((<>))
import           Data.Text
import           Data.Time

import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.XML                  (Document, readFile)

import           Text.XML.Decode
import           Text.XML.Decode.Instances ()

data BookCategory
  = Haskell
  | Scala
  | Programming
  | FunctionalProgramming
  deriving (Eq,Show)

bookCategoryFromText :: Text -> Maybe BookCategory
bookCategoryFromText "Haskell"                = Just Haskell
bookCategoryFromText "Scala"                  = Just Scala
bookCategoryFromText "Programming"            = Just Programming
bookCategoryFromText "Functional Programming" = Just FunctionalProgramming
bookCategoryFromText _ = Nothing


data LibrarySection
  = Fiction Text       -- Fiction by author first letter(s)
  | NonFiction Double  -- Dewey decimal number
  deriving (Eq,Show)
makePrisms ''LibrarySection

data Book    = Book
  { _bookId           :: Integer
  , _bookName         :: Text
  , _bookSection      :: LibrarySection
  , _bookPublished    :: Day
  , _bookLastBorrowed :: Maybe UTCTime
  , _bookCategories   :: [BookCategory]
  } deriving (Eq,Show)
makeLenses ''Book

data Library = Library
  { _books :: [Book]
  } deriving (Eq,Show)
makeLenses ''Library

instance DecodeCursor BookCategory where
  decode = parseCursor (parseMaybe "BookCategory" bookCategoryFromText)

instance DecodeCursor LibrarySection where
  decode = decodeChoice
    [ choice (laxElement "fiction") decodeFiction
    , choice (laxElement "non_fiction") decodeNonFiction
    ]
    where
      decodeFiction  c   = Fiction <$> parseCursor parseText c
      decodeNonFiction c = NonFiction <$> parseCursor parseDouble c

instance DecodeCursor Book where
  decode c = Book
    <$> decodeAttr    "id" parseInteger c
    <*> decodeSingle  (c %/ laxElement "name")
    <*> decodeSingle  (c %/ laxElement "section")
    <*> (decodeSingle (c %/ laxElement "published") <&> toDay)
    <*> (decodeMay    (c %/ laxElement "lastBorrowed") <&> fmap toUtcT)
    <*> decodeMany    (c %/ laxElement "category")

instance DecodeCursor Library where
  decode c = Library
    <$> decodeMany ( c %/ laxElement "book" )

decodeOk :: Assertion
decodeOk = do
  c <- fromDocument <$> loadXmlForTest "ok"
  let r = decodeSingle c
  r @?= Right expectedLibrary
  where
    expectedLibrary = Library
      [ Book
        1
        "Learn you a haskell for great good!"
        (NonFiction 5.1)
        (fromGregorian 2011 4 21)
        (Just (UTCTime (fromGregorian 2015 1 5) 59400))
        [Programming,Haskell,FunctionalProgramming]
      , Book
        2
        "Enterprise Pragmatic Scala"
        (Fiction "K")
        (fromGregorian 2013 5 1)
        Nothing
        [Programming,Scala]
      ]

decodeBad :: Assertion
decodeBad = do
  c <- fromDocument <$> loadXmlForTest "bad"
  let r = (decodeSingle c :: DecodeResult Library)
  r @?= Left (
    "'For teh lulz!' was not BookCategory"
    ,[ MoveAxis Child
     , LaxElement "book"
     , MoveAxis Child
     , LaxElement "category"
    ])

decodeBadChoice :: Assertion
decodeBadChoice = do
  c <- fromDocument <$> loadXmlForTest "bad_choice"
  let r = (decodeSingle c :: DecodeResult Library)
  r @?= Left (
    "Choices Exhausted"
    , [ MoveAxis Child , LaxElement "book"
      , MoveAxis Child , LaxElement "section"
      , Choice
        [ [ MoveAxis Child , LaxElement "fiction" ]
        , [ MoveAxis Child , LaxElement "non_fiction" ]]
        []])

decodeFailedDecodeInsideChoice :: Assertion
decodeFailedDecodeInsideChoice = do
  c <- fromDocument <$> loadXmlForTest "bad_decode_inside_choice"
  let r = (decodeSingle c :: DecodeResult Library)
  r @?= Left (
    "'This is not a dewey decimal' was not a double"
    , [ MoveAxis Child , LaxElement "book"
      , MoveAxis Child , LaxElement "section"
      , Choice
        [ [ MoveAxis Child , LaxElement "fiction" ] ]
        [ MoveAxis Child , LaxElement "non_fiction" ] ] )

loadXmlForTest :: Text -> IO Document
loadXmlForTest tn = readFile def . unpack $ "tests/xml/" <> tn <> ".xml"

tests :: TestTree
tests = testGroup "DecodeTests"
  [ testCase "ok" decodeOk
  , testCase "bad" decodeBad
  , testCase "bad_choice" decodeBadChoice
  , testCase "bad_decode_inside_choice" decodeFailedDecodeInsideChoice
  ]

main :: IO ()
main = defaultMain tests
