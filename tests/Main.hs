{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import           BasePrelude               hiding (readFile)
import           Prelude                   ()

import           Control.Lens
import           Data.Default              (def)
import           Data.Text
import           Data.Time
import           Filesystem.Path.CurrentOS (fromText)
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


data Book    = Book
  { _bookId           :: Integer
  , _bookName         :: Text
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

instance DecodeCursor Book where
  decode c = Book
    <$> decodeAttr    "id" parseInteger c
    <*> decodeSingle  (c %/ laxElement "name")
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
        (fromGregorian 2011 4 21)
        (Just (UTCTime (fromGregorian 2015 1 5) 59400))
        [Programming,Haskell,FunctionalProgramming]
      , Book
        2
        "Enterprise Pragmatic Scala"
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

loadXmlForTest :: Text -> IO Document
loadXmlForTest tn = readFile def . fromText $ "tests/xml/" <> tn <> ".xml"

tests :: TestTree
tests = testGroup "DecodeTests"
  [ testCase "ok" decodeOk
  , testCase "bad" decodeBad
  ]

main :: IO ()
main = defaultMain tests
