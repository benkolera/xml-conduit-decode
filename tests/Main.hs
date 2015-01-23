{-# LANGUAGE TemplateHaskell #-}
module Main where

import BasePrelude hiding (readFile)
import Prelude     ()

import Control.Lens
import Data.Default     (def)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text
import Data.Time
import Text.XML         (Document,readFile)

import Text.XML.Decode

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  []

data BookCategory
  = Haskell
  | Scala
  | Programming
  | FunctionalProgramming
  deriving (Eq,Show)

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

decodeOk :: Assertion
decodeOk = decodeTest "ok" "library"

decodeTest :: (Eq a, Show a, DecodeCursor a) => FilePath -> Text -> a -> Assertion
decodeTest tn t r = do
  d <- loadXmlForTest tn
  let r = decodeSingle (fromDocument d %// laxElement t)
  r @?= (Right r)

loadXmlForTest :: FilePath -> IO Document
loadXmlForTest tn = readFile def ("tests/xml/" <> tn <> ".xml")
