{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{- |
Module      :  Text.XML.Decode
Description :  Provides a Cursor based XML decoder that gives nice error messages
Copyright   :  (c) Ben Kolera
License     :  MIT

Maintainer  :  Ben Kolera
Stability   :  experimental

This library extends the functionality of 'Text.XML.Cursor' to allow you to
accumulate history as you traverse the structure. This leads to greater context
about why your decoder failed when it invariably does.

Here is an example of a decoder:

@
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
@

If you tried to decode the following XML with a bad value for the section:

@
<library>
  <book id="1">
    <name>Learn you a haskell for great good!</name>
    <section>
      <reference>5.1</reference>
    </section>
    <published>2011-04-21</published>
    <lastBorrowed>2015-01-05T16:30:00Z</lastBorrowed>
    <category>Programming</category>
    <category>Haskell</category>
    <category>Functional Programming</category>
  </book>
</library>
@

Then you'd get an error that looks like this:

@
  Left (
    "Choices Exhausted"
    , [ MoveAxis Child , LaxElement "book"
      , MoveAxis Child , LaxElement "section"
      , Choice
        [ [ MoveAxis Child , LaxElement "fiction" ]
        , [ MoveAxis Child , LaxElement "non_fiction" ]]
        Nothing
      ])
@

Checkout the tests for more examples!

-}

module Text.XML.Decode
  ( module Text.XML.Decode.DecodeCursor
  , module Text.XML.Decode.HCursor
  , module Text.XML.Decode.Parsers
  , module Text.XML.Decode.Time
  ) where

import           Text.XML.Decode.DecodeCursor
import           Text.XML.Decode.HCursor
import           Text.XML.Decode.Parsers
import           Text.XML.Decode.Time
