{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Text.XML.Decode.HCursor
Description :  Traverses around a Cursor accumulating history
Copyright   :  (c) Ben Kolera
License     :  MIT

Maintainer  :  Ben Kolera
Stability   :  experimental

The big issue with using a plain 'Text.XML.Cursor' is that all you get when you
fail to parse what you were after is an Empty list of cursors and no idea how
you got there.

An HCursor, however, only allows you to traverse it using the combinators in this
file, and each one of these combinators accumulates `CursorHistory` in the HCursor
describing each navigation operation, so that if you ever get to a position where
you have an empty HCursor (i.e. the elements you were looking for didn't exist)
then you can use that history to describe where you went wrong in an error
message.

There is a general pattern to the combinators in this file:

Prefixes:

  * % apply a shift to a HCursor
  * $ apply a shift to a Cursor
  * & apply a shift to another shift (composes them)

Suffixes:

  * / Applies the shift to the children of the current foci
  * // Applies the shift to all descendants of the current foci

-}
module Text.XML.Decode.HCursor
  ( Shift
  , shift
  , HCursor(..)
  , CursorOp(..)
  , CursorAxis(..)
  , CursorResult
  , CursorHistory
  , Predicate(..)
  , foldCursor
  , fromCursor
  , fromDocument
  , failedCursor
  , successfulCursor
  , withHistory
  -- Lenses / Prisms
  , cursors
  , history
  , _Child
  , _Descendant
  , _Backtrack
  , _BacktrackSucceed
  , _GenericOp
  , _MoveAxis
  , _LaxElement
  , _FailedCompose
  , predFun
  , predDesc
  -- Shifts
  , laxElement
  , filterPred
  , shiftGeneric
 , (|||)
  , (***)
  , (%/)
  , (%//)
  , ($/)
  , ($//)
  , (&/)
  , (&//)
  ) where

import           Control.Lens       (makeLenses, makePrisms, over, to, (&),
                                     (^.))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NEL
import           Data.Text          (Text)
import qualified Text.XML           as X
import qualified Text.XML.Cursor    as C

-- | These describe the axis that we can move from one set of elements to another.
--   Note, these MoveAxis operations are the only CursorOps that actually "move"
--   the cursor and replace the current foci with another set of possibilities.
--
--   Every other operation is actually a filtering operation of the foci.
data CursorAxis
  = Child      -- ^ To just the immediate children of our elements
  | Descendant -- ^ To all descendants of the current elements
  deriving (Show,Eq)
makePrisms ''CursorAxis

type CursorHistory = [CursorOp]

-- | Describes the operations that got an HCursor into its state
data CursorOp =
  -- | We had a choice, determined by the shifts. The shifts that we failed
  --   to match are recorded and our potential success is too.
    Choice { _notMatched :: [CursorHistory] , _matched :: Maybe CursorHistory }
  -- | This is brought by the '|||' operator which backtracks to the next
  --   cursor if the first one fails
  | Backtrack { _failed :: CursorHistory , _new :: CursorHistory }
  -- | When the first choice of a backtrack succeeds
  | BacktrackSucceed CursorHistory
  -- | If you need to cheat and create your own Op with a text description
  | GenericOp Text
  -- | Move the cursor from its current foci to a new set of foci based on the axis
  | MoveAxis CursorAxis
  -- | Filter the current foci based on element name (case insensive, namespace free)
  | LaxElement Text
  -- | Filter the current foci based on a predicate (described by a string)
  | FilterPredicate Text
  -- | We tried to do a Shift onto a HCursor that was empty.
  | FailedCompose
  deriving (Show,Eq)
makePrisms ''CursorOp

-- | An HCursor carries around the elements of the XML in focus (the cursors)
--   and the history as to how we got these elements in focus.
data HCursor = HCursor
  { _cursors :: [C.Cursor]
  , _history :: CursorHistory
  } deriving (Show)
makeLenses ''HCursor

-- | A shift moves the HCursor foci to another set of foci, collection cursor
--   history in the new HCursor. If the shift could not find any elements from
--   this movement, the cursor will be empty.
data Shift = Shift { runShift :: C.Cursor -> HCursor }

-- | Construct a shift given a `Cursor` movement and a description of the movement
shift :: (C.Cursor -> [C.Cursor]) -> CursorOp -> Shift
shift f o = Shift (\ c -> HCursor (f c) [o])

-- | Tests to see whether this cursor still has foci to traverse
successfulCursor :: HCursor -> Bool
successfulCursor = foldCursor (const False) (const . const $ True)

-- | Tests to see if this cursor has no foci left (has failed)
failedCursor :: HCursor -> Bool
failedCursor = not . successfulCursor

-- | Modify the history of a cursor
withHistory :: (CursorHistory -> CursorHistory) -> HCursor -> HCursor
withHistory f = (& over history f)

bindCursor :: (C.Cursor -> HCursor) -> HCursor -> HCursor
bindCursor f = foldCursor aFail aWin
  where
    aFail h   = HCursor [] (h ++ [FailedCompose])
    aWin cs h = let
      cs' = fmap f cs
      ws  = NEL.filter successfulCursor cs'
      in case ws of
        []    -> HCursor [] (h ++ (cs' ^. to NEL.head . history))
        (x:_) -> HCursor (ws >>= (^. cursors)) (h ++ x ^. history)

-- | Fold on whether the cursor is failed
foldCursor
  :: (CursorHistory -> a) -- ^ Failure: Gives the history leading to this failure
  -> (NonEmpty C.Cursor -> CursorHistory -> a) -- ^ The foci and history
  -> HCursor
  -> a
foldCursor f _ (HCursor [] h )     = f h
foldCursor _ w (HCursor (x:xs) h ) = w (x :| xs) h


-- | Tries the first shift, and backtracks to try the second if the first fails
(|||) :: Shift -> Shift -> Shift
a ||| b = Shift step
  where
    step c       = foldCursor (aFail c) aWin . runShift a $ c
    aFail c ah   = withHistory (\ bh -> [Backtrack ah bh]) . runShift b $ c
    aWin cs ah   = HCursor (NEL.toList cs) [BacktrackSucceed ah]

(>=>) :: Shift -> Shift -> Shift
a >=> b = Shift $ bindCursor (runShift b) . runShift a

-- | Repeat a shift n times
(***) :: Shift -> Int -> Shift
s *** 0 = s
s *** n = s >=> (s *** (n - 1))

-- | Constructs a Generic Cheat Text shift operation
shiftGeneric :: Text -> (C.Cursor -> [C.Cursor]) -> Shift
shiftGeneric n f = shift f $ GenericOp n

-- | Apply this shift to the children of the current foci
(%/) :: HCursor -> Shift -> HCursor
hc %/ s = bindCursor (runShift (shiftAxis Child >=> s)) hc

-- | Apply this shift to all descendants of the current foci
(%//) :: HCursor -> Shift -> HCursor
hc %// s = bindCursor (runShift (shiftAxis Descendant >=> s)) hc

-- | Compose a shift to another shift, apply the right to children foci following the first shift
(&/) :: Shift -> Shift -> Shift
a &/ b = a >=> shiftAxis Child >=> b

-- | Compose a shift to another shift, apply the right all descendant foci following the first shift
(&//) :: Shift -> Shift -> Shift
a &// b = a >=> shiftAxis Descendant >=> b

-- | Apply a shift to children elements of a raw `Cursor`
($/) :: C.Cursor -> Shift -> HCursor
c $/ s = runShift (shiftAxis Child >=> s) c

-- | Apply a shift to descendant elements of a raw `Cursor`
($//) :: C.Cursor -> Shift -> HCursor
c $// s = runShift (shiftAxis Descendant >=> s) c


infixr 1 &/
infixr 1 &//
infixr 1 $/
infixr 1 $//
infixr 1 %/
infixr 1 %//

shiftAxis :: CursorAxis -> Shift
shiftAxis ca = shift (C.$| axis) $ MoveAxis ca
  where
    axis = case ca of
      Child      -> C.child
      Descendant -> C.descendant

-- | Filter foci based on element name, ignoring case or namespaces
laxElement :: Text -> Shift
laxElement n = shift (C.$| C.laxElement n) $ LaxElement n

-- | A node filtering function with a textual description
data Predicate = Predicate
  { _predDesc :: Text
  , _predFun  :: X.Node -> Bool
  }
makeLenses ''Predicate

-- | Filter foci based on the predicate
filterPred :: Predicate -> Shift
filterPred (Predicate d f) = shift (C.$| C.checkNode f) $ FilterPredicate d

type CursorResult a = Either (Text,CursorHistory) (NonEmpty a)

fromCursor :: C.Cursor -> HCursor
fromCursor c =  HCursor [c] []

fromDocument :: X.Document -> HCursor
fromDocument = fromCursor . C.fromDocument
