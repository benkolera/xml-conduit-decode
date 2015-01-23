{-# LANGUAGE TemplateHaskell #-}

module Text.XML.Decode.HCursor
  ( HCursor(..)
  , CursorOp(..)
  , CursorAxis(..)
  , CursorResult
  , CursorHistory
  , Predicate(..)
  , Shift
  , foldCursor
  , fromCursor
  , fromDocument
  , tryCursor
  , failedCursor
  , successfulCursor
  -- Lenses / Prisms
  , cursors
  , history
  , _Child
  , _Descendant
  , _ChoiceSucceed
  , _ChoiceSwitch
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

import Prelude ()
import BasePrelude hiding (shift,(>=>),(***),(|||))

import Control.Lens ((^.),(&),to,makeLenses,makePrisms,over)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import qualified Text.XML.Cursor as C
import qualified Text.XML as X

data CursorAxis = Child | Descendant deriving (Show,Eq)
makePrisms ''CursorAxis

type CursorHistory = [CursorOp]

data CursorOp =
  ChoiceSucceed CursorHistory
  | ChoiceSwitch { _failed :: CursorHistory , _new :: CursorHistory }
  | GenericOp Text
  | MoveAxis CursorAxis
  | LaxElement Text
  | FilterPredicate Text
  | FailedCompose
  deriving (Show,Eq)
makePrisms ''CursorOp

data HCursor = HCursor
  { _cursors :: [C.Cursor]
  , _history :: CursorHistory
  } deriving (Show)
makeLenses ''HCursor

successfulCursor :: HCursor -> Bool
successfulCursor = foldCursor (const False) (const . const $ True)

failedCursor :: HCursor -> Bool
failedCursor = not . successfulCursor

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

foldCursor ::
  (CursorHistory -> a) ->
  (NonEmpty C.Cursor -> CursorHistory -> a) ->
  HCursor ->
  a
foldCursor f _ (HCursor [] h )     = f h
foldCursor _ w (HCursor (x:xs) h ) = w (x :| xs) h

data Shift = Shift { runShift :: C.Cursor -> HCursor }
(|||) :: Shift -> Shift -> Shift
a ||| b = Shift step
  where
    step c       = foldCursor (aFail c) aWin . runShift a $ c
    aFail c ah   = withHistory (\ bh -> [ChoiceSwitch ah bh]) . runShift b $ c
    aWin cs ah   = HCursor (NEL.toList cs) [ChoiceSucceed ah]

(>=>) :: Shift -> Shift -> Shift
a >=> b = Shift $ bindCursor (runShift b) . runShift a

(***) :: Shift -> Int -> Shift
s *** 0 = s
s *** n = s >=> (s *** (n - 1))


shift :: (C.Cursor -> [C.Cursor]) -> CursorOp -> Shift
shift f o = Shift (\ c -> HCursor (f c) [o])

shiftGeneric :: Text -> (C.Cursor -> [C.Cursor]) -> Shift
shiftGeneric n f = shift f $ GenericOp n

(%/) :: HCursor -> Shift -> HCursor
hc %/ s = bindCursor (runShift (shiftAxis Child >=> s)) hc

(%//) :: HCursor -> Shift -> HCursor
hc %// s = bindCursor (runShift (shiftAxis Descendant >=> s)) hc

($/) :: C.Cursor -> Shift -> HCursor
c $/ s = runShift (shiftAxis Child >=> s) c

($//) :: C.Cursor -> Shift -> HCursor
c $// s = runShift (shiftAxis Descendant >=> s) c

(&/) :: Shift -> Shift -> Shift
a &/ b = a >=> shiftAxis Child >=> b

(&//) :: Shift -> Shift -> Shift
a &// b = a >=> shiftAxis Descendant >=> b

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

laxElement :: Text -> Shift
laxElement n = shift (C.$| C.laxElement n) $ LaxElement n

data Predicate = Predicate
  { _predDesc :: Text
  , _predFun  :: X.Node -> Bool
  }
makeLenses ''Predicate

filterPred :: Predicate -> Shift
filterPred (Predicate d f) = shift (C.$| C.checkNode f) $ FilterPredicate d

type CursorResult a = Either (Text,CursorHistory) (NonEmpty a)

fromCursor :: C.Cursor -> HCursor
fromCursor c =  HCursor [c] []

fromDocument :: X.Document -> HCursor
fromDocument = fromCursor . C.fromDocument

tryCursor ::
  (HCursor -> HCursor) ->
  (HCursor -> HCursor) ->
  HCursor ->
  HCursor
tryCursor f w = foldCursor f' w'
  where
    f'      = f . HCursor []
    w' cs h = w $ HCursor (NEL.toList cs) h
