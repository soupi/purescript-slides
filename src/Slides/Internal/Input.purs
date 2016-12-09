module Slides.Internal.Input where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Int (toNumber)
import Signal (map2, merge, sampleOn)
import Signal (Signal, foldp) as S
import Signal.DOM (Touch, DimensionPair, tap, touch, windowDimensions)
import Signal.DOM (keyPressed) as S

input :: forall e. Eff (dom :: DOM | e) (S.Signal Input)
input = do
  arrows <- arrowsSignal
  taps   <- tapsSignal
  pure $ S.foldp updateInput initInput (merge arrows taps)

initInput :: Input
initInput =
  { arrows:
      { right: Idle
      , left: Idle
      , down: Idle
      , up: Idle
      }
  }

updateInput :: Arrows Boolean -> Input -> Input
updateInput arrI state =
  { arrows: arrFold arrI state.arrows
  }

simpleUpdateInput :: Arrows Boolean -> Input -> Input
simpleUpdateInput arr _ =
    { arrows: { left: f arr.left, right: f arr.right, down: f arr.down, up: f arr.up } }
  where f true = Click
        f false = Idle

type Input =
  { arrows :: Arrows BtnAction
  }

showInput :: Input -> String
showInput i = "Input\n  " <> showArrows i.arrows


type Arrows a =
  { right :: a
  , left  :: a
  , down  :: a
  , up    :: a
  }

data BtnAction
  = Click
  | Hold
  | Idle
  | Release

clickOrHold :: BtnAction -> Boolean
clickOrHold = case _ of
  Click -> true
  Hold  -> true
  _     -> false

instance showBtnAction :: Show BtnAction where
  show Idle = "Idle"
  show Hold = "Hold"
  show Click = "Click"
  show Release = "Release"

instance eqBtnAction :: Eq BtnAction where
  eq Hold Hold = true
  eq Click Click = true
  eq Idle Idle = true
  eq Release Release = true
  eq _ _ = false


showArrows :: Arrows BtnAction -> String
showArrows arrows =
  "Arrows "
  <> show arrows.left
  <> " "
  <> show arrows.down
  <> " "
  <> show arrows.up
  <> " "
  <> show arrows.right

arrFold :: Arrows Boolean -> Arrows BtnAction -> Arrows BtnAction
arrFold inp arrows =
  { right: btnStateUpdate inp.right arrows.right
  , left: btnStateUpdate inp.left arrows.left
  , down: btnStateUpdate inp.down arrows.down
  , up: btnStateUpdate inp.up arrows.up
  }

btnStateUpdate :: Boolean -> BtnAction -> BtnAction
btnStateUpdate false Hold = Release
btnStateUpdate false _    = Idle
btnStateUpdate true  Idle = Click
btnStateUpdate true  _    = Hold

arrowsSignal :: forall e. Eff (dom :: DOM | e) (S.Signal (Arrows Boolean))
arrowsSignal = do
  rightArrow <- S.keyPressed rightKeyCode
  leftArrow  <- S.keyPressed leftKeyCode
  downArrow  <- S.keyPressed downKeyCode
  upArrow    <- S.keyPressed upKeyCode
  pure $ { left: _, right: _, down: _, up: _ }
      <$>  leftArrow
      <*>  rightArrow
      <*>  downArrow
      <*>  upArrow

leftKeyCode :: Int
leftKeyCode = 37

upKeyCode :: Int
upKeyCode = 38

rightKeyCode :: Int
rightKeyCode = 39

downKeyCode :: Int
downKeyCode = 40

tapsSignal :: forall e. Eff (dom :: DOM | e) (S.Signal (Arrows Boolean))
tapsSignal = do
  sig <- sampleOn <$> tap <*> (map2 { t: _, wd: _ } <$> touch <*> windowDimensions)
  pure $ map touchToArrows sig

touchToArrows :: { t :: Array Touch, wd :: DimensionPair } -> Arrows Boolean
touchToArrows = case _ of
  { t: [t], wd: wd }
     | toNumber t.screenX / toNumber wd.w < 0.3 -> initArrBool { left = true }
     | toNumber t.screenX / toNumber wd.w > 0.7 -> initArrBool { right = true }
  _ -> initArrBool

initArrBool :: Arrows Boolean
initArrBool =
  { left: false
  , right: false
  , up: false
  , down: false
  }
