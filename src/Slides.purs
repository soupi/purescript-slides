-- | A tiny EDSL for creating presentations

module Slides
  ( runSlides
  , runSlidesWithMoves
  , mkSlides
  , Move(..)
  , Slides()
  , Slide()
  , Element()
  , (<+>)
  , appendPlus
  , slide
  , empty
  , title
  , text
  , code
  , image
  , link
  , valign
  , halign
  , ulist
  , group
  , center
  , withClass
  , withId
  , bold
  , italic
  ) where

import Prelude
import Data.List.Zipper as Z
import Signal (Signal)
import Slides.Internal.Input as I
import Control.Comonad (extract)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array ((:), uncons, singleton)
import Data.Foldable (foldMap, fold, foldr)
import Data.Generic (class Generic, gShow)
import Data.List (List(..), length)
import Data.Maybe (Maybe(..), fromMaybe)
import Signal (foldp, runSignal) as S
import Text.Smolder.HTML (p, div, img, a, h2, ul, li, span, pre) as H
import Text.Smolder.HTML.Attributes (className, id, src, href) as H
import Text.Smolder.Markup (Markup, text) as H
import Text.Smolder.Markup ((!))
import Text.Smolder.Renderer.String (render) as H

-------------
-- Running --
-------------


-- | run a component for a presentation
runSlides :: forall e. Slides -> Eff ( dom :: DOM | e ) Unit
runSlides slides = do
  inn <- I.input
  runSlidesWithMoves (inputToMove <$> inn) slides

-- | run a component for a presentation with a custom Move
-- | Signal to determine when the slides should move and to which
-- | Slide.
runSlidesWithMoves :: forall e. Signal Move -> Slides -> Eff ( dom :: DOM | e ) Unit
runSlidesWithMoves move (Slides slides) = do
  let ui = S.foldp moveSlides slides move
  S.runSignal (setHtml <<< H.render <<< render <$> ui)


-- Rendering

render :: forall a. SlidesInternal -> H.Markup a
render slides =
  H.div ! H.id "main" $ do
    H.span ! H.className "counter" $
      H.text $ show (position slides + 1) <> " / " <> show (zipLength slides)
    renderSlides (extract slides)

foreign import setHtml :: forall e. String -> Eff ( dom :: DOM | e ) Unit

-- | Which way should the slides move:
-- | - `Back`: Go back one slide. If at the beginning will do nothing
-- | - `Next`: Go to next slide. If at the end will do nothing
-- | - `Start`: Go to the start
-- | - `End`: Go to the end
-- | - `BackOrEnd`: Like `Back` but will wrap around
-- | - `NextOrEnd`: Like `Next` but will wrap around
data Move
  = Back
  | Next
  | Start
  | End
  | BackOrEnd
  | NextOrStart
  | None

inputToMove :: I.Input -> Move
inputToMove i
  | I.clickOrHold (i.arrows.right) = Next
  | I.clickOrHold (i.arrows.left)  = Back
  | I.clickOrHold (i.arrows.down)  = Start
  | I.clickOrHold (i.arrows.up)    = End
  | otherwise = None

moveSlides :: Move -> SlidesInternal -> SlidesInternal
moveSlides m slides = case m of
  NextOrStart ->
    fromMaybe (moveSlides Start slides) (Z.up slides)

  BackOrEnd ->
    fromMaybe (moveSlides End slides) (Z.up slides)

  Next ->
    fromMaybe slides (Z.down slides)

  Back ->
    fromMaybe slides (Z.up slides)

  Start ->
    Z.beginning slides

  End ->
    Z.end slides

  None ->
    slides


position :: forall a. Z.Zipper a -> Int
position (Z.Zipper b _ _) = length b

zipLength :: forall a. Z.Zipper a -> Int
zipLength (Z.Zipper b _ a) = 1 + length b + length a

-----------
-- Model --
-----------

-- | Slides state for a component
data Slides = Slides SlidesInternal

type SlidesInternal
  = Z.Zipper Slide

-- | A single slide
data Slide
  = Slide Element

-- | A data type defining the AST
data Element
  = Empty
  | Title  String
  | Text   String
  | Code   String
  | Image  String
  | Link   String Element
  | HAlign (Array Element)
  | VAlign (Array Element)
  | UList  (Array Element)
  | Group  (Array Element)
  | Class String Element
  | Id    String Element

derive instance genericElement :: Generic Element

-- | A Show instance for testing
instance showElement :: Show Element where
  show = gShow

instance semigroupElement :: Semigroup Element where
  append e1 e2 = Group [e1, e2]

-- | Append two elements with some padding in between
appendPlus :: Element -> Element -> Element
appendPlus e1 e2 = e1 <> Class "padapp" Empty <> e2

-- | Append two elements with some padding in between
infixr 5 appendPlus as <+>


-----------
-- Utils --
-----------

-- | Create slides component from an array of slides
mkSlides :: Array Slide -> Slides
mkSlides sl = case uncons sl of
  Just {head, tail} ->
    Slides $ Z.Zipper Nil head (foldr Cons Nil tail)

  Nothing ->
    Slides $ Z.Zipper Nil empty Nil

-- | Create a slide from a title and an element
slide :: String -> Element -> Slide
slide ttl el = Slide (valign [title ttl, el])

-- | An empty slide
empty :: Slide
empty = Slide Empty

-- | A title
title :: String -> Element
title ttl = halign [valign [text ""], Title ttl, valign [text ""]]

-- | Turn an element into a link to url
link :: String -> Element -> Element
link = Link

-- | A text element
text :: String -> Element
text = Text

-- | An image element
image :: String -> Element
image = Image

-- | A <pre> element
code :: String -> Element
code = Code

-- | Group elements as a unit
group :: Array Element -> Element
group = Group

-- | Horizontally align elements in array
halign :: Array Element -> Element
halign = HAlign

-- | Vertically align elements in array
valign :: Array Element -> Element
valign = VAlign

-- | An unordered list of element from an array of elements
ulist :: Array Element -> Element
ulist = UList

-- | Add an Html class to an element
withClass :: String -> Element -> Element
withClass = Class

-- | Add an Html id to an element
withId :: String -> Element -> Element
withId = Id

-- | Position an element at the center of its parent
center :: Element -> Element
center = withClass "center" <<< group <<< singleton

-- | Format text with bold in this element
bold :: Element -> Element
bold = withClass "boldEl" <<< group <<< singleton

-- | Format text with italic in this element
italic :: Element -> Element
italic = withClass "italicEl" <<< group <<< singleton


---------------
-- Rendering --
---------------

renderSlides :: forall a. Slide -> H.Markup a
renderSlides (Slide el) =
  H.div ! H.className "slide" $ renderE el

renderE :: forall a. Element -> H.Markup a
renderE element =
  case element of
    Empty ->
      H.span (H.text "")

    Title tl ->
      H.span ! H.className "title" $ H.h2 (H.text tl)

    Link l el ->
      H.a ! H.href l $ renderE el

    Text str ->
      H.p ! H.className "marwid" $ H.text str

    Code c ->
      H.pre ! H.className "marwid" $ H.text c

    Image url ->
      H.img ! H.className "marwid" ! H.src url

    VAlign els ->
      H.span ! H.className "colflex" $ fold $ applyRest block $ map renderE els

    HAlign els ->
      H.span ! H.className "rowflex" $ foldMap renderE els

    UList els ->
      H.span $ H.ul $ foldMap (H.li <<< renderE) els

    Group els ->
      H.span $ foldMap renderE els

    Class c e ->
      renderE e ! H.className c

    Id i e ->
      renderE e ! H.id i

block :: forall a. H.Markup a -> H.Markup a
block = H.span ! H.className "block"

applyRest :: forall a. (a -> a) -> Array a -> Array a
applyRest f xs =
  case uncons xs of
    Nothing -> xs
    Just list -> list.head : map f list.tail

