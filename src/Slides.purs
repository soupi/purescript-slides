-- | A tiny EDSL for creating presentations

module Slides
  ( runSlides
  , mkSlides
  , ui
  , Move()
  , Slides()
  , Slide()
  , Element()
  , (<+>)
  , appendPlus
  , slide
  , empty
  , title
  , text
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
import Data.Generic (class Generic, gShow)
import Data.List.Zipper as Z
import Control.Comonad (extract)
import Data.Array ((:), uncons, singleton)
import Data.List (List(..), length)
import Data.Functor (($>))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Eff (Eff)

import Halogen.HTML (className) as H
import Halogen.HTML.Properties (id_, class_) as H

import Halogen (ComponentDSL, Natural, ComponentHTML, Component, HalogenEffects, component, modify, runUI)
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen.Query (action)
import Halogen.HTML.Indexed (HTML(Element), className, span, li_, ul_, img, text, p, a, h2_, span_, div, button) as Html
import Halogen.HTML.Events.Indexed as Events
import Halogen.HTML.Events.Handler as Events
import Halogen.HTML.Properties.Indexed (class_, src, href) as Html


-------------
-- Running --
-------------

-- | run a component for a presentation
runSlides :: Slides -> Eff (HalogenEffects ()) Unit
runSlides slides = runHalogenAff do
  body <- awaitBody
  runUI ui slides body

-- | Halogen UI component for a presentation
ui :: forall g. Component Slides Move g
ui = component { render, eval }
  where

  render :: Slides -> ComponentHTML Move
  render (Slides state) =
    Html.span []
      [ Html.button [ keyboardEvent, Events.onClick (Events.input_ Back) ] [ Html.text "Back" ]
      , Html.button [ keyboardEvent, Events.onClick (Events.input_ Next) ] [ Html.text "Next" ]
      , Html.span
          [ Html.class_ $ Html.className "counter" ]
          [ Html.text $ show (position state + 1) <> " / " <> show (zipLength state) ]
      , renderSlides (extract state)
      ]

  eval :: Natural Move (ComponentDSL Slides Move g)
  eval move = do
    modify (\(Slides slides) -> Slides $ moveSlides move slides)
    pure (getNext move)

  keyMapping = case _ of
    35.0 -> Events.preventDefault $> map action (Just End) -- End key
    36.0 -> Events.preventDefault $> map action (Just Start) -- Home key
    37.0 -> Events.preventDefault $> map action (Just Back) -- Left Arrow key
    39.0 -> Events.preventDefault $> map action (Just Next) -- Right Arrow key
    _ -> pure Nothing

  keyboardEvent = Events.onKeyPress \e -> keyMapping e.keyCode

data Move a
  = Back a
  | Next a
  | Start a
  | End a

getNext :: forall a. Move a -> a
getNext = case _ of
  Back a -> a
  Next a -> a
  Start a -> a
  End a -> a

moveSlides :: forall a. Move a -> SlidesInternal -> SlidesInternal
moveSlides (Back _) slides =
  fromMaybe slides (Z.up slides)

moveSlides (Next _) slides =
  fromMaybe slides (Z.down slides)

moveSlides (Start _) slides =
  Z.beginning slides

moveSlides (End _) slides =
  Z.end slides


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

renderSlides :: forall p i. Slide -> Html.HTML p i
renderSlides (Slide el) =
  Html.div (giveClass "slide") [renderE el]

renderE :: forall p i. Element -> Html.HTML p i
renderE element =
  case element of
    Empty ->
      Html.span_ []

    Title tl ->
      Html.span (giveClass "title") [ Html.h2_ [ Html.text tl ] ]

    Link l el ->
      Html.a [ Html.href l ] [ renderE el ]

    Text str ->
      Html.p marwidStyle [ Html.text str ]

    Image url ->
      Html.img (marwidStyle <> [ Html.src url ])

    VAlign els ->
      Html.span colFlexStyle (applyRest block $ map renderE els)

    HAlign els ->
      Html.span rowFlexStyle (map renderE els)

    UList els ->
      Html.span [] [ Html.ul_ $ map (Html.li_ <<< singleton <<< renderE) els ]

    Group els ->
      Html.span [] $ map renderE els

    Class c e ->
      case renderE e of
        Html.Element nm tn props els ->
          Html.Element nm tn (props <> [ H.class_ $ H.className c ]) els
        el ->
          el

    Id i e ->
      case renderE e of
        Html.Element nm tn props els ->
          Html.Element nm tn (props <> giveId i) els
        el ->
          el

giveClasses = map (Html.class_ <<< Html.className)
giveClass = singleton <<< Html.class_ <<< Html.className
giveId    = singleton <<< H.id_

marwidStyle  = giveClass "marwid"
rowFlexStyle = giveClass "rowflex"
colFlexStyle = giveClass "colflex"

block x = Html.span (giveClass "block") [ x ]

applyRest :: forall a. (a -> a) -> Array a -> Array a
applyRest f xs =
  case uncons xs of
    Nothing -> xs
    Just list -> list.head : map f list.tail
