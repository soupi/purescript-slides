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
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (intercalate)
import Data.Array ((:), uncons, singleton, length, (!!))
import Control.Monad.Eff (Eff)
import Halogen.Util (awaitBody, runHalogenAff)
import Halogen (ComponentDSL, Natural, ComponentHTML, Component, HalogenEffects, component, modify, runUI)
import Halogen.HTML.Core (HTML())
import Halogen.HTML.Events.Indexed as Events
import Halogen.HTML.Indexed (className, span, li_, ul_, img, text, p, a, h2_, span_, div, button) as Html
import Halogen.HTML.Properties.Indexed (class_, id_, src, href) as Html


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
      [ Html.button [ Events.onClick (Events.input_ Back) ] [ Html.text "Back" ]
      , Html.button [ Events.onClick (Events.input_ Next) ] [ Html.text "Next" ]
      , renderSlides $ fromMaybe empty (state.slides !! state.pos)
      ]

  eval :: Natural Move (ComponentDSL Slides Move g)
  eval move@(Next next) = do
    modify (\(Slides slides) -> Slides $ moveSlides move slides)
    pure next
  eval move@(Back next) = do
    modify (\(Slides slides) -> Slides $ moveSlides move slides)
    pure next


-- | Slides state for a component
data Slides = Slides SlidesInternal

type SlidesInternal
  = { pos :: Int
    , slides :: Array Slide
    }

data Move a
  = Back a
  | Next a

moveSlides :: forall a. Move a -> SlidesInternal -> SlidesInternal
moveSlides (Back _) s =
  if s.pos - 1 < 0
  then s
  else s { pos = s.pos - 1 }
moveSlides (Next _) s =
  if s.pos + 1 >= length s.slides
  then s
  else s { pos = s.pos + 1 }


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

instance semigroupElement :: Semigroup Element where
  append e1 e2 = Group [e1, e2]

-- | Append two elements with some padding in between
appendPlus :: Element -> Element -> Element
appendPlus e1 e2 = e1 <> Class "padapp" Empty <> e2

-- | Append two elements with some padding in between
infixr 5 appendPlus as <+>

-- | A Show instance for testing
instance showElement :: Show Element where
  show =
    case _ of
      Text  str -> "Text " <> show str
      Link  l e -> "Link " <> l <> " (" <> show e <> ")"
      Title str -> "Title " <> show str
      Image str -> "Image " <> show str
      UList  xs -> "UList ["  <> intercalate ", " (map show xs) <> "]"
      HAlign xs -> "HAlign [" <> intercalate ", " (map show xs) <> "]"
      VAlign xs -> "VAlign [" <> intercalate ", " (map show xs) <> "]"
      Group  xs -> "Group ["  <> intercalate ", " (map show xs) <> "]"
      Class c e -> "Class "   <> show c <> " (" <> show e <> ")"
      Id    i e -> "Id "      <> show i <> " (" <> show e <> ")"
      Empty -> ""

-- | Create slides component from an array of slides
mkSlides :: Array Slide -> Slides
mkSlides [] = Slides { pos : 0, slides : [empty] }
mkSlides sl = Slides { pos : 0, slides : sl }


-- | Position an element at the center of its parent
center :: Element -> Element
center = Class "center"

-- | Group elements as a unit
group :: Array Element -> Element
group = Group

-- | An empty slide
empty :: Slide
empty = Slide Empty

-- | Create a slide from a title and an element
slide :: String -> Element -> Slide
slide ttl el = Slide (valign [title ttl, el])

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

-- | Add an Html class to an element
withClass :: String -> Element -> Element
withClass = Class

-- | Add an Html id to an element
withId :: String -> Element -> Element
withId = Id

-- | An unordered list of element from an array of elements
ulist :: Array Element -> Element
ulist = UList

-- | Horizontally align elements in array
halign :: Array Element -> Element
halign = HAlign

-- | Vertically align elements in array
valign :: Array Element -> Element
valign = VAlign

renderSlides :: forall p i. Slide -> HTML p i
renderSlides (Slide el) =
  Html.div [ Html.class_ $ Html.className "slide" ] [renderE el]

renderE element =
  case element of
    Empty ->
      Html.span_ []

    Title tl ->
      Html.span [ Html.class_ $ Html.className "title" ] [ Html.h2_ [ Html.text tl ] ]

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
      Html.span [Html.class_ $ Html.className c] [renderE e]

    Id i e ->
      Html.span [Html.id_ i] [renderE e]

marwidStyle =
    [ Html.class_ $ Html.className "marwid" ]

rowFlexStyle =
    [ Html.class_ $ Html.className "rowflex" ]

colFlexStyle =
    [ Html.class_ $ Html.className "colflex" ]

block x =
  Html.span [ Html.class_ $ Html.className "block" ] [ x ]

applyRest f xs =
  case uncons xs of
    Nothing -> xs
    Just list -> list.head : map f list.tail
