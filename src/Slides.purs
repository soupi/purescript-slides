-- | A tiny EDSL for creating presentations

module Slides
  ( runSlides
  , mkSlides
  , Slides()
  , Slide()
  , Element()
  , slide
  , empty
  , title
  , text
  , image
  , valign
  , halign
  , ulist
  ) where

import Data.Maybe (Maybe(..))
import Prelude
import Data.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Array
import Data.Foldable (mconcat, intercalate)
import OpticUI.Markup.HTML as Html
import OpticUI.Markup as Html
import OpticUI as UI


-- | A component for a presentation
runSlides slides = UI.animate slides $ UI.with \state h ->
  let clicked action = const $ UI.runHandler h $ state # moveSlides action
  in mconcat $ UI.ui <$>
       [ Html.button [Html.titleA "Back", Html.onClick $ clicked Back] $ Html.text "Back"
       , Html.button [Html.titleA "Next", Html.onClick $ clicked Next] $ Html.text "Next"
       , render $ fromMaybe empty (state.slides !! state.pos)
       ]


type Slides
  = { pos :: Int
    , slides :: Array Slide
    }

data Move
  = Back
  | Next

moveSlides :: Move -> Slides -> Slides
moveSlides Back s =
  if s.pos - 1 < 0
  then s
  else s { pos = s.pos - 1 }
moveSlides Next s =
  if s.pos + 1 >= length s.slides
  then s
  else s { pos = s.pos + 1 }

-- | A single slide
data Slide
  = Slide Element

data Element
  = Empty
  | Title  String
  | Text   String
  | Image  String
  | HAlign (Array Element)
  | VAlign (Array Element)
  | UList  (Array Element)

-- | A Show instance for testing
instance showElement :: Show Element where
  show x =
    case x of
      Text  str -> "Text " <> str
      Title str -> "Title " <> str
      Image str -> "Image " <> str
      UList  xs -> "UList [" <> intercalate ", " (map show xs) <> "]"
      HAlign xs -> "HAlign [" <> intercalate ", " (map show xs) <> "]"
      VAlign xs -> "VAlign [" <> intercalate ", " (map show xs) <> "]"

mkSlides :: Array Slide -> Slides
mkSlides [] = { pos : 0, slides : [empty] }
mkSlides sl = { pos : 0, slides : sl }

empty :: Slide
empty = Slide Empty

slide :: String -> Element -> Slide
slide ttl el = Slide (valign [title ttl, el])

title :: String -> Element
title ttl = halign [valign [text ""], Title ttl, valign [text ""]]

text :: String -> Element
text = Text

image :: String -> Element
image = Image

ulist :: Array Element -> Element
ulist = UList

halign :: Array Element -> Element
halign = HAlign

valign :: Array Element -> Element
valign = VAlign

render :: Slide -> Html.Markup
render (Slide el) =
  Html.div  [Html.attr "style" "width: 80%; height: 80%; margin: auto; display: flex; justify-content: space-around; align-items: center;"]
  (renderE el)

renderE :: Element -> Html.Markup
renderE element =
  case element of
    Empty ->
      mempty

    Title tl ->
      Html.span [Html.attr "style" "text-align: center; display: inline-block; margin: auto;"] (Html.h2_ (Html.text tl))

    Text str ->
      Html.p marwidStyle (Html.text str)

    Image url ->
      Html.img (marwidStyle ++ [Html.attr "src" url]) mempty

    VAlign els ->
      Html.span colFlexStyle (mconcat $ applyRest block $ map renderE els)

    HAlign els ->
      Html.span rowFlexStyle (mconcat $ map renderE els)

    UList els ->
      Html.span [] (Html.ul_ $ mconcat $ map (Html.li_ <<< renderE) els)

marwidStyle =
    [ Html.attr "style" "display: inline-block; margin: auto;" ]

rowFlexStyle =
    [ Html.attr "style" "display: flex; flex-flow: row wrap;" ]

colFlexStyle =
    [ Html.attr "style" "display: flex; flex-flow: column wrap;" ]

block x =
  Html.span [Html.attr "style" "display: block;"] x

applyRest f xs =
  case uncons xs of
    Nothing -> xs
    Just list -> list.head : map f list.tail
