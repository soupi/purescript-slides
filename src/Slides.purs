-- | A tiny EDSL for creating presentations

module Slides
  ( runSlides
  , mkSlides
  , Slides()
  , Slide()
  , Element()
  , (<+>)
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

import Data.Maybe (Maybe(..))
import Prelude
import Data.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Array hiding (group)
import Data.Foldable (mconcat, intercalate)
import OpticUI.Markup.HTML as Html
import OpticUI.Markup as Html
import OpticUI as UI


-- | run a component for a presentation
runSlides (Slides slides) = UI.animate slides $ UI.with \state h ->
  let clicked action = const $ UI.runHandler h $ state # moveSlides action
  in mconcat $ UI.ui <$>
       [ Html.button [Html.titleA "Back", Html.onClick $ clicked Back] $ Html.text "Back"
       , Html.button [Html.titleA "Next", Html.onClick $ clicked Next] $ Html.text "Next"
       , render $ fromMaybe empty (state.slides !! state.pos)
       ]

-- | Slides state for a component
data Slides = Slides SlidesInternal

type SlidesInternal
  = { pos :: Int
    , slides :: Array Slide
    }

data Move
  = Back
  | Next

moveSlides :: Move -> SlidesInternal -> SlidesInternal
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
  | Style String Element

instance semigroupElement :: Semigroup Element where
  append e1 e2 = Group [e1, e2]

infixr 5 <+>

-- | Append two elements with some padding in between
(<+>) :: Element -> Element -> Element
(<+>) e1 e2 = e1 <> Style "padding: 8px;" Empty <> e2

-- | A Show instance for testing
instance showElement :: Show Element where
  show x =
    case x of
      Text  str -> "Text " <> str
      Link  l e -> "Link " <> l <> " (" <> show e <> ")"
      Title str -> "Title " <> str
      Image str -> "Image " <> str
      UList  xs -> "UList [" <> intercalate ", " (map show xs) <> "]"
      HAlign xs -> "HAlign [" <> intercalate ", " (map show xs) <> "]"
      VAlign xs -> "VAlign [" <> intercalate ", " (map show xs) <> "]"
      Group  els -> "Group " <> show (map show els)
      Style style e -> "Style " <> show style

-- | Create slides component from an array of slides
mkSlides :: Array Slide -> Slides
mkSlides [] = Slides { pos : 0, slides : [empty] }
mkSlides sl = Slides { pos : 0, slides : sl }


-- | Position an element at the center of its parent
center :: Element -> Element
center = Style "display: flex; margin: auto; justify-content: center;"

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

-- | An unordered list of element from an array of elements
ulist :: Array Element -> Element
ulist = UList

-- | Horizontally align elements in array
halign :: Array Element -> Element
halign = HAlign

-- | Vertically align elements in array
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

    Link l el ->
      Html.a [Html.attr "href" l] (renderE el)

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

    Group els ->
      Html.span [] $ mconcat $ map renderE els

    Style style e ->
      Html.span [Html.attr "style" style] (renderE e)

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
