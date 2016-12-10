module Main where

import Prelude (($), (<>))
import Slides

main = runSlides slides


slides = mkSlides [s1, s2, s3, s4, s5]


s1 = slide "Slides" $
  valign
    [ image "https://i.imgur.com/Hm9pTxy.gif"
    , title "Let's build a presentation!"
    , center $
        text "(In "
        <+> link "http://purescript.org" (text "PureScript")
        <>  text ", Using "
        <+> link "https://github.com/soupi/purescript-slides" (text "purescript-slides")
        <>  text ")"
    ]

s2 = slide "Primitives" $
  valign
    [ text "We have the following primitives:"
    , ulist
        [ code "text" <+> text "- write a block of text"
        , code "code" <+> text "- write a block of code"
        , code "link" <+> text "- turn an element into a clickable link"
        , code "image" <+> text "- display an image from a url"
        , code "title" <+> text "- a title"
        , code "center" <+> text "- center an element"
        , code "bold"  <> text "/" <> code "italic" <+> text "-"
          <+> bold (text "bold")
          <+> text "and"
          <+> italic (text "italic")
        , code "withClass" <> text "/" <> code "withId" <+> text "- add a class or id to element"
        ]
    ]

s3 = slide "Combinators" $
  valign
    [ text "To combine elements, we can use the following combinators:"
    , center $ ulist
        [ code "valign" <+> text "- vertically align elements in a list"
        , code "halign" <+> text "- horizontally align elements in a list"
        , code "group" <+> text "- group an array of elements"
        , code "ulist" <+> text "- create a list of bullets"
        ]
    ]

s4 = slide "Creating slides" $
  ulist
    [ text "to create a slide, call the" <+> code "slide" <+> text "function with a title string and an element"
    , text "to create slides, call the" <+> code "mkSlides" <+> text "function with a list of slides"
    , text "to run the slides, call the" <+> code "runSlides" <+> text "function with the slides"
    ]

s5 = slide "That's it!" $
  valign
    [ text "This library is still tiny and may grow in the future :)"
    , center $ text "Interested? Check the source on" <+> link "https://github.com/soupi/purescript-slides" (text "Github") <> text "!"
    ]


