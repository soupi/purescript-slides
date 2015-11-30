module Main where

import Prelude (map, ($), (<>))
import Slides

main = runSlides slides


slides = mkSlides [s1, s2, s3, s4, s5]


s1 = slide "Slides" $
  valign
    [ image "http://i.imgur.com/Hm9pTxy.gif"
    , title "Let's build a presentation!"
    , center $
        text "(In "
        <+> link "http://purescript.org" (text "PureScript")
        <>  text ", Using "
        <+> link "https://github.com/soupi/ps-slides" (text "ps-slides")
        <>  text ")"
    ]

s2 = slide "Primitives" $
  valign
    [ text "We have the following primitives:"
    , ulist
        [ text "text: write a block of text"
        , text "image: display an image from a url"
        , text "title: a title"
        , text "link: turn an element into a clickable link"
        , text "center: center an element"
        ]
    ]

s3 = slide "Combinators" $
  valign
    [ text "To combine elements, we can use the following combinators:"
    , center $ ulist
        [ text "group: group an array of elements"
        , text "valign: vertically align elements in a list"
        , text "halign: horizontally align elements in a list"
        , text "ulist: create a list of bullets"
        ]
    ]

s4 = slide "Creating slides" $
  ulist
  [ text "to create a slide, call the `slide` function with a title string and an element"
  , text "to create slides, call the `mkSlides` function with a list of slides"
  , text "to run the slides, call the `runSlides` function with the slides"
  ]

s5 = slide "That's it!" $
  valign
    [ text "This library is still tiny and may grow in the future :)"
    , center $ text "Interested? Check the source on" <+> link "https://github.com/soupi/ps-slides" (text "Github") <> text "!"
    ]


