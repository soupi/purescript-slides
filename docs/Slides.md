## Module Slides

A tiny EDSL for creating presentations

#### `Slides`

``` purescript
data Slides
```

Slides state for a component

#### `Slide`

``` purescript
data Slide
```

A single slide

#### `Element`

``` purescript
data Element
```

A data type defining the AST

##### Instances
``` purescript
Semigroup Element
Show Element
```

#### `(<+>)`

``` purescript
(<+>) :: Element -> Element -> Element
```

_right-associative / precedence 5_

Append two elements with some padding in between

#### `mkSlides`

``` purescript
mkSlides :: Array Slide -> Slides
```

Create slides component from an array of slides

#### `center`

``` purescript
center :: Element -> Element
```

Position an element at the center of its parent

#### `group`

``` purescript
group :: Array Element -> Element
```

Group elements as a unit

#### `empty`

``` purescript
empty :: Slide
```

An empty slide

#### `slide`

``` purescript
slide :: String -> Element -> Slide
```

Create a slide from a title and an element

#### `title`

``` purescript
title :: String -> Element
```

A title

#### `link`

``` purescript
link :: String -> Element -> Element
```

Turn an element into a link to url

#### `text`

``` purescript
text :: String -> Element
```

A text element

#### `image`

``` purescript
image :: String -> Element
```

An image element

#### `ulist`

``` purescript
ulist :: Array Element -> Element
```

An unordered list of element from an array of elements

#### `halign`

``` purescript
halign :: Array Element -> Element
```

Horizontally align elements in array

#### `valign`

``` purescript
valign :: Array Element -> Element
```

Vertically align elements in array


