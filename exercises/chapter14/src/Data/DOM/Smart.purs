module Data.DOM.Smart
  ( Element
  , Attribute
  , Content
  , AttributeKey

  , a
  , p
  , img
  , divs

  , href
  , _class
  , src
  , width
  , height

  , attribute, (:=)
  , text
  , elem
  , disabled

  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

newtype Attribute = Attribute
  { key          :: String
  , value        :: Maybe String
  }

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

newtype AttributeKey = AttributeKey String

attribute :: AttributeKey -> String -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: Just value
  }

infix 4 attribute as :=

emptyAttribute :: AttributeKey -> Attribute
emptyAttribute (AttributeKey key) = Attribute
  { key: key
  , value: Nothing
  }

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

divs :: Array Attribute -> Array Content -> Element
divs attribs content = element "div" attribs (Just content)

href :: AttributeKey
href = AttributeKey "href"

_class :: AttributeKey
_class = AttributeKey "class"

src :: AttributeKey
src = AttributeKey "src"

width :: AttributeKey
width = AttributeKey "width"

height :: AttributeKey
height = AttributeKey "height"

disabled :: Attribute
disabled = emptyAttribute $ AttributeKey "disabled"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = x.key <> case x.value of
                                               Just v -> "=\"" <> v <> "\""
                                               Nothing -> ""

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'
