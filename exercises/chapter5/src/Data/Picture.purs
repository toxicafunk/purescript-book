module Data.Picture where

import Data.Maybe
import Prelude

import Data.Foldable (foldl)
import Global as Global
import Math as Math

data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
--  | Clipped Picture Rectangle

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"
-- showShape (Clipped pic) = foldl (\acc a -> acc <> showShape a) "" pic

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

instance showShape' :: Show Shape where
  show (Circle point num) = showShape (Circle point num)

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

intersect :: Bounds -> Bounds -> Bounds
intersect (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = union (shapeBounds shape) b

fact :: Int -> Int -> Int
fact 0 acc = acc
fact n acc = fact (n - 1) (acc * n)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n - 1))

pascal :: Int -> Int -> Int
pascal _ k | k < 1 = 1
pascal n k | k > n = 0
pascal n k = (pascal (n - 1) k) + (pascal (n - 1) (k - 1))

sortPair :: Array Int -> Array Int
sortPair arr@[x,y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: x } } { address: { city: y }} = x == y

fromSingleton :: forall a. a -> Array a -> a
fromSingleton x [y] = y
fromSingleton x _ = x

aCircle :: Shape
aCircle = Circle (Point {x:0.0,y:0.0}) 10.0

double :: Shape -> Shape
double (Circle origin r) = Circle origin (r * 2.0)
double (Rectangle origin w h) = Rectangle origin (w*2.0) (h*2.0)
double (Line p1 (Point {x,y})) = Line p1 (Point {x: x*2.0, y: y*2.0})
double text@(Text _ _) = text

extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _ = Nothing

extractText' :: Shape -> Maybe String
extractText' shape = case shape of
  Text _ text -> Just text
  _ -> Nothing

area :: Shape -> Number
area (Circle _ r) = 2.0 * (Math.pi * (r `Math.pow` 2.0)) 
area (Rectangle _ w h) = w * h
area _ = 0.0
