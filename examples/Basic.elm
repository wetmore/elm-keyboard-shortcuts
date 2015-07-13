module Basic where

import Graphics.Element exposing (..)
import Signal exposing ((<~))
import KeyboardShortcuts exposing (listenFor)

(:->) = (,)

pairs =
  [ "a b"    :-> 1
  , "a b c"  :-> 2
  , "ctrl+c" :-> 3
  , "up up down down left right left right b a enter" :-> 9001
  ]

test = listenFor 0 pairs

info = show <| "Try the following shortcuts:"

shortcuts = let
    showShortcut (sc, ev) = show <| sc -- ++ " -> " ++ toString ev
  in List.map showShortcut pairs

entered : Signal (List Int)
entered = Signal.foldp (::) [] test

main = (\x -> flow down (info :: shortcuts ++ (List.map show x))) <~ entered