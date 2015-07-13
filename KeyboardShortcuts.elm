module KeyboardShortcuts where

import Keys exposing (Key)
import Parser exposing (parsePairs)
import ShortcutState exposing (fromTree)
import Tree exposing (treeFromShortcuts)

import Graphics.Element exposing (..)
import Signal exposing ((<~))
import Result

type alias Shortcut = List Key

listenFor' : a -> List (Shortcut, a) -> Signal a
listenFor' def pairs = fromTree (treeFromShortcuts pairs) def

listenForWithErr : a -> List (String, a) -> Result String (Signal a)
listenForWithErr def pairs = Result.map (listenFor' def) <| parsePairs pairs

listenFor : a -> List (String, a) -> Signal a
listenFor def pairs = case listenForWithErr def pairs of
  Err _ -> Signal.constant def
  Ok s  -> s

(:->) = (,)

test = let
    pairs = [ "a b"    :-> 1
            , "a b c"  :-> 2
            , "ctrl+c" :-> 3
            , "up up down down left right left right b a enter" :-> 9001
            ]
  in listenFor 0 pairs

main = (\x -> flow down (List.map show x)) <~ (Signal.foldp (::) [] test)