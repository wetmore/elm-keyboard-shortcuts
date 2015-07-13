module KeyCombos where

import Keys exposing (..)
import ComboState exposing (..)
import Parser exposing (..)
import Tree exposing (..)

import Graphics.Element exposing (..)
import Signal exposing ((<~))
import Result

type alias Combo = List Key

listenFor' : a -> List (Combo, a) -> Signal a
listenFor' def pairs = fromTree (treeFromCombos pairs) def

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