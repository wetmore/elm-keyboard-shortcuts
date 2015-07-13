module KeyboardShortcuts where

import Result
import Util.Keys exposing (Key)
import Util.Parser exposing (parsePairs)
import Util.ShortcutState exposing (fromTree)
import Util.Tree exposing (treeFromShortcuts)


type alias Shortcut = List Key

listenFor' : a -> List (Shortcut, a) -> Signal a
listenFor' def pairs = fromTree (treeFromShortcuts pairs) def

listenForWithErr : a -> List (String, a) -> Result String (Signal a)
listenForWithErr def pairs = Result.map (listenFor' def) <| parsePairs pairs

listenFor : a -> List (String, a) -> Signal a
listenFor def pairs = case listenForWithErr def pairs of
  Err _ -> Signal.constant def
  Ok s  -> s
