module Keys where

import Keyboard exposing (KeyCode)
import Signal exposing ((<~), (~), map2)
import Set exposing (Set)
import List
import Graphics.Element exposing (..)
import Signal.Extra exposing (switchWhen)

import String
import Char

type RawKeys = KeysDown (Set KeyCode, Set KeyCode) | Presses KeyCode
type Meta = Shift | Ctrl | Alt
type Key  = Press Char | Esc | Ret | Chord (List Meta) Key

metas = [16, 17, 18]

isMeta : KeyCode -> Bool
isMeta x = x `List.member` metas

toMeta : KeyCode -> Meta
toMeta x = case x of
  16 -> Shift
  17 -> Ctrl
  18 -> Alt

emptyPair = (Set.empty, Set.empty)

keys' : Signal (Set KeyCode, Set KeyCode)
keys' = let
    fn (w,x) (_,z) = (w, x `Set.diff` z)
  in Signal.foldp fn emptyPair <| Set.partition isMeta <~ Keyboard.keysDown

rawKeys : Signal RawKeys
rawKeys = Signal.map KeysDown <| Signal.filter ((/=) emptyPair) emptyPair keys'

metaDown : RawKeys -> Bool
metaDown raw = case raw of
  KeysDown (set, _) -> List.length (Set.toList set) > 0
  _                 -> False

pressFromCode : KeyCode -> Key
pressFromCode k = Press <| Char.fromCode k

toKey : RawKeys -> Maybe Key
toKey raw = case raw of
  Presses k -> Just <| pressFromCode k
  KeysDown (set1, set2) -> let
      metas = Set.toList set1
      keys  = Set.toList set2
    in case keys of
      [k] -> Just <| Chord (List.map toMeta metas) <| pressFromCode k
      _   -> Nothing

s : Signal RawKeys
s = switchWhen (Signal.map metaDown rawKeys) rawKeys (Signal.map Presses Keyboard.presses)

keys : Signal Key
keys = Signal.filterMap toKey Esc s

main : Signal Element
main = (\x -> flow down (List.map show x)) <~ (Signal.foldp (::) [] keys)

draw list = let
    letters = List.map (\(_,x) -> List.map Char.fromCode <| Set.toList x) list
    string = String.concat <| List.map String.fromList letters
  in show (String.reverse string) 