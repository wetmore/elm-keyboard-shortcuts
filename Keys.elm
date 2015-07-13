module Keys where

import Char
import Dict exposing (Dict)
import Keyboard exposing (KeyCode)
import List
import Set exposing (Set)
import Signal exposing ((<~), (~))
import Signal.Extra exposing (switchWhen)


type Modifier = Shift | Ctrl | Alt | Meta
type alias Modifiers = List Modifier
type Key  = Press Char | Chord Modifiers Key
          | Esc | Return | Tab | CapsLock | Up | Down | Left | Right

type RawKeys = KeysDown (Set KeyCode, Set KeyCode) | Presses KeyCode

specialKeys = Dict.fromList <|
  [ (9, Tab)
  , (13, Return)
  , (20, CapsLock)
  , (27, Esc)
  , (37, Left)
  , (38, Up)
  , (39, Right)
  , (40, Down)
  ]
  -- not handled: backspace, del, pageup, pagedown, end, home, insert

modifiers = Dict.fromList <|
  [ (16, Shift)
  , (17, Ctrl)
  , (18, Alt)
  , (91, Meta)
  , (93, Meta)
  , (224, Meta)
  ]

kcs : Dict KeyCode Char
kcs = Dict.fromList <|
  [ (106, '*')
  , (107, '+')
  , (109, '-')
  , (110, '.')
  , (111, '/')
  , (186, ';')
  , (187, '=')
  , (188, ',')
  , (189, '-')
  , (190, '.')
  , (191, '/')
  , (192, '`')
  , (219, '[')
  , (220, '\\')
  , (221, ']')
  , (222, '\'')
  ]

keyFromCode : KeyCode -> Key
keyFromCode k = let
    result = Dict.get k specialKeys
    c = Char.fromCode k
  in case result of
    Just key -> key
    Nothing  -> case Char.isUpper c of
      True  -> Press <| Char.toLower c
      False -> Press c

-- `Keyboard.keysDown` gives different KeyCodes than `Keyboard.presses`. Some
-- outputs give weird results when converted to a character. This function
-- normalizes some of the weird results so that the Press we associate with
-- a chord makes sense.
pressFromKeyDown : KeyCode -> Key
pressFromKeyDown k = let
    result = Dict.get k kcs
  in case result of
    Just c  -> Press c
    Nothing -> keyFromCode k

isModifier : KeyCode -> Bool
isModifier x = x `Dict.member` modifiers

isSpecial : KeyCode -> Bool
isSpecial x = x `Dict.member` specialKeys

toModifier : KeyCode -> Maybe Modifier
toModifier x = Dict.get x modifiers

mapAndCollapse : (a -> Maybe b) -> List a -> List b
mapAndCollapse f = let
    fn a acc = case f a of
      Just b  -> b :: acc
      Nothing -> acc
  in List.foldr fn []

emptyPair = (Set.empty, Set.empty)

keys' : Signal (Set KeyCode, Set KeyCode)
keys' = let
    fn (w,x) (_,z) = (w, x `Set.diff` z)
  in Signal.foldp fn emptyPair <| Set.partition isModifier <~ Keyboard.keysDown

rawKeys : Signal RawKeys
rawKeys = Signal.map KeysDown <| Signal.filter ((/=) emptyPair) emptyPair keys'

useKeysDown : RawKeys -> Bool
useKeysDown raw = case raw of
  KeysDown (set1, set2)  -> if List.length (Set.toList set1) > 0
                            then True
                            else case (Set.toList set2) of
                              [k] -> isSpecial k
                              _   -> False
  _ -> False

toKey : RawKeys -> Maybe Key
toKey raw = case raw of
  Presses k -> Just <| keyFromCode k
  KeysDown (set1, set2) -> let
      mods = Set.toList set1
      keys = Set.toList set2
    in case keys of
      [k] -> Just <| case mods of 
        [] -> pressFromKeyDown k
        _  -> Chord (mapAndCollapse toModifier mods) <| pressFromKeyDown k
      _   -> Nothing

s : Signal RawKeys
s = switchWhen (Signal.map useKeysDown rawKeys) rawKeys (Signal.map Presses Keyboard.presses)

keys : Signal Key
keys = Signal.filterMap toKey Esc s