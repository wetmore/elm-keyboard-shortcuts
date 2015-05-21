module Keys where

import Keyboard exposing (KeyCode)
import Signal exposing ((<~), (~))
import Set exposing (Set)
import List
import Graphics.Element exposing (..)
import Signal.Extra exposing (switchWhen)


import Graphics.Input.Field as Field


import Dict exposing (Dict)
import String
import Char

type RawKeys = KeysDown (Set KeyCode, Set KeyCode) | Presses KeyCode
type Modifier = Shift | Ctrl | Alt | Meta
type alias Modifiers = List Modifier
type Key  = Press Char | Chord Modifiers Key
          | Esc | Return | Tab | CapsLock | Up | Down | Left | Right


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
  in case result of
    Just key -> key
    Nothing  -> Press <| Char.fromCode <| k


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

pressFromCode : KeyCode -> Key
pressFromCode k = Press <| Char.fromCode k

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

main : Signal Element
main = (\x -> flow down (field::(List.map show x))) <~ (Signal.foldp (::) [] keys)


content : Signal.Mailbox Field.Content
content =
  Signal.mailbox Field.noContent

field =  Field.field Field.defaultStyle (Signal.message content.address) "Type here!" Field.noContent


draw list = let
    letters = List.map (\(_,x) -> List.map Char.fromCode <| Set.toList x) list
    string = String.concat <| List.map String.fromList letters
  in show (String.reverse string) 