module Parser (parse, parsePairs) where

import Dict exposing (Dict)
import Keys exposing (Key(..), Modifier(..), Modifiers)
import List exposing (head, tail, reverse)
import Maybe
import Result exposing (andThen, formatError)
import String


type alias Shortcut = List Key
type alias Outcome a = Result String a

parse : String -> Outcome Shortcut
parse s = let
    f err = "Error parsing " ++ s ++ ": " ++ err
  in formatError f << sequence <| List.map parseAtoms <| String.split " " s

zip = List.map2 (,)

parsePairs : List (String, a) -> Outcome (List (Shortcut, a))
parsePairs ps = let
    (strings, acts) = List.unzip ps
    parsed = sequence <| List.map parse strings
  in Result.map (flip zip acts) parsed

-- Returns an atom for a sequence, so some Key
parseAtoms : String -> Outcome Key
parseAtoms s = case s of
  "" -> Err "Shortcut string must be non-empty."
  _  -> case cotail <| String.split "+" s of
    Nothing -> Err "Somehow String.split returned an empty list."
    Just (mods, key) -> let
        parsedMods = sequence <| List.map parseModifier mods
        parsedKey  = parseKey key
        pair = Result.map2 (,) parsedMods parsedKey
        makeKey (x,y) = case x of
          [] -> Ok <| y
          xs -> Ok <| mergeModifiers xs y 
      in pair `andThen` makeKey

mergeModifiers : Modifiers -> Key -> Key
mergeModifiers ms key = case key of
  (Combo ms' k) -> let
      merge = List.foldr (\x ys -> if x `List.member` ys then ys else x :: ys)
    in Combo (merge ms ms') k
  k -> case ms of
    [] -> k
    _  -> Combo ms k  

-- Outcome forms a monad
return : a -> Outcome a
return = Ok

sequence : List (Outcome a) -> Outcome (List a)
sequence ms = let
    k m m' = m `andThen` \x -> m' `andThen` \xs -> return (x::xs)
  in List.foldr k (return []) ms

parseModifier : String -> Outcome Modifier
parseModifier s = case s of
  "shift"   -> Ok Shift
  "ctrl"    -> Ok Ctrl
  "alt"     -> Ok Alt
  "meta"    -> Ok Meta
  "command" -> Ok Meta
  "windows" -> Ok Meta
  x         -> Err <| "Unknown modifier: " ++ x

parseKey : String -> Outcome Key
parseKey s = case s of
  "esc"    -> Ok Esc
  "return" -> Ok Return
  "enter"  -> Ok Return
  "tab"    -> Ok Tab
  "caps"   -> Ok CapsLock
  "up"     -> Ok Up
  "down"   -> Ok Down
  "left"   -> Ok Left
  "right"  -> Ok Right
  "plus"   -> Ok <| Press '+'
  x        -> case String.toList x of
    [c] -> Ok <| case Dict.get c shiftMap of
      Just downShifted -> Combo [Shift] (Press downShifted)
      Nothing          -> Press c
    _   -> Err <| "Unknown non-modifier key: " ++ x

cotail : List a -> Maybe (List a, a)
cotail list = let
    flipped    = reverse list
    addHead tl = Maybe.map ((,) tl) <| head flipped
  in tail flipped `Maybe.andThen` addHead

shiftMap : Dict Char Char
shiftMap = Dict.fromList <|
  [ ('~', '`')
  , ('!', '1')
  , ('@', '2')
  , ('#', '3')
  , ('$', '4')
  , ('%', '5')
  , ('^', '6')
  , ('&', '7')
  , ('*', '8')
  , ('(', '9')
  , (')', '0')
  , ('_', '-')
  , ('+', '=')
  , (':', ';')
  , ('"', '\'')
  , ('<', ',')
  , ('>', '.')
  , ('?', '/')
  , ('|', '\\')
  ]
