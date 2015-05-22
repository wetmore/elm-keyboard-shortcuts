module Parser (parse) where

import Result exposing (andThen)
import Keys exposing (Key(..), Modifier(..))
import ComboState exposing (..)

import Maybe
import List exposing (head, tail, reverse)
import String

type alias Outcome a = Result String a

parse : String -> Outcome Combo
parse s = sequence <| List.map parseAtoms <| String.split " " s

-- Returns an atom for a sequence, so some Key
parseAtoms : String -> Outcome Key
parseAtoms s = case cotail <| String.split "+" s of
  Nothing -> Err "Somehow String.split returned an empty list."
  Just (mods, key) -> let
    parsedMods = sequence <| List.map parseModifier mods
    parsedKey  = parseKey key
    pair = Result.map2 (,) parsedMods parsedKey
    makeKey (x,y) = case x of
      [] -> Ok <| y
      xs -> Ok <| Chord xs y 
  in pair `andThen` makeKey

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
  x        -> case String.toList x of
    [c] -> Ok <| Press c
    _   -> Err <| "Unknown non-modifier key: " ++ x

cotail : List a -> Maybe (List a, a)
cotail list = let
    flipped    = reverse list
    addHead tl = Maybe.map ((,) tl) <| head flipped
  in tail flipped `Maybe.andThen` addHead