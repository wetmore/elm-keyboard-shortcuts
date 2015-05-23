module Parser (parse, Outcome) where

-- TODO: add key-wildcards to combos

import Result exposing (andThen)
import Keys exposing (Key(..), Modifier(..), Modifiers)
import ComboState exposing (..)

import Dict exposing (Dict)
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
      xs -> Ok <| mergeModifiers xs y 
  in pair `andThen` makeKey

mergeModifiers : Modifiers -> Key -> Key
mergeModifiers ms key = case key of
  (Chord ms' k) -> let
      merge = List.foldr (\x ys -> if x `List.member` ys then ys else x :: ys)
    in Chord (merge ms ms') k
  k -> case ms of
    [] -> k
    _  -> Chord ms k  

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
      Just downShifted -> Chord [Shift] (Press downShifted)
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