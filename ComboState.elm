module ComboState where

import Tree exposing (..)
import Keys exposing (..)
import Time exposing (..)
import Signal exposing ((<~))
import Signal.Time exposing (settledAfter)

import String

import Graphics.Element exposing (..)

type alias ZipperTree a = (Tree a, List (PartialFunc T (Tree a)))

root : ZipperTree a -> ZipperTree a
root (tree, xs) = case xs of
  []    -> (tree, [])
  f::fs -> root (Node f, fs)

{-| Goes to the root if there is no transition for the provided input -}
goTo : T -> ZipperTree a -> ZipperTree a
goTo t (tree, fs) = case tree of 
  Leaf _ -> root (tree, fs)
  Node f -> case eval f t of
    Nothing    -> root (tree, fs)
    Just tree' -> (tree', f::fs)

treeState : Tree a -> Signal (ZipperTree a)
treeState t = Signal.foldp goTo (t, []) withExpires

fromTree : Tree a -> a -> Signal a
fromTree t default = let
    fn (tree, _) = case tree of
      Leaf x -> Just x
      _      -> Nothing
  in Signal.filterMap fn default <| treeState t

expireDelay : Time
expireDelay = second

withExpires : Signal T
withExpires = let
    expires = delay expireDelay <| Signal.map (always Expire) keys
  in Signal.merge (Signal.map Event keys) (settledAfter expireDelay expires)

test = fromTree testTree 0

main : Signal Element
main = (\x -> flow down (List.map draw x)) <~ (Signal.foldp (::) [] <| treeState testTree)

draw (tree, fs) = let
    past = String.concat <| List.map (always "o-") fs 
    now = case tree of
      Leaf x -> toString x
      _      -> "o"
  in show <| String.append past now

