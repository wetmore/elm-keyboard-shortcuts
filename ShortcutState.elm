module ShortcutState where

import Keys exposing (keys)
import Time exposing (..)
import Tree exposing (T(..), Tree(..), PartialFunc, eval)
import Signal
import Signal.Time exposing (settledAfter)


type alias ZipperTree a = (Tree a, List (PartialFunc T (Tree a)))

root : ZipperTree a -> ZipperTree a
root (tree, xs) = case xs of
  []    -> (tree, [])
  f::fs -> root (Node f, fs)

{-| Goes to the root if there is no transition for the provided input -}
goTo : T -> ZipperTree a -> ZipperTree a
goTo t (tree, fs) = case tree of 
  Leaf _ -> goTo t <| root (tree, fs)
  Node f -> case eval f t of
    Nothing -> case fs of 
      [] -> (tree, fs) -- we are already at the root
      _  -> goTo t <| root (tree, fs)
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
expireDelay = second / 2

withExpires : Signal T
withExpires = let
    expires = delay expireDelay <| Signal.map (always Expire) keys
  in Signal.merge (Signal.map Event keys) (settledAfter expireDelay expires)
