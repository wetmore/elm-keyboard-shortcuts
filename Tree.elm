module Tree where

import Keys exposing (..)
import List

type alias Combo = List Key

type T = Expire | Event Key

{-| A partial function may map some elements of its domain to Nothing.
The associated Int tracks the fanout of the function, i.e. the number of
elements in the domain which do not map to Nothing. We use partial functions
for the transition function for a Tree, which explains why we call it fanout.
-}
type PartialFunc a b = PF (a -> Maybe b) Int

type Tree a = Leaf a | Node (PartialFunc T (Tree a))

isNothing : Maybe a -> Bool
isNothing m = case m of
  Nothing -> True
  _       -> False

{-| Extend a partial function to handle a new element of the domain.
If that element already maps to something, its image will be overwritten.
-}
extend : PartialFunc a b -> a -> b -> PartialFunc a b
extend (PF f n) x y = let
    fanout = if isNothing <| f x then n + 1 else n
  in PF (\t -> if t == x then Just y else f t) fanout

emptyFunc : PartialFunc a b
emptyFunc = PF (always Nothing) 0

singleton : a -> b -> PartialFunc a b
singleton = extend emptyFunc

eval : PartialFunc a b -> a -> Maybe b
eval (PF f _) x = f x

fanout : PartialFunc a b -> Int
fanout (PF _ n) = n

emptyTree : Tree a
emptyTree = Node emptyFunc

{-| Given a pair (`combo`, `action`) and a tree, add a path from the root
described by the sequence of keys in `combo`, ending at a leaf holding `action`.
If the tree already contains an action associated to that path, it is
overwritten.

If P and Q are two paths, where P is an initial segment of Q, it is important
to note the behaviour of adding P and Q to a tree: consider, for example, the
sequences "a b" and "a b c", associated to actions 1 and 2 respectively. To
access the action associated to "a b", there must be a sequence of "a", "b", and
then an Expire.
-}
addPath : (Combo, a) -> Tree a -> Tree a
addPath (xs, act) tree = case xs of
  [] -> case tree of
    -- note this will overwrite previous actions at this point if they exist
    Node delta -> case fanout delta of
      0 -> Leaf act
      _ -> Node <| extend delta Expire <| Leaf act
    Leaf _     -> Leaf act
  k::ks -> let
      newNode = addPath (ks, act) emptyTree
    in case tree of 
      Leaf act'  -> Node <|
        extend (singleton Expire (Leaf act')) (Event k) newNode
      Node delta -> case eval delta (Event k) of
        Nothing -> Node <| extend delta (Event k) newNode
        Just t  -> Node <| extend delta (Event k) (addPath (ks, act) t)

treeFromCombos : List (Combo, a) -> Tree a
treeFromCombos = List.foldl addPath emptyTree

value : Tree a -> Maybe a
value tree = case tree of
  Node _ -> Nothing
  Leaf x -> Just x

-- for debugging purposes
followPath : List T -> Tree a -> Maybe a
followPath xs tree = case xs of
  []     -> value tree
  k::ks -> case tree of
    Leaf _ -> Nothing
    Node delta -> case eval delta k of
      Nothing -> Nothing
      Just tree' -> followPath ks tree'


testTree = let
    t1 = addPath ([Press 'a', Press 'b'], 1) emptyTree
    t2 = addPath ([Press 'a', Press 'b', Press 'c'], 2) t1
    t3 = addPath ([Press 'b', Press 'c'], 3) t2
    t4 = addPath ([Press 'a', Press 'c'], 4) t3
  in t4
{--
main : Element
main = show <| followPath [Event <| Press 'a', Event <| Press 'b', Event <| Press 'c'] testTree
--}