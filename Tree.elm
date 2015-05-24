module Tree where
{-| In order to find keyboard combinations within the inputs coming from
`Keys.keys`, we build a tree. The edges in this tree have labels. Each leaf
holds an action. Each path from the root to a leaf in the tree corresponds to a
combination which is meant to be matched. Whenever that combination is entered,
the action at the respective leaf is sent out on a Signal.

# Definition

The trees we use have the following type:

@docs Tree

In order to understand this type, one needs to see the definition of a
PartialFunction as well:

@docs PartialFunc

As we can see from the type `Tree`, the edges of the tree are labelled with
elements of the following type:

@docs T

# Useful operations

The main functionality exported by this module is `treeFromCombos`, which leans
heavily on `addPath`.

@docs treeFromCombos, addPath
-}


import Keys exposing (..)
import List

type alias Combo = List Key

{-| When handling keypresses, we also need to handle what happens if the user
stops entering a combination. After a preset timeout duration (found in
`ComboState`), if there is no input from the user, the system will stop waiting
for the user to complete whatever combo they are entering.

This allows us to handle combinations which are initial segments of each other.
For example, if we want "a b" (the combination consisting of pressing "a", then
pressing "b") to trigger an action A, and "a b c" to trigger B, then after the
user has entered "a b" we aren't sure if the use will continue to press "c" to
trigger B, or if they are finished and wanted to trigger A. So we trigger B only
if the user stops entering keys for some time. ***maybe any unrecognized input should go there**

Hence the stream of things we must handle consists of keypresses as well as an
Expire event. This type encompasses the things we must handle.
-}
type T = Expire | Event Key

{-| A partial function may map some elements of its domain to Nothing.
The associated Int tracks the fanout of the function, i.e. the number of
elements in the domain which do not map to Nothing. We use partial functions
for the transition function for a Tree, which explains why we call it fanout.
-}
type PartialFunc a b = PF (a -> Maybe b) Int

{-| Fairly run-of-the-mill type for a tree, with the catch that the fanout at
each node is not constant. In a way, it is more like a labelled transition
system; We use a partial function at each node to represent the branches from
that node.
-}
type Tree a = Leaf a | Node (PartialFunc T (Tree a))

{-| Extend a partial function to handle a new element of the domain.
If that element already maps to something, its image will be overwritten.
-}
extend : PartialFunc a b -> a -> b -> PartialFunc a b
extend (PF f n) x y = let
    fanout = case f x of
      Nothing -> n + 1
      _       -> n  -- in the case of an overwrite, fanout is unchanged
  in PF (\t -> if t == x then Just y else f t) fanout

{-| Partial function which maps every element of its domain to Nothing. -}
emptyFunc : PartialFunc a b
emptyFunc = PF (always Nothing) 0

{-| Convenience function to create a partial function which maps exactly one
element to something.
-}
singleton : a -> b -> PartialFunc a b
singleton = extend emptyFunc

{-| Evaluate a partial function at some element of its domain. -}
eval : PartialFunc a b -> a -> Maybe b
eval (PF f _) x = f x

{-| Returns the number of inputs on which a partial function is defined. -}
fanout : PartialFunc a b -> Int
fanout (PF _ n) = n

{-| Tree with no branching or leaves. -}
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

{-| Given a list of combinations and associated actions, builds a tree, whose
paths to the leaves are precisely the combinations from the inputs.
-}
treeFromCombos : List (Combo, a) -> Tree a
treeFromCombos = List.foldl addPath emptyTree

testTree = let
    t1 = addPath ([Press 'a', Press 'b'], 1) emptyTree
    t2 = addPath ([Press 'a', Press 'b', Press 'c'], 2) t1
    t3 = addPath ([Press 'b', Press 'c'], 3) t2
    t4 = addPath ([Press 'a', Press 'c'], 4) t3
  in t4