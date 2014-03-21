---
title: fmap versus recursion
---


## fmap vs. recursion
\
\
A):
``` haskell
mapLabel ::  Eq a => (a -> b) -> Tree a -> Tree b
mapLabel f (Node label []) = Node (f label) []
mapLabel f (Node label subTrees) = (Node (f label) (subLeaves subTrees))
	where subLeaves [] = []
        subLeaves (t:ts)
            | leaf t    =  Node (f label) [] : subLeaves ts
			| otherwise =  Node (f label) (subLeaves subTrees) : subLeaves ts
                                      
```

B):
``` haskell
fmap f (Node x ts) = Node (f x) (map (fmap f) ts)
```
\

The idea was to create a function that would walk through a tree-like data
structure and do some stuff to it.
First I spent 2 evenings designing (or understanding rather) a recursive
patter-matching function. The evening I was able to compile it *and* produce
the expected result – I felt quite content with myself…

C):
``` haskell
mapLabel ::  Eq a => (a -> b) -> Tree a -> Tree b
mapLabel f (Node label []) = Node (f label) []
mapLabel f (Node label subTrees) = (Node (f label) (subLeaves subTrees))
    where subLeaves [] = []
        subLeaves (t:ts)
            | leaf t    =  Node (f label) [] : subLeaves ts
			| otherwise =  Node (f label) (subLeaves subTrees) : subLeaves ts
                where label    = rootLabel t
```
Just look at the awe-inspiring complexity of it!.. How is it even possible that
my humble mind was able to grasp that? Then I revisited some of the related
source-code inside Data.Tree module:

D):
``` haskell
instance Functor Tree where
    fmap f (Node x ts) = Node (f x) (map (fmap f) ts)
```
… So much for the awesome complexity. It turned out the the solution B) is functionally
equivalent to A) and has additional benefit of being so much more concise and
easy to re-factor into, say, this:

``` haskell
fmap f (Node x ts) = Node (f ts) (map (fmap f) ts)
```

(This function will recursively operate on ts (pattern-matched to subForest members), rather than on the x (rootLabel), as in the function above, without loosing readability – ain’t that just peachy?):

Further example of the same principle:

``` haskell
-- A and B are roughly equivalent:
-- A:
gamma :: Tree String -> Tree String -> Tree Int
gamma t k = Node a forest 
           where a      = (\[x, y] -> x*y) $ map read $ map rootLabel [t,k]
                 forest = (multIntForests (subForest t) (subForest k)) 

multIntTrees :: Tree String -> Tree String -> Tree Int
multIntTrees = gamma
 

multIntForests :: [Tree String] -> [Tree String]-> [Tree Int]
multIntForests [] _ = []
multIntForests _ [] = []
multIntForests (t:ts) (k:ks) = gamma t k : multIntForests ts ks 


-- B:
delta f (Node a ts) (Node b ks) = (Node (f a b) (zipWith (delta f) ts ks))
```

… And more examples:


``` haskell
-- A and B are equivalent:
-- A:
-- Aquire a tree with the number of sub-nodes as a value of the rootLabel:
members :: Tree String -> Tree String
members (Node x [])  = Node {rootLabel = "0", subForest = []}
members (Node x ts0) = Node {rootLabel = (show $ length ts0), subForest = subNodes ts0}
     where subNodes []     = []
           subNodes [t]    = members t : []
           subNodes (t:ts) = (members t): subNodes ts

-- B:
members :: Tree String -> Tree String
members = fmap length
```
