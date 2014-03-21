---
title: Haskell Gems
---

### Examples of beautiful haskell code

``` haskell

module Main where

import System.IO

main = readFile "input.txt" >>= print.length.lines

```
\


### [Lazy Pattern Matching]

``` haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

class Foo a where 
    foo :: a 

instance a ~ Int => Foo (a -> Int) where 
    foo k = k

instance (a ~ Int, b ~ Int) => Foo (a -> b -> Int) where 
    foo k l = k+l

instance (a ~ String) => Foo (a -> String) where 
    foo k = k

-- | usage: 
-- | foo 1 :: Int
-- | foo 1 2 :: Int
-- | foo "bar":: String
```

