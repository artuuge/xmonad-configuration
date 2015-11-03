-- The current commit contains a module `Switch` which is meant to be used to impose a totality check on the functions.
-- The basic idea is described in the `total` library on hackage. While the trick is not really needed since the compiler
-- is able to issue a warning if a function is not total, it is nonetheless of interest to be able to express totality
-- in the type system itself.

-- Assume that we have: 

{-# LANGUAGE TemplateHaskell #-}  
{-# LANGUAGE DeriveGeneric #-}  
{-# LANGUAGE LambdaCase #-}  

import qualified GHC.Generics as GHC  
import Control.Lens  
import Data.Void  
import qualified Control.Category.Cartesian as CCC  


-- The key combinator is of the shape:
-- `on p f g x = (CCC.|||) f g (p Left x)` 
-- `CCC.|||` constructs a copruduct of `f` and `g` and applies it to `(p Left x)`.
-- If we ask GHCi to compute the type of `on`, we obtain:
-- `on :: ((a1 -> Either a1 b1) -> t -> Either a b) -> (a -> c) -> (b -> c) -> t -> c`
-- In what follows it is necessary to specialize `b1` to `Void`, so: 

on :: ((a1 -> Either a1 Void) -> t -> Either a b) -> (a -> c) -> (b -> c) -> t -> c
on p f g x = (CCC.|||) f g (p Left x) 

-- Consider a data type

data Color = Red | Green | Blue  
  deriving (Show, Read, Eq, Enum, GHC.Generic)

-- The first step to make things work is to organize an auxiliary type

data ColorF a b c = RedF a | GreenF b | BlueF c
  deriving (Show, Read, Eq, GHC.Generic)

-- Every constructor should have a single argument and the arguments should be distinct. 
-- One can perceive `Color` as `ColorF () () ()`.
-- To make it more precise, define an isomorphism:

fromColor :: Color -> ColorF () () ()
fromColor = \case
  Red -> RedF ()
  Green -> GreenF ()
  Blue -> BlueF ()

-- This kind of definition can be implemented, of course, with Template Haskell,
-- but one may keep it for the sake of experimenting.

-- Suppose we wish to write a function `weight :: Color -> Int` which returns `10` for `Red`, `20` for `Green`, and `30` for `Blue`.
-- Normally:

weight :: Color -> Int
weight = \case
  Red -> 10
  Green -> 20
  Blue -> 30

-- The other way to do it is as follows.
-- First, create prisms (just prisms, not classy prisms) for `ColorF a b c`:

makePrisms ''ColorF

-- After that, consider an auxiliary function:

switchColorF :: ColorF Void Void Void -> a
switchColorF = \case
  RedF x -> absurd x
  GreenF x -> absurd x
  BlueF x -> absurd x

-- We can now have another implementation `weight'` of `weight`:

weight' :: Color -> Int
weight' = go . fromColor where
  go = switchColorF
    & on _RedF (\() -> 10)
    & on _GreenF (\() -> 20)
    & on _BlueF (\() -> 30)

-- If we forget by accident one of the lines in enumerating the cases, the compiler is going to through a type error.
-- Similarly, branching on the same pattern more than once leads to a type error. 
-- Note, that the replacement mentioned of `b1` with `Void` was necessary to be able to detect this kind of overlaps. 

-- It is, perhaps, of interest to mention that one may organize a dual trick
-- to perform a check of the initialization completeness for record types.
-- Consider a data type (a rectangle described by width and length):

data Rct a = Rct { _len :: a, _wid :: a }
  deriving (Show, Read, Eq, GHC.Generic)

-- Consider an auxiliary type with lenses (not classy lenses):

data RctG a b = RctG { _lenG :: a, _widG :: b }
  deriving (Show, Read, Eq, GHC.Generic)

makeLenses '' RctG

-- Each field should correspond to a different parameter: `a`, `b`, ....
-- To link the two types, consider: 

toRct :: RctG a a -> Rct a
toRct (RctG x y) = Rct x y 

-- Instead of

myRct :: Rct Int
myRct = Rct { _len = 10, _wid = 20 }

-- one may write:

myRct' :: Rct Int
myRct' = toRct $ (RctG () ())
  & over lenG (\() -> 10)
  & over widG (\() -> 20)

-- Forgetting one of the fileds, or filling in the same field more than once, leads to a type error at compile time.
-- The role of the combinator `on` is taken by `over` (infix notation: (%~)).


-- ghc -O2 -j4 filename.hs -Wall

main :: IO ()
main = do
  putStrLn "Exhaustive pattern matching:"
  putStrLn . show $ fmap weight [Red, Green, Blue]
  putStrLn . show $ fmap weight' [Red, Green, Blue]
  putStrLn "Complete record initialization:"
  putStrLn . show $ myRct
  putStrLn . show $ myRct'
  return ()

