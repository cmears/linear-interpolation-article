{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Data.Monoid
import Data.List
import qualified Data.Map as M
import Data.Map (Map, lookupGE, lookupLE)
import Criterion.Main
import Template
import Tables

import Language.Haskell.TH

-- Compute the linear interpolation of "x" between the points (a,av)
-- and (b,bv).
interpolate (a,av) (b,bv) x = av + (x-a)*(bv-av)/(b-a)

-- Ordinary linear traversal of the table.
linearLookup :: [ (Double,Double) ] -> Double -> Double
linearLookup [] x = error "linearLookup: empty table"
linearLookup ((a,av):rest) x | x <= a = av
                             | otherwise = loop ((a,av):rest)
  where -- At the end of the table, so use the last value.
        loop [(a,av)] = av
        loop ((a,av):(b,bv):rest)
             -- We've found the target range; do the interpolation.
             | x <= b = interpolate (a,av) (b,bv) x
             -- Otherwise, keep looking.
             | otherwise = loop ((b,bv):rest)

-- Use the First monoid to "hide" the recursion.
monoidalLookup :: [ (Double, Double) ] -> Double -> Double
monoidalLookup table x =
    case (firstJust . map f . tails) table of
      Just (ap,bp) -> interpolate ap bp x
      Nothing -> if x < fst (head table)
                 then snd (head table)
                 else snd (last table)
  where f ((a,av):(b,bv):_) | a <= x && x <= b = Just ((a,av),(b,bv))
        f _ = Nothing

-- Get the first "Just" value from a list.
firstJust :: [Maybe a] -> Maybe a
firstJust = getFirst . mconcat . map First

-- Find the points to interpolate between using a map.
mapLookup :: Map Double Double -> Double -> Double
mapLookup m x =
    case (lookupLE x m, lookupGE x m) of
      (Just (a,av), Just (b,bv)) ->
        if a == b
        then av
        else interpolate (a,av) (b,bv) x
      (Nothing, Just (b,bv)) -> bv
      (Just (a,av), Nothing) -> av
      _ -> error "mapLookup"

staticLookup x = $(unrollLookupTree sinTree) x
staticLinearLookup x = $(unrollLinearLookup sinTable) x


benchmarks = [ bgroup "linear" [ bench "front" (whnf (linearLookup sinTable) targetFront)
                               , bench "middle" (whnf (linearLookup sinTable) targetMiddle)
                               , bench "back" (whnf (linearLookup sinTable) targetBack) ]
             , bgroup "map" [ bench "front" (whnf (mapLookup sinMap) targetFront)
                            , bench "middle" (whnf (mapLookup sinMap) targetMiddle)
                            , bench "back" (whnf (mapLookup sinMap) targetBack) ]
             , bgroup "monoidal" [ bench "front" (whnf (monoidalLookup sinTable) targetFront)
                                 , bench "middle" (whnf (monoidalLookup sinTable) targetMiddle)
                                 , bench "back" (whnf (monoidalLookup sinTable) targetBack) ]
             , bgroup "tree" [ bench "front" (whnf (treeLookup sinTree) targetFront)
                             , bench "middle" (whnf (treeLookup sinTree) targetMiddle)
                             , bench "back" (whnf (treeLookup sinTree) targetBack) ]
             , bgroup "static-tree" [ bench "front" (whnf staticLookup targetFront)
                                    , bench "middle" (whnf staticLookup targetMiddle)
                                    , bench "back" (whnf staticLookup targetBack) ]
             , bgroup "static-linear" [ bench "front" (whnf staticLinearLookup targetFront)
                                      , bench "middle" (whnf staticLinearLookup targetMiddle)
                                      , bench "back" (whnf staticLinearLookup targetBack) ]
             ]

main = defaultMain benchmarks

-- Check that all methods give the same answers.
findError = (find (\x -> not (ok x)) [0.0,0.001..11.0])
ok x =
    let outputs = [ linearLookup sinTable x
                  , mapLookup sinMap x
                  , monoidalLookup sinTable x
                  , treeLookup sinTree x
                  , staticLookup x
                  , staticLinearLookup x
                  ]
    in case all (uncurry nearEq) (pairs outputs) of
         True -> True
         False -> error $ show (x,outputs)
x `nearEq` y = abs (x-y) < 1.0e-15
pairs list = [ (x,y) | (x:xs) <- tails list, y <- xs ]

littleLookup x =
  case compare x 2 of
    EQ -> 4
    LT -> case compare x 1 of
            EQ -> 1
            LT -> 1
            GT -> 1 + (((x - 1) * (4 - 1)) / (2 - 1))
    GT -> case compare x 3 of
            EQ -> 9
            LT -> 4 + (((x - 2) * (9 - 4)) / (3 - 2))
            GT -> 9
