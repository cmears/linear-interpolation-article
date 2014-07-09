{-# LANGUAGE TemplateHaskell #-}

module Template where

import Language.Haskell.TH

unrollLookupTree :: SplitTree -> ExpQ
unrollLookupTree t = do
  x <- newName "x"
  body <- loop (varE x) Nothing Nothing t
  return $ LamE [VarP x] body

loop :: ExpQ -> Maybe (Double,Double) -> Maybe (Double,Double) -> SplitTree -> ExpQ
loop x lb ub Leaf =
    case (lb,ub) of
      (Nothing, Nothing) -> error "can't interpolate on empty tree"
      (Just (l,lv), Nothing) -> ld lv
      (Nothing, Just (u,uv)) -> ld uv
      (Just (l,lv), Just (u,uv)) ->
        [| $(ld lv) + ($x - $(ld l)) * ($(ld uv) - $(ld lv))/($(ld u) - $(ld l)) |]
loop x lb ub (Node (n,nv) left right) =
    [| case compare $x $(ld n) of
         EQ -> $(ld nv)
         LT -> $(loop x lb (Just (n,nv)) left)
         GT -> $(loop x (Just (n,nv)) ub right)
    |]

-- Lift a Double to an ExpQ
ld :: Double -> ExpQ
ld = litE . rationalL . realToFrac

unrollLinearLookup [] = error "linearLookup: empty table"
unrollLinearLookup table = do
  x <- newName "x"
  body <- linearLoop (varE x) Nothing table
  return $ LamE [VarP x] body

linearLoop :: ExpQ -> Maybe (Double,Double) -> [(Double,Double)] -> ExpQ
linearLoop x Nothing [] = error "absolutely cannot happen"
linearLoop x (Just (a,av)) [] = ld av
linearLoop x previous ((b,bv):rest) =
    let val = case previous of
                Just (a,av) -> [| $(ld av) + ($x - $(ld a)) * ($(ld bv) - $(ld av))/($(ld b) - $(ld a)) |]
                Nothing -> ld bv
    in [| case compare $x $(ld b) of
            GT -> $(linearLoop x (Just (b,bv)) rest)
            _ -> $val
       |]

data SplitTree = Leaf
               | Node (Double, Double) SplitTree SplitTree
  deriving (Show)

makeTree :: [(Double,Double)] -> SplitTree
makeTree [] = Leaf
makeTree [p] = Node p Leaf Leaf
makeTree ps = let n = length ps
                  m = n `div` 2
                  (left, (middle:right)) = splitAt m ps
              in Node middle (makeTree left) (makeTree right)

treeLookup :: SplitTree -> Double -> Double
treeLookup t x = loop Nothing Nothing t
  where loop lb ub Leaf =
            case (lb,ub) of
              (Nothing, Nothing) -> error "can't interpolate on empty tree"
              (Just (l,lv), Nothing) -> lv
              (Nothing, Just (u,uv)) -> uv
              (Just (l,lv), Just (u,uv)) -> lv + (x-l)*(uv-lv)/(u-l)
        loop lb ub (Node (n,nv) left right) =
            case compare x n of
              EQ -> nv
              LT -> loop lb (Just (n,nv)) left
              GT -> loop (Just (n,nv)) ub right
