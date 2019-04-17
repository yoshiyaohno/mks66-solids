module Solids where

import Line
import Screen
import Transform
import qualified Data.List as L

newtype Triangle a = Triangle (Vect a, Vect a, Vect a) deriving (Show, Eq)

piStep :: Floating a => a
piStep = pi/11

drawTriangle :: Color -> Triangle Double -> Screen -> Screen
drawTriangle c t = draw
    [((round $ getX px, round $ getY px), c)
        | px <- scanTriangle t]
    
lh :: (Eq a, Enum a, Fractional a) =>
    (Vect a -> a) -> Vect a -> Vect a -> [Vect a]
lh = lineHelper

scanTriangle :: (Enum a, Fractional a, Ord a) => Triangle a -> [Vect a]
scanTriangle (Triangle (a, b, c)) = let
    [top, mid, bot] = L.sortOn getY [a, b, c]
    e1 = lh getY top bot
    e2 = lh getY mid bot ++ tail (lh getY top mid)
    es = if (getX $ e2!!1) < (getX $ e1!!1) then zip e2 e1 else zip e1 e2
    in concat $ map (uncurry $ lh getX) es

    --in concat $ zipWith (lh getX) e1 e2
--  es = if (getX mid) < (getX top) then zip e2 e1 else zip e1 e2
--  in concat $ map (uncurry $ jelp getX) es
        
--  lh (getY top, getY bot) (getX top, getX bot)

toEdges :: Triangle a -> [Line a]
toEdges (Triangle (a, b, c)) = [Line a b, Line b c, Line a c]

drawTriangles :: Color -> [Triangle Double] -> Screen -> Screen
drawTriangles c = foldr (.) id . zipWith drawTriangle
    (cycle [red, grn, blu, wht]) . bfCull

--drawTriangles :: Color -> [Triangle Double] -> Screen -> Screen
--drawTriangles c =
--    foldr (.) id . map (drawLine c . fmap round)
--        . concat . map toEdges . bfCull

bfCull :: (Num a, Ord a) => [Triangle a] -> [Triangle a]
bfCull = filter ((>0) . getZ . normal)

normal :: (Num a) => Triangle a -> Vect a
normal (Triangle (a, b, c)) = crossProd ((-) <$> c <*> a) ((-) <$> b <*> a)

trTriangle :: (Num a) => Transform a -> Triangle a -> Triangle a
trTriangle t (Triangle (a, b, c)) = Triangle (pmult t a, pmult t b, pmult t c)

sphere :: (Floating a, Enum a) => a -> a -> a -> a -> [Triangle a]
sphere cx cy cz r = concat $ zipWith stitchLines (rotate 1 arcs) arcs
    where arcs = [[Vect (cx + r * cos thet) (cy + r * sin thet * cos phi)
                 (cz + r * sin thet * sin phi) 1
                 | thet <- [0, piStep .. pi]] | phi <- [0, 2*piStep .. 2*pi]]

torus :: (Floating a, Enum a) => a -> a -> a -> a -> a -> [Triangle a]
torus cx cy cz r0 r1 = concat $ zipWith stitchLines arcs (rotate 1 arcs)
    where arcs = [[Vect (cx + r0 * cos thet * cos phi + r1 * cos phi)
                  (cy + r0 * sin thet)
                  (cz - sin phi * (r0 * cos thet + r1)) 1
                  | thet <- [0, 2*piStep .. 2*pi]]
                  | phi <- [0, 2*piStep .. 2*pi]]

box :: (Floating a, Enum a) => a -> a -> a -> a -> a -> a -> [Triangle a]
box cx cy cz w h d = let
    [p000, p001, p010, p011, p100, p101, p110, p111] = 
        [Vect (cx + qx * w) (cy - qy * h) (cz - qz * d) 1
            | qx <- [0,1], qy <- [0,1], qz <- [0,1]]
                in stitch4 p000 p010 p110 p100
                   ++ stitch4 p100 p110 p111 p101
                   ++ stitch4 p111 p011 p001 p101
                   ++ stitch4 p110 p010 p011 p111
                   ++ stitch4 p011 p010 p000 p001
                   ++ stitch4 p000 p100 p101 p001
    -- y a h o o

stitchLines :: [Vect a] -> [Vect a] -> [Triangle a]
stitchLines [] _   = []
stitchLines _ []   = []
stitchLines _ [b0] = []
stitchLines [a0] _ = []
stitchLines (a0:a1:as) (b0:b1:bs) =
    (stitch4 a1 a0 b0 b1) ++ (stitchLines (a1:as) (b1:bs))

-- these four points go clockwise to face the normal into you
stitch4 :: Vect t -> Vect t -> Vect t -> Vect t -> [Triangle t]
stitch4 a b c d = [Triangle (a, b, c), Triangle (a, c, d)]

